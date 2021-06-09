import datetime
import collections as coll
import itertools as itr
import pytz
import sortedcontainers as sc
import bisect
import math

# Error Strings
TRIGGER_TYPE_ERR = "Report triggers/trigger must be list/int"
WINDOW_TRIGGER_MISMATCH_ERR = "Incompatible windows/triggers specifed: {} vs {}"
DATETIME_REQ_ERR = "Datetime objects required"
WINDOW_SORT_ERR = "Windows must be specified in small-to-large order"

# An illegal hashtag string used to represent all tokens
ALLTOKENS = u'ALL#TOKENS'


# Define a coroutine decorator that primes functions
# http://www.dabeaz.com/coroutines/Coroutines.pdf
## not in python3
# def coroutine(func):
#    def start(*args,**kwargs):
#        cr = func(*args,**kwargs)
#        cr.next()
#        return cr
#    return start


def try_for_debug(func):
    def in_fn(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            print("{} - {}: {}".format(func.__name__, type(e), str(e)))
            raise

    return in_fn


class SlidingWindowMonitor(object):
    """Monitor a live stream and report about significant tags.

       Will generate one report the first time the number of uses for a tag
       within an interval exceeds any of the report_triggers for that interval.

       Can monitor multiple window sizes simultaneously.

       The stream should be an iterable that yields
       ('tag', datetime.datetime) tuples or
       ('tag', int_timestamp) tuples

       The format of windows arg should match format input stream

       The co_process method can be used as part of a coroutine processing
       pipeline if an upstream process sends in data and an appropriate
       downstream consumer is specified.

       Specify a high_val_thresh_fn to compute reporting levels for tags with very high usage.
       high_val_thresh_fn should take a count and return some value which is then converted to int
       using math.floor().
        e.g.
            # Every multiple of max threshold: [10, 20, 30, 40, ...]
            high_val_thresh_fn = lambda x, t_max: x/t_max

            # Use a doubling rule: [10, 20, 40, 80, ....]
            high_val_thresh_fn = lambda x, t_max: math.log2(x/t_max)

       The highest trigger value is used as t_max.
       """

    def __init__(
            self,
            stream=None,
            tag_window_triggers = None,
            window=datetime.timedelta(seconds=30),
            windows=None,
            report_interval=datetime.timedelta(seconds=30),
            report_trigger=None,
            report_triggers=((16, 32, 64, 128, 256, 512, 1024),),
            consumer=None,
            high_val_thresh_fn=None,
            require_run=False,
            track_all_tags = False,
            no_window_overlap = False,
            report_decreases = False
    ):
        self.report_interval = report_interval
        self.tag_tracker = {}
        self.stream = stream
        self.tag_max_date = coll.OrderedDict()
        self.next_update_tag = sc.SortedList()
        self.last_report = {}
        self.start_datetime = None
        self.output = []
        self.high_val_thresh_fn = high_val_thresh_fn
        self.require_run = require_run
        self.track_all_tags = track_all_tags
        self.total_tag_count =  coll.defaultdict(int)
        self.no_window_overlap = no_window_overlap
        self.report_decreases = report_decreases



        if tag_window_triggers:
            parsed_tag_window_triggers = {datetime.timedelta(**window): sorted(triggers)
                           for window, triggers in tag_window_triggers}
            windows, report_triggers = zip(*sorted(parsed_tag_window_triggers.items()))
            # force ms conversion for now
            windows = (x.total_seconds() * 1000 for x in windows)

        # Single or multiple windows?
        if hasattr(windows, '__iter__'):
            self.windows = list(windows)
        else:
            self.windows = [window]

        assert self.windows == sorted(self.windows), WINDOW_SORT_ERR

        # Single or multiple triggers?
        if hasattr(report_triggers, '__iter__'):
            self.triggers = [sorted(x) for x in report_triggers]
        elif isinstance(report_trigger, int):
            self.triggers = [[report_trigger]]
        else:
            raise RuntimeError(TRIGGER_TYPE_ERR)

        assert len(self.triggers) == len(self.windows), WINDOW_TRIGGER_MISMATCH_ERR.format(self.triggers, self.windows)

        if isinstance(self.windows[0], datetime.timedelta):
            self.DTZERO = datetime.datetime(1970, 1, 1, 0, 0, 0, 0, pytz.UTC)
            self.windows_in_seconds = tuple(x.total_seconds() for x in self.windows)
        else:
            self.DTZERO = 0
            # assume milliseconds
            self.windows_in_seconds = tuple(x // 1000 for x in self.windows)

        self.oldest = self.DTZERO

        # Track the current level of each tag at each window width
        self.tag_current_level = {i: {} for i in self.windows}

        # Create required reporting keys:
        self.last_update = {
            w: {t: {} for t, _ in enumerate(trig)}
            for w, trig in zip(self.windows, self.triggers)
            }

        if consumer is None:
            self.consumer = self.default_reporter()
        else:
            self.consumer = consumer
        self.consumer.send(None)  # prime consumer coroutine

    # @coroutine
    @try_for_debug
    def default_reporter(self):
        while True:
            rep_items = (yield)
            if rep_items:
                self.report_candidate(*rep_items)

    @try_for_debug
    def process(self):
        if not self.stream:
            return
        self.output = []
        self.process_first()
        for tag, dtime in self.stream:
            self.process_one(tag, dtime, self.consumer)
            # Close-out active tags?

    # @coroutine
    @try_for_debug
    def co_process(self):
        while True:
            tag_dtime = (yield)
            if not tag_dtime:
                continue
            tag, dtime = tag_dtime
            try:
                # print("processing {}: {}".format(tag, len(self.tag_tracker)))
                self.process_one(tag, dtime, self.consumer)
            except Exception as e:
                print(tag, dtime)
                raise

    @try_for_debug
    def process_one(self, tag, dtime, consumer):
        if tag is None or dtime is None:
            return
        # Oldest time we care about this step
        min_time = dtime - 2 * self.windows[-1]
        # Housekeeping (tunable, every step is worst performance!)
        #  Update tag expiration
        self.update_tag_max(tag, dtime)
        #  Expire old tags:
        self.discard_old_tags(min_time)
        #  Do any tags have windows that need updating?
        # self.update_open_tags(dtime, consumer)

        # Process the tuple from the stream
        if self.track_all_tags:
            self.total_tag_count[tag] += 1

        self.single_tag_update(tag, dtime, consumer, insert=True)

    @try_for_debug
    def process_first(self):
        # Initialize with first item.
        tag, dtime = next(self.stream)
        self.start_datetime = dtime
        self.tag_tracker[tag] = coll.deque((dtime,))
        self.tag_max_date[tag] = dtime
        ## self.next_update_tag.add(??)   ####

    @try_for_debug
    def single_tag_update(self, tag, dtime, consumer, insert=False):
        # compute when needed ## window_edges = [dtime-x for x in self.windows]

        # Update tag use counts
        curr_counts, prev_counts = self.prune_count_compute(tag, dtime, insert)
        # current_all_counts = self.prune_counts(ALLTOKENS, dtime, window_edges, insert)
        delta_r = [
            (c - p) / (w ** 2)
            for c, p, w
            in zip(curr_counts, prev_counts, self.windows_in_seconds)
            ]

        # Instead of reporting, we now see if current_count indicates a
        # change in activity levels

        # We have multiple windows in current_counts
        # Each window might have a set of different activation levels
        # for each window, decide which activation level we match
        items = enumerate(zip(curr_counts, prev_counts, self.triggers))
        for i, (wc_curr, wc_prev, triggers) in items:

            level_idx = sum(wc_curr >= t for t in triggers)
            level_idx_prev = sum(wc_prev >= t for t in triggers)

            # allow for very large counts with high_val_thresh_fn
            if self.high_val_thresh_fn:
                if wc_curr > triggers[-1]:
                    ht_val = self.high_val_thresh_fn(wc_curr, triggers[-1])
                    ht_lvl_curr = math.floor(ht_val)
                    level_idx += ht_lvl_curr
                if wc_prev > triggers[-1]:
                    ht_val = self.high_val_thresh_fn(wc_prev, triggers[-1])
                    ht_lvl_prev = math.floor(ht_val)
                    level_idx_prev += ht_lvl_prev

            self.tag_current_level[self.windows[i]][tag] = level_idx

            w_size = self.windows[i]

            if not level_idx in self.last_update[w_size]:
                self.last_update[w_size][level_idx] = {}

            last_time = self.last_update[w_size][level_idx].get(tag, self.DTZERO)
            if self.no_window_overlap:
                last_time = max(
                    (x.get(tag, self.DTZERO) for x in self.last_update[w_size].values()),
                    default=self.DTZERO
                )

            if last_time + w_size > dtime:
                # suppress report:
                continue

            # Reporting Conditions
            two_period_run = level_idx > 0 and level_idx_prev > 0
            had_two_period_run = not self.require_run or two_period_run
            sig_activity = level_idx > 0
            lvl_chg = level_idx > level_idx_prev
            not_decreasing = self.report_decreases or ((wc_curr - wc_prev) >= 0)

            should_report = all([sig_activity, had_two_period_run, not_decreasing])
            if should_report:
                tag_level = level_idx
                delta_pct = (wc_curr - wc_prev) / wc_prev if wc_prev > 0 else None
                rep_items = (dtime, tag, tag_level, wc_curr, self.windows[i], delta_pct)
                # print("Reporting:".format(rep_items[1])
                consumer.send(rep_items)
                # self.output.append(rep_items)
                self.last_update[w_size][level_idx][tag] = dtime

    @try_for_debug
    def prune_update(self, tag, dtime, insert=False):
        # Get the list of tag use times and append
        tag_val = self.tag_tracker.get(tag, coll.deque())
        if insert:
            tag_val.append(dtime)

        # Expire old date entries and update dates:
        window_edge = dtime - 2 * self.windows[-1]
        while tag_val and tag_val[0] < window_edge:
            tag_val.popleft()

        if len(tag_val) < 1:
            _ = self.tag_tracker.pop(tag, None)
            _ = self.tag_max_date.pop(tag, None)
        else:
            self.tag_tracker[tag] = tag_val
            self.tag_max_date[tag] = window_edge

        return tag_val

    @try_for_debug
    def prune_count_compute(self, tag, dtime, insert=False):
        """ Remove expired entries, compute the count past period and the
        acceleration from previous period to this period.
        """
        tag_val = self.prune_update(tag, dtime, insert)
        assert (tag_val is not None), "tag_val_error"

        if len(tag_val) < 1:
            return ([0] * len(self.windows), [0] * len(self.windows))

        window_counts_curr = []
        window_counts_prev = []
        next_updates = []
        tag_val_len = len(tag_val)
        last_edge_curr = tag_val_len
        last_edge_prev = tag_val_len

        window_edges_prev = [dtime - 2 * x for x in self.windows]
        window_edges_curr = [dtime - x for x in self.windows]

        for edge_prev, edge_curr in zip(window_edges_prev, window_edges_curr):
            idx_curr = bisect.bisect(tag_val, edge_curr, hi=last_edge_curr)
            idx_prev = bisect.bisect(tag_val, edge_prev, hi=last_edge_prev)
            assert (idx_curr is not None), "idx_curr"
            assert (idx_prev is not None), "idx_prev"
            assert (tag_val_len is not None), "tag_val_len"
            if idx_curr < tag_val_len:
                edge_val_diff_curr = tag_val[idx_curr] - edge_curr
                next_updates.append(dtime + edge_val_diff_curr)

            window_counts_curr.append(tag_val_len - idx_curr)
            last_edge_curr = idx_curr
            last_edge_prev = idx_prev
            window_counts_prev.append(idx_curr - idx_prev)

            if idx_prev < idx_curr:
                edge_val_diff_prev = tag_val[idx_prev] - edge_prev

        if next_updates:
            self.next_update_tag.add((min(next_updates), tag))

        return (window_counts_curr, window_counts_prev)

    @try_for_debug
    def prune_counts(self, tag, dtime, window_edges, insert=False):

        # Get the list of tag use times and append
        tag_val = self.tag_tracker.get(tag, coll.deque())
        if insert:
            tag_val.append(dtime)

        window_edge = 2 * window_edges[-1]  # twice the oldest time
        # Expire old date entries and update dates:
        while tag_val and tag_val[0] <= window_edge:
            tag_val.popleft()

        self.tag_tracker[tag] = tag_val
        if tag_val:
            self.tag_tracker[tag] = tag_val
        else:
            _ = self.tag_tracker.pop(tag, None)
            _ = self.tag_max_date.pop(tag, None)
        if len(tag_val) < 1:
            return [0] * len(self.windows)

        window_counts = []
        next_updates = []
        tag_val_len = len(tag_val)
        last_edge = tag_val_len
        for edge in window_edges:
            idx = bisect.bisect(tag_val, edge, hi=last_edge)
            if idx < tag_val_len:
                edge_val_diff = tag_val[idx] - edge
                next_updates.append(dtime + edge_val_diff)
            window_counts.append(tag_val_len - idx)
            last_edge = idx
        if next_updates:
            self.next_update_tag.add((min(next_updates), tag))
        return window_counts

    @try_for_debug
    def update_tag_max(self, tag, dtime):
        # Pull tag from OrderedDict if it exists
        _ = self.tag_max_date.pop(tag, None)
        # Update tag expire time
        self.tag_max_date[tag] = dtime

    @try_for_debug
    def update_open_tags(self, dtime, consumer):
        #
        to_update = []
        for i, (t_to_up, tag) in enumerate(self.next_update_tag):
            if t_to_up < dtime:
                to_update.append(tag)
            else:
                break
        if len(to_update) < 1:
            return

        del self.next_update_tag[0:i]
        for tag in to_update:
            self.single_tag_update(tag, dtime, consumer, insert=False)

    @try_for_debug
    def discard_old_tags(self, window_edge):
        if self.oldest > window_edge:
            return

        to_rm = []
        for k, v in self.tag_max_date.items():
            if v < window_edge:
                to_rm.append(k)
            else:
                self.oldest = v
                break
        for k in to_rm:
            del self.tag_max_date[k]
            del self.tag_tracker[k]
            for window in self.windows:
                try:
                    del self.tag_current_level[window][k]
                except KeyError:
                    continue
                    # leaves orphan next_update_tag entries

    @try_for_debug
    def report_candidate(self, dtime, tag, tag_level, window_size, dv):
        subs = [dtime,
                tag,
                tag_level,
                window_size,
                dv,
                ]
        rep_string = u"{} - Reporting tag: {} with {} uses in {} interval and acc {}".format(*subs)
        print(rep_string)
