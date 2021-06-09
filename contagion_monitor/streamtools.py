import datetime
import heapq
import itertools as itr
import time

import pandas as pd

import swm_py3 as swm


def try_for_debug(func):
    def in_fn(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            print("{} - {}: {}".format(func.__name__, type(e), str(e)))
            raise

    return in_fn


class TokenTracker(object):
    def __init__(self, **kwargs):
        # create a SWM
        self.swm = swm.SlidingWindowMonitor(**kwargs)
        self.swm_cp = self.swm.co_process

    def ingress(self):
        co_gen = self.swm_cp()
        co_gen.send(None)
        return co_gen


class NgramTracker(TokenTracker):
    def __init__(self, grams=(2, 3), sig_cutoff=None, **kwargs):
        super().__init__(**kwargs)
        self.grams = grams
        self.sig_cutoff = sig_cutoff

        # Create necessary TF-IDF tracking variables
        ##
        ##

        # Hold off reporting n-grams until we learn about the text
        # for now, let us hold the first 1000 tweets
        self.pre_history = []
        self.doc_count = 0

    def ingress(self):
        co_gen = self.co_process()
        co_gen.send(None)
        return co_gen

    def significant_words(self, words):
        for word in words:
            pass
            # update TF-IDF scores

        return [word for word in set(words) if self.is_sig_word(word)]

    def is_sig_word(self, word):
        # is the word score high enough?
        return True

    def send_ngrams(self, sig_words, dtime):
        for n in self.grams:
            for ngram in itr.combinations(sorted(sig_words), n):
                self.swm_cp.send((ngram, dtime))

    def co_process(self):
        while True:
            words, dtime = (yield)
            if not words:
                continue

            self.doc_count += 1

            if self.doc_count < 1000:
                self.pre_history.append((words, dtime))

            elif self.doc_count == 1000:
                for words, dtime in self.pre_history:
                    sig_words = self.significant_words(words)
                    self.send_ngrams(sig_words, dtime)
                self.pre_history = None

            else:
                sig_words = self.significant_words(words)
                self.send_ngrams(sig_words, dtime)


class SigWordGramTracker(TokenTracker):
    def __init__(self, grams=(2, 3), sig_cutoff=None, stop_words=None, **kwargs):
        super().__init__(**kwargs)
        self.grams = grams
        self.sig_cutoff = sig_cutoff
        self.stop_words = {}
        if isinstance(stop_words, dict):
            self.stop_words = stop_words

        # setup SWM coprocess
        self.swm_co_gen = self.swm_cp()
        self.swm_co_gen.send(None)
        
        # Create necessary TF-IDF tracking variables
        ##
        ##
        self.word_doc_freq = {}
        # for now, accumulate counts forever
        # otherwise implement a hist tracker
        # of documents

        # Hold off reporting n-grams until we learn about the text
        # for now, let us hold the first 1000 tweets
        self.pre_history = {}
        self.doc_count = {}

    def ingress(self):
        co_gen = self.co_process()
        co_gen.send(None)
        return co_gen

    def update_word_sig(self, words, lang):
        if not lang in self.word_doc_freq:
            self.word_doc_freq[lang] = {}
        for word in set(words):
            if word in self.stop_words.get(lang,set()):
                continue
            self.word_doc_freq[lang][word] = self.word_doc_freq[lang].get(word,0) + 1
        # could expire old
    
    def get_sig_words(self, words, lang):
        return [word for word in set(words) if self.is_sig_word(word, lang)]

    def is_sig_word(self, word, lang):
        # is the word infrequent enough?
        if word in self.stop_words.get(lang,set()):
            return False
        if self.sig_cutoff is None:
            return True
        if not lang in self.word_doc_freq:
            return True
        if not lang in self.doc_count:
            return True
        return self.word_doc_freq.get(lang, {}).get(word,0)/self.doc_count[lang] < self.sig_cutoff

    def send_ngrams(self, sig_words, dtime):
        for n in self.grams:
            for ngram in itr.combinations(sorted(sig_words), n):
                self.swm_co_gen.send((ngram, dtime))

    def co_process(self):
        while True:
            (lang, words), dtime = (yield)
            if not words:
                continue

            self.doc_count[lang] = self.doc_count.get(lang,0) + 1
            if not lang in self.pre_history:
                self.pre_history[lang] = []
            self.update_word_sig(words, lang)

            if self.doc_count[lang] < 1000:
                self.pre_history[lang].append((words, dtime))
                continue

            elif self.doc_count[lang] == 1000:
                for words, dtime in self.pre_history[lang]:
                    sig_words = self.get_sig_words(words, lang)
                    self.send_ngrams(sig_words, dtime)
                del self.pre_history[lang]
            
            else:
                sig_words = self.get_sig_words(words, lang)
                self.send_ngrams(sig_words, dtime)
                



class SimpleAccumulator(object):
    def __init__(self, printing=False):
        self.data = {}
        self.printing = printing
        self.df = None

    def ingress(self):
        co_gen = self.co_process()
        co_gen.send(None)
        return co_gen

    @try_for_debug
    def co_process(self):
        """Accumulate incoming reports"""
        while True:
            report_items = (yield)
            if not report_items:
                continue

            # rep_items = (dtime, tag, tag_level, wc_curr, self.windows[i], delta_pct)
            dtime, tag, tag_level, count, window, delta_pct = report_items
            record = (dtime, tag_level, count, window, delta_pct)
            if self.printing:
                print("Reporting {} at level {}".format(tag, tag_level))
            try:
                self.data[tag].append(record)
            except KeyError:
                self.data[tag] = [record]

    def write_dframe(self, out_file_path):
        """Create a dataframe of reports and write to a file"""
        print("# of reports: {}".format(len(self.data)))
        df_data = []
        for k, v in self.data.items():
            tag = k
            for record in v:
                df_data.append((tag, record[0]))

        df = pd.DataFrame.from_records(
            df_data,
            columns=['tag', 'time']
        )

        td_grp = df.groupby('tag')
        df_utags = pd.DataFrame({'count': td_grp['time'].size(), 'min_date': td_grp['time'].min()}).reset_index()
        # print_all(df_utags)
        df_utags.sort_values(by='min_date', inplace=True)
        df_utags.to_csv(out_file_path, index=False, encoding='utf-8')
        print(df_utags.shape)

    def create_df(self):
        df_data = []
        for k, v in self.data.items():
            tag = k
            for record in v:
                items = [tag]
                items.extend(record)
                df_data.append(items)

        self.df = pd.DataFrame.from_records(
            df_data,
            columns=['tag', 'time', 'tag_level', 'count', 'window', 'delta_pct']
        )
        
    
    def write_reports(self, out_file_path):
        """Create a dataframe of reports and write to a file"""
        print("# of reports: {}".format(len(self.data)))
        self.create_df()
        self.df.to_csv(out_file_path, index=False, encoding='utf-8')
        print(self.df.shape)


class StreamCleaner(object):
    def __init__(self, delay, consumer):
        if isinstance(delay, datetime.timedelta):
            self.delay_ms = delay.total_seconds() * 1000
        else:
            self.delay_ms = int(delay)
        self.consumer = consumer
        self.hq = []
        self.last_seen = None
        self.max_ms = 0
        self.insert_order = itr.count()

    def ingress(self):
        co_gen = self.co_process()
        co_gen.send(None)
        return co_gen

    @try_for_debug
    def co_process(self):
        """Coroutine to insert and yield tweets"""
        while True:
            item = (yield)
            if not item:
                continue
            try:
                t_ms = int(item["timestamp_ms"])
                tid = item["id_str"]
                # print("StreamCleaner: got {}".format(tid))
                assert isinstance(t_ms, int), "t_ms type error"

                if t_ms > self.max_ms:
                    self.max_ms = t_ms
                if t_ms >= self.max_ms - self.delay_ms:
                    q_entity = (
                        t_ms,
                        tid,
                        next(self.insert_order),
                        item
                    )
                    heapq.heappush(self.hq, q_entity)
                    self.clearq(t_ms=self.max_ms)
                else:
                    msg_fmt = "Discarding late arrival of {} seconds"
                    msg = msg_fmt.format((self.max_ms - t_ms) / 1000)
                    print(msg)
            except KeyError as e:
                continue
        finally:
            # flush queue at end of input
            self.clearq() 

    @try_for_debug
    def clearq(self, t_ms=None):
        """Given current timestamp, yield from queue to consumer"""
        while len(self.hq) > 0 and (t_ms is None or (t_ms - self.hq[0][0] > self.delay_ms)):
            try:
                _ts, tid, _ord, current_item = heapq.heappop(self.hq)
                # print("StreamCleaner.clearq: popped {}".format(tid))
                if current_item != self.last_seen:
                    self.last_seen = current_item
                    # print("StreamCleaner: sending {}".format(tid))
                    self.consumer.send(current_item)
                    # print("StreamCleaner: sent {}".format(tid))
            except Exception as e:
                print("clearq - Error: {}".format(e))
                print(type(e))
                print(len(self.hq))
                print(_ts, tid, _ord, current_item)
                raise
