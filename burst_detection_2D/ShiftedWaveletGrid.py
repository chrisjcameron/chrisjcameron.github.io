from __future__ import division

import itertools as itr
import collections as coll
import decimal
import operator

# Define a GeoTweet type
class GeoTweet(object):
    __slots__ = ['utc', 'lon', 'lat', 'is_home_time', 'iso_cal']
    def __init__(
        self,
        utc=None,
        lon=None,
        lat=None,
        is_home_time=None,
        iso_cal=None
        ):
        self.utc = utc
        self.lon = lon
        self.lat = lat
        self.is_home_time = is_home_time
        self.iso_cal = iso_cal

"""
# Define a GeoTweet type
GeoTweet = coll.namedtuple('GeoTweet',['utc', 'lon', 'lat', 'is_home_time', 'iso_cal'])
GeoTweet.__new__.__defaults__ = (None,)*len(GeoTweet._fields)
"""

class ShiftedWaveletGrid(object):
    '''
    Use a grid with spacing equal to half the desired resolution
    and aggregate to a 0 and 1/2*resolution shifted grid so every possible
    concentration of points is identified, regardless of the
    arbitrary superimposition of the grid.

    #Example
    swt = ShiftedWaveletGrid()
    for geoT in tweets_collection:
        swt.insert(point)
    best_boxes = swt.get_best_home_candidates()
    very_best = best.boxes.keys()[0]
    points_in_best = swt.get_points_in_boxes(box=very_best)

    Uses GeoTweet namedtuple:
        GeoTweet:['utc', 'lon', 'lat', 'is_home_time']

    Define your own GeoTweet namedtuple as:
    GeoTweet = coll.namedtuple('GeoTweet',['utc', 'lon', 'lat', 'is_home_time', 'iso_cal'])
    GeoTweet.__new__.__defaults__ = (None,)*len(GeoTweet._fields)

    Uses decimal.Decimal for numbers

    Based on:
    Zhu, Y., & Shasha, D. (2003).
    Efficient elastic burst detection in data streams.
    doi:10.1145/956750.956789
    '''
    def __init__(self,
                 resolution=decimal.Decimal('.001'),
                 divisions=2,
                 ndays_threshold=10
                ):
        """
        resolution is fractions of degrees of lat/long
        """
        assert divisions%2==0, "Number of divisions must be even"
        self.sub_box_dict = {}
        self.box_dict = {}
        self.divisions = divisions
        self.scale = resolution if divisions==0 else resolution/divisions
        self.inv_scale = self.scale**-1
        #self.beta_offset = self.scale/2
        self.minor_box_unique_days = {}
        self.box_unique_days = {}
        self.ndays_threshold = ndays_threshold
        self.box_dict_out_of_sync = True
        #print(self.scale, self.inv_scale)

    def decimal_to_scale(self, n, scale):
        '''
        Fancy rounding function. Instead of rounding to the nearest integer,
        rounds to the nearest scale

        Example: if scale is 0.25, always rounds down to the nearest quarter
        If n = 2.7 and scale is 0.25, rounds to 2.5

        Used to compute the labels for the grid boxes. Does not wrap around!!!
        If -180 should wrap to 0, this function does not do it
        Needs boundary conditions to work for globe; works for US though
        '''
        r = n % scale
        if r >= 0:
            return n - r
        else:
            return n - (r + scale)

    def box_label(self, lon, lat):
        '''
        Given a decimal.Decimal lon and lat, compute the sub-box label for the
        box to which the lon,lat belong.
        '''
        return (self.decimal_to_scale(lon, self.scale),
                self.decimal_to_scale(lat, self.scale) )

    def insert(self, geotweet):
        '''
        User called function: Insert a geotweet into the sub-box grid
        '''
        assert isinstance(geotweet, GeoTweet), "ShiftedWaveletGrid only processes GeoTweets"
        # Compute labels
        blabel = self.box_label(geotweet.lon, geotweet.lat)

        # insert
        try:
            self.sub_box_dict[blabel].add(geotweet)
        except KeyError:
            self.sub_box_dict[blabel] = set((geotweet,))

        # update day count
        self.unique_days_update(blabel, geotweet.iso_cal)
        self.box_dict_out_of_sync = True

    def unique_days_update(self, label, isodate):
        '''
        Helper to update the count of unique days in the sub-grid box.
        '''
        try:
            self.minor_box_unique_days[label].add(isodate)
        except:
            self.minor_box_unique_days[label] = set((isodate,))

    def aggregate(self):
        """
        Each occupied cell in sub_box_dict scale contributes
        to the totals for several cells in box_dict with resolution scale*divisions

        This function puts points from the sub boxes into all upscaled boxes
        """
        for sub_box_label, geotweet_set in self.sub_box_dict.iteritems():
            for up_box_label in self.upscale_boxes(sub_box_label):
                try:
                    self.box_dict[up_box_label].update(geotweet_set)
                except KeyError:
                    self.box_dict[up_box_label] = set(geotweet_set)
                try:
                    self.box_unique_days[up_box_label].update(
                        self.minor_box_unique_days[sub_box_label]
                    )
                except KeyError:
                    self.box_unique_days[up_box_label] = set(
                        self.minor_box_unique_days[sub_box_label]
                    )
        self.box_dict_out_of_sync = False
        # box_dict has many candidate boxes with overlaps.

    def upscale_boxes(self, box):
        '''
        Internal function for the aggregation,
        matches the sub box to all up-scaled boxes
        '''
        if self.divisions==0:
            return (box,)
        # divisions box offset
        upscale_filter = [
            -self.scale*x for x in reversed(range(self.divisions))
        ]
        #= [-self.scale, 0]

        # compute the upscale boxes the input box contributes towards
        up_boxes = (
            tuple(itr.imap(operator.add, box,x)) for x in\
            itr.product(upscale_filter, upscale_filter)
        )
        return up_boxes

    def get_points_in_sub_boxes(self, boxes=None, box=None):
        '''
        Return the collection of GeoPoints within the collection of sub boxes
        defined by the labels in boxes. If only a single box's contents are needed
        supply only the box argument.
        '''
        if boxes is not None and box is not None:
            raise ValueError("Specify either boxes or box argument")
        if boxes is None and box is None:
            boxes = self.sub_box_dict.iterkeys()
        elif boxes is None and box:
        	boxes = (box,)
        points = (val for boxx in boxes for val in self.sub_box_dict[boxx])
        return points

    def get_points_in_boxes(self, boxes=None, box=None):
        '''
        User called function: Return the collection of GeoPoints
        within the collection of major boxes defined by the labels in
        boxes. If only a single box's contents are needed supply only the
        box argument.
        '''
        if boxes is not None and box is not None:
            raise ValueError("Specify either boxes or box argument")
        if self.box_dict_out_of_sync:
            self.aggregate()
        if boxes is None and box is None:
            boxes = self.box_dict.iterkeys()
        elif boxes is None and box:
            boxes = (box,)
        points = (point for b in boxes for point in self.box_dict[b])
        return points

    def neighborhood_boxes(self, box):
        '''
        A helper function to compute the labels of box and overlapping boxes.
        '''
        if self.divisions==0:
            return (box,)
        # divisions box offset
        upscale_filter = [
            self.scale*x for x in range(-(self.divisions-1), self.divisions)
        ]
        # compute the self + overlapping boxes
        nbr_boxes = (
            tuple(itr.imap(operator.add, box, x))
            for x in itr.product(upscale_filter, upscale_filter)
        )
        return nbr_boxes

    def home_box_candidates(self):
        '''
        Get the non-overlapping set of boxes that best
        contain the spatial bursts and exceed the n_days_threshold.
        Return the OrderedDict with box labels as keys and
        (#_weekend_evening, #_all_tweets) as values reverse sorted by
        the item values. I.E. The box with the most evening tweets is
        first and count of all tweets serves as a tie-breaker.

        For George:
        candidates = []
        for box_label, geotweets_list in self.box_dict.viewitems()
        	if len(self.box_unique_days[box_label]) >= self.ndays_threshold:
        		weekend_night_count = sum(x.is_home_time for x in geotweets_list)
        		tweet_count = len(geotweets_list)
        		candidates.append((box_label,weekend_night_count,tweet_count))

        '''
        # (boxlonlat, (home_count, point_count))
        # for boxes that have more than the unique day threshold
        candidates = (
            (k, (sum(x.is_home_time for x in v), len(v) ) )
            for k, v in self.box_dict.viewitems()
            if len(self.box_unique_days[k]) >= self.ndays_threshold
        )
        # sort by freq
        sorted_tuples = sorted(candidates, reverse=True, key=lambda x: x[1])
        hbox_candidates = coll.OrderedDict(sorted_tuples)
        # contains best candidates in order
        best_home_candidates = coll.OrderedDict()
        # this kills the other candidate boxes in one area
        # i.e. all of the overlapping boxes get removed
        while len(hbox_candidates) > 0:
            best_box, box_vals = next(hbox_candidates.iteritems())
            # Save best
            best_home_candidates[best_box] = box_vals
            # Knock out runners-up and move best
            for nbr in self.neighborhood_boxes(best_box):
                try:
                    del hbox_candidates[nbr]
                except KeyError:
                    pass
        return best_home_candidates if best_home_candidates else None

    def get_best_home_candidates(self):
        '''
        User called function:  Aggregate over all inserted points
        and return the non-overlapping set of boxes that best contain the
        spatial bursts and exceed the n_days_threshold. Return the
        OrderedDict with box labels as keys and (#_weekend_evening,
        #_all_tweets) as values reverse sorted by the item values. I.E.
        The box with the most evening tweets is first and count of all
        tweets serves as a tie-breaker.
        '''
        self.aggregate()
        return self.home_box_candidates()
