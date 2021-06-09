import json
import regex as re
import itertools as itr
from dateutil import parser as dtp

def try_for_debug(func):
    def in_fn(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            print("{} - {}: {}".format(func.__name__, type(e), str(e)))
            raise
    return in_fn


def remove_punctuation(text):
    pat = re.compile(r"\p{P}+")
    return pat.sub("", text)


def get_tags(tweet, lower=False):
    """Extract tags from a tweet"""
    try:
        ent = tweet.get('entities', [])
        if not ent:
            return []
        if lower:
            return (tag['text'].lower() for tag in ent.get('hashtags', []))
        return set(tag['text'] for tag in ent.get('hashtags', []))
    except KeyError as e:
        print("get_tags got KeyError")


def get_tag_grams(tweet, gram_length=2, lower=True):
    """Get all grams composed of tags"""
    tags = sorted(set(get_tags(tweet, lower=lower)))
    return itr.combinations(tags, r=gram_length)


def get_urls(tweet, key='url'):
    """Extract urls from a tweet"""
    ent = tweet.get('entities', [])
    if not ent:
        return []
    return (tag[key] for tag in ent.get('urls', []))


def get_mentions(tweet):
    """Extract tags from a tweet"""
    ent = tweet.get('entities', [])
    if not ent:
        return []
    return (tag['id_str'] for tag in ent.get("user_mentions", []))


def get_words(tweet):
    """Extract non-entity words from a tweet"""
    ents = tweet.get('entities', [])
    text = list(tweet.get('text', ''))
    if not text:
        return []

    # Remove the entities from the text
    text_ls = text
    for e_list in ents.values():
        idxs = [o.get('indices') for o in e_list]
        for idx in idxs:
            for i in range(*idx):
                text_ls[i] = u' '

    # Remove punctuation
    text = u''.join(text_ls)
    text_np = remove_punctuation(text)

    # Lowercase and split text into words
    words = text_np.strip().lower().split()

    return [words]


def get_lang_words(tweet):
    """Extract non-entity words and language tag from a tweet"""
    ents = tweet.get('entities', [])
    ents.pop('polls',None)
    text = list(tweet.get('text', ''))
    #text = tweet.get('text', '')
    lang = tweet.get('lang', 'und')
    if not text:
        return (lang, [])

    # Remove the entities from the text
    text_ls = text
    for e_list in ents.values():
        idxs = [o.get('indices') for o in e_list]
        for idx in idxs:
            for i in range(*idx):
                text_ls[i] = u' '

    # Remove punctuation
    text = u''.join(text_ls)
    text_np = remove_punctuation(text)
    
    # Lowercase and split text into words
    words = [
        x.strip().lower() 
        for x in text_np.split() 
        if ("http" not in x) and (len(x) > 2)
    ]
    #words = text_np.strip().lower().split()
    #print(lang, words)
    return ((lang, words),)


@try_for_debug
def json_parser(consumer=None):
    line_count = 0
    drop_count = 0
    while True:
        try: 
            line = (yield)
            line = line.replace("\\U000","")
            if not line or not line.strip():
                drop_count += 1
                continue
            line_count += 1
            if line_count % 10000 == 0:
                print('Processed {} lines, dropped {}'.format(line_count, drop_count))
            try:
                tweet = json.loads(line)
                #print("json_parser: sending {}".format(tweet['id_str']))
                consumer.send(tweet)
            except (ValueError, KeyError) as e:
                drop_count += 1
                # Could be a "delete" notice or misread
                print(e)
                continue
        except:
            print('Processed {} lines, dropped {}'.format(line_count, drop_count))
            raise

@try_for_debug
def pre_process(consumer=None):
    """Fix some fields via inference"""
    # case insensitive 'rt' followed by a non-word character
    # ending the substring preceding a user_mention
    # Some exclusions to prevent partial word or url matches
    re_pat = re.compile(r"(?iV1)(?<=^|(?<=[^\p{L}\p{N}-\.]))rt[\W]*.$")
    while True:
        tweet = (yield)
        if tweet is None:
            continue
        try:
            is_rt = bool(tweet.get('retweeted_status', False))
            if is_rt:
                consumer.send(tweet)
                continue
            text = tweet['text']
            mentioned_users = tweet['entities']['user_mentions']
            last_idx = 0
            for user in mentioned_users:
                if re_pat.match(text[last_idx:user['indices'][0]]):
                    tweet['retweeted_status'] = True
                    break
                last_idx = user['indices'][1]

            consumer.send(tweet)
            continue
        except (ValueError, KeyError) as e:
            print(str(e))
            # Let the downstream handle it
            consumer.send(tweet)
            continue


@try_for_debug
def token_extractor(pipeline=None, sort_key='timestamp_ms'):
    """pipeline=[{'getter':get_tags, 'for_each':tag_tracker, 'for_rt':rt_tag_tracker}]"""
    while True:
        # print("token_extractor: waiting")
        tweet = (yield)
        # print("token_extractor: got something {}".format(tweet.get("tid", None)))
        if tweet is None:
            continue
        # print("token_extractor: got {}".format(tweet["id"]))

        try:
            dtime = int(tweet[sort_key])
            is_rt = bool(tweet.get('retweeted_status', False))
        except (ValueError, KeyError) as e:
            # Could be a "delete" notice or misread
            continue

        for token_type in pipeline:
            # print("Extracting with {}".format(token_type['getter'].__name__))
            track_tokens = token_type['for_each']
            track_rt = token_type.get('for_rt', False)
            tokens = [x for x in token_type['getter'](tweet) if x is not None]
            if not tokens:
                continue
            for token in tokens:
                #print("token: {}".format(token))
                track_tokens.send((token, dtime))
                if is_rt and track_rt:
                    # print("rt_token: {}".format(token))
                    track_rt.send((token, dtime))
