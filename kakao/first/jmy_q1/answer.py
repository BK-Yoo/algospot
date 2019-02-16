import codecs
import re
import sys
from random import randint

sys.stdin = codecs.getreader("utf-8")(sys.stdin.detach())
sys.stdout = codecs.getwriter("utf-8")(sys.stdout.detach())


def fy_shuffle_evenly(arr):
    for cur in range(len(arr) - 1, 0, -1):
        pick = randint(0, cur)
        if pick != cur:
            arr[cur], arr[pick] = arr[pick], arr[cur]

    # for removing closeness of same songs
    for idx in range(0, len(arr) - 2):
        if arr[idx] == arr[idx + 1]:
            arr[idx + 1], arr[idx + 2] = arr[idx + 2], arr[idx + 1]
    return arr


def sort_artist_as_length_of_songs(artist_to_song):
    # divide artist by length of songs
    len_to_artist = {}
    for artist, songs in artist_to_song.items():
        if len(songs) in len_to_artist:
            len_to_artist[len(songs)].append(artist)
        else:
            len_to_artist[len(songs)] = [artist]

    # shuffle if there is one more artist in specific length
    result = []
    for key in sorted(len_to_artist.keys(), reverse=True):
        result += fy_shuffle_evenly(len_to_artist[key])
    return result


def find_blank_pos_evenly(cur, playlist):
    if not playlist[cur]:
        return cur

    else:
        left, right = None, None
        for left_idx in range(cur - 1, -1, -1):
            if not playlist[left_idx]:
                left = left_idx
                break
        for right_idx in range(cur + 1, len(playlist)):
            if not playlist[right_idx]:
                right = right_idx
                break

        if left is None:
            return right
        elif right is None:
            return left
        else:
            # closer to initial position will be selected
            if cur - left < right - cur:
                return left
            else:
                return right


def start():
    # num_of_test_case  = int(input())
    with open('./test_case.txt') as f:
        for _ in range(0, int(f.readline())):
            playlist = re.split(r'\t+', f.readline())
            artists = re.split(r'\t+', f.readline())
            random_playlist = [""] * len(playlist)

            # sort playlist by artists
            art_to_song = {}
            for song, artist in zip(playlist, artists):
                if artist not in art_to_song:
                    art_to_song[artist] = [song]
                else:
                    art_to_song[artist].append(song)

            # sort each group of songs
            for artist in art_to_song:
                fy_shuffle_evenly(art_to_song[artist])

            # sort keys ordered by the length of songs of artist(desc)
            sorted_artists = sort_artist_as_length_of_songs(art_to_song)

            # combine as one playlist
            # put the highest number of songs first
            size = len(random_playlist)
            for artist in sorted_artists:
                songs = art_to_song[artist]

                # get positions of each songs as evenly as possible
                stripe = int(size / len(songs))
                pos = [stripe * step for step in range(0, len(songs))]

                # set random offset to possible positions
                random_offset = randint(0, size - 1 - pos[-1])
                pos = map(lambda e: e + random_offset, pos)

                # put songs to random playlist
                for p, song in zip(pos, songs):
                    p = find_blank_pos_evenly(p, random_playlist)
                    random_playlist[p] = song

            print('\t'.join(random_playlist))


if __name__ == '__main__':
    start()
