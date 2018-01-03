import csv
import re
import io
import os
import dateutil.parser

from collections import defaultdict

DB_FILE = os.path.expanduser('~/.fate.db')


def read(input_file):
    with io.open(input_file, 'rb') as db:
        for line in csv.reader(db):
            timestamp, from_buffer, to_buffer = line
            yield dateutil.parser.parse(timestamp), from_buffer, to_buffer


def count(data):
    a, b = data.next(), data.next()
    while a and b:
        yield b[1], (b[0] - a[0]).total_seconds()
        a, b = b, data.next()


def accumulate(data, transform=lambda x: x):
    values = defaultdict(int)
    for name, time in data:
        values[transform(name)] += time
    return values.items()



RES = {
    'Spotify': re.compile('^\s*Spotify'),
    'Minibuf': re.compile('^\s*\*Minibuf-\d\*'),
    'Firefox': re.compile('^\s*Firefox\s-'),
}

def dedup(name):
    for short_name, regex in RES.items():
        if regex.match(name):
            return short_name
    return name


def main():
    accumulated = accumulate(count(read(DB_FILE)), dedup)
    for name, time in sorted(accumulated, key=lambda x: x[1]):
        print(name, time)


if __name__ == '__main__':
    main()
