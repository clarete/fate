import csv
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


def accumulate(data):
    values = defaultdict(int)
    for name, time in data:
        values[name] += time
    return values.items()


def main():
    for name, time in accumulate(count(read(DB_FILE))):
        print(name, time)


if __name__ == '__main__':
    main()
