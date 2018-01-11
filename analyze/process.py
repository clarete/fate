# Copyright (C) 2018  Lincoln de Sousa <lincoln@clarete.li>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import csv
import datetime
import re
import io
import os
import sys

import dateparser
import dateutil.parser

from collections import defaultdict

DB_FILE = os.path.expanduser('~/.fate.db')


def read(input_file):
    with io.open(input_file, 'rb') as db:
        for line in csv.reader(db):
            timestamp, from_buffer, to_buffer = line
            yield dateutil.parser.parse(timestamp), from_buffer, to_buffer


def count(data):
    a, b = next(data), next(data)
    while a and b:
        # format of a & b: [date_time, from_buffer, to_buffer]
        yield (a[0], b[0]), b[1], (b[0] - a[0]).total_seconds()
        a, b = b, next(data)


RES = {
    '*Minibuf*': re.compile('^\s*\*Minibuf-\d\*'),
    'Spotify': re.compile('^\s*Spotify'),
    'Firefox': re.compile('^\s*Firefox\s-'),
    'Chromium': re.compile('^\s*Chromium\s-'),
}


def dedup(name):
    for short_name, regex in RES.items():
        if regex.match(name):
            return short_name
    return name


def accumulate(data, filtr):
    values = defaultdict(int)
    for date_range, name, time in data:
        if date_filter(filtr, date_range):
            values[dedup(name)] += time
    return values.items()


def date_filter(filter_range, date_range):
    tzinfo = date_range[0].tzinfo
    return date_range[0] >= filter_range[0].replace(tzinfo=tzinfo) and \
        date_range[1] <= filter_range[1].replace(tzinfo=tzinfo)


def main():
    len_argv = len(sys.argv)
    if len_argv == 1:
        from_date = datetime.datetime(2017, 12, 1, 0, 0)
        to_date = datetime.datetime.now()
    elif len_argv == 2:
        from_date = dateparser.parse(sys.argv[1])
        to_date = datetime.datetime.now()
    elif len_argv == 3:
        from_date = dateparser.parse(sys.argv[1])
        to_date = dateparser.parse(sys.argv[2])
    date_range = (from_date, to_date)
    accumulated = accumulate(count(read(DB_FILE)), date_range)
    for name, time in sorted(accumulated, key=lambda x: x[1]):
        print(name, time)


if __name__ == '__main__':
    main()
