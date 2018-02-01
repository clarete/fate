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

import argparse
import csv
import datetime
import re
import io
import os
import sys

import dateparser
import dateutil.parser
import pylab

from collections import defaultdict

DB_FILE = os.path.expanduser('~/.fate.db')


def read(input_file):
    with io.open(input_file, 'rb') as db:
        for line in csv.reader(db, escapechar='\\'):
            timestamp, from_buffer, to_buffer = line
            yield dateutil.parser.parse(timestamp), from_buffer, to_buffer


def count(data):
    a, b = next(data), next(data)
    while a and b:
        # format of a & b: [date_time, from_buffer, to_buffer]
        yield (a[0], b[0]), b[1], (b[0] - a[0]).total_seconds()
        a, b = b, next(data)


RES = {
    re.compile('\*\*idle-fate\*\*'): 'Idle',
    re.compile('^\s*Franz'): 'Chat',

    re.compile('^\s*\*Minibuf-\d\*'): 'Minibuf',
    re.compile('^\s*\*Backtrace'): 'Emacs',
    re.compile('^\s*\*Completions'): 'Emacs',
    re.compile('^\s*\*Help'): 'Emacs',
    re.compile('^\s*\*Messages'): 'Emacs',

    re.compile('^\s*Pgadmin'): 'Pgadmin',
    re.compile('^\s*Spotify'): 'Spotify',
    re.compile('^\s*Firefox\s-'): 'Browsing',
    re.compile('^\s*Chromium\s-'): 'Browsing',
    re.compile('\*eww'): 'Browsing',

    re.compile('\*ansi-term\*'): 'Terminal',
    re.compile('\*eshell\*'): 'Terminal',

    re.compile('\*magit'): 'Coding',
    re.compile('\*Org\sAgenda'): 'Org',
    re.compile('\*Calendar\*'): 'Org',
}


def by_same(name):
    for regex, short_name in RES.items():
        if regex.match(name):
            return short_name
    return name


PATHS = {
    os.path.expanduser('~/src'): 'Coding',
    os.path.expanduser('~/org'): 'Org',
}

def by_path(name):
    for path, short_name in PATHS.items():
        if name.startswith(path):
            return short_name
    return name


def date_filter(data, filter_range):
    for date_range, name, time in data:
        tzinfo = date_range[0].tzinfo
        if date_range[0] >= filter_range[0].replace(tzinfo=tzinfo) and \
           date_range[1] <= filter_range[1].replace(tzinfo=tzinfo):
            yield name, time


def group(by, data):
    values = defaultdict(int)
    for name, time in data:
        values[by(name)] += time
    return values.items()


def print_groups(groups):
    for name, time in sorted(groups.items(), key=lambda x: x[1]):
        print(name, time)


def pie(output, groups):
    pylab.figure(1, figsize=(6,6))
    pylab.pie(groups.values(), labels=groups.keys())
    pylab.savefig(output, bbox_inches='tight')


def commandline():
    parser = argparse.ArgumentParser(description='Visualize fate data.')
    parser.add_argument('--date-from', metavar='DF',
                        type=dateparser.parse,
                        default=datetime.datetime(2017, 12, 1, 0, 0),
                        help='show no entries before this date')
    parser.add_argument('--date-to', metavar='DT',
                        type=dateparser.parse,
                        default='now',
                        help='show no entries after this date')
    parser.add_argument('--output', metavar='OUT',
                        default='fate-output.png',
                        help='output file name')
    return parser.parse_args()


def main():
    args = commandline()
    data = date_filter(count(read(DB_FILE)), (args.date_from, args.date_to))
    groups = group(by_path, group(by_same, data))
    filtered = {k: v for k, v in groups if v > 500}
    print_groups(filtered)
    pie(args.output, filtered)


if __name__ == '__main__':
    main()
