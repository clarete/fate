# Python batteries & external modules
import io
import datetime
import dateutil.tz

# Testing tools
import sure
import mock

# Code to be tested
import process


# Stuff needed during tests
TZ = dateutil.tz.tzoffset(None, -18000)


def test_tests_running():
    "Ensure the test runner is working"
    1 .should.equal(1)


@mock.patch('process.io')
def test_read(process_io):
    "Ensure reader yields lines of the CSV file"

    # Given a CSV file
    process_io.open.return_value = io.BytesIO('''\
2017-12-31T12:15:32.584059428-0500,*scratch*, *Minibuf-1*
2017-12-31T12:15:34.618458371-0500, *Minibuf-1*,Spotify - Spotify''')

    # When the above file is read
    output = list(process.read('input file'))

    # Then it should enumerate all the lines of the file with the date
    # parsed
    output.should.equal([
        (datetime.datetime(2017, 12, 31, 12, 15, 32, 584059, tzinfo=TZ),
         '*scratch*',
         ' *Minibuf-1*'),
        (datetime.datetime(2017, 12, 31, 12, 15, 34, 618458, tzinfo=TZ),
         ' *Minibuf-1*',
         'Spotify - Spotify')
    ])


def test_count():
    "Ensure count yields the right amount of seconds spent in each buffer"

    # Given the following input events
    input_data = iter([
        (datetime.datetime(2017, 12, 31, 12, 15, 32, 0, tzinfo=TZ),
         'Buffer 1', 'Buffer 2'),
        (datetime.datetime(2017, 12, 31, 12, 15, 33, 0, tzinfo=TZ),
         'Buffer 2', 'Buffer 3'),
        (datetime.datetime(2017, 12, 31, 12, 15, 35, 0, tzinfo=TZ),
         'Buffer 3', 'Buffer 4'),
        (datetime.datetime(2017, 12, 31, 12, 15, 38, 0, tzinfo=TZ),
         'Buffer 4', 'Buffer 5')
    ])

    # When the time is countend
    output = list(process.count(input_data))

    # Then it should count how much time was spent in each buffer
    output.should.equal([
        ('Buffer 2', 1.0),
        ('Buffer 3', 2.0),
        ('Buffer 4', 3.0),
    ])
