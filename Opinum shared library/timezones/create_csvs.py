import pandas as pd
import datetime as dt

import pytz


def create_csv(serial_number, mapping_config, dates, format):
    df = pd.DataFrame()
    df['date'] = dates.strftime(format)
    df['value'] = fibos
    df['source_serialnumber'] = serial_number
    df['mapping_config'] = mapping_config
    df.to_csv(f"StandardDataFile-{serial_number}-{mapping_config}.csv", index=False)


serial_numbers = ['UTC_shared', 'Brussels_shared']
for site in ('UTC', 'Local', 'FarAway'):
    for source in ('UTC', 'Site_timezone', 'Brussels', 'Site_timezone_adapted'):
        serial_numbers.append(f'{site}-{source}')

utc_dates = pd.date_range(dt.datetime(2021, 10, 28, tzinfo=pytz.UTC),
                          dt.datetime(2021, 11, 2, tzinfo=pytz.UTC),
                          freq='1H')
brussels_dates = utc_dates.tz_convert(pytz.timezone("Europe/Brussels"))

fibos = [0.000002, 0.000003]
for _ in range(len(utc_dates) - 2):
    fibos.append(fibos[-2] + fibos[-1])

for serial_number in serial_numbers:
    create_csv(serial_number, 'unaware', utc_dates, '%Y-%m-%dT%H:%M:%S')
    create_csv(serial_number, 'unaware_brussels', brussels_dates, '%Y-%m-%dT%H:%M:%S')
    create_csv(serial_number, 'utc', utc_dates, '%Y-%m-%dT%H:%M:%SZ')
    create_csv(serial_number, 'utc_2', utc_dates, '%Y-%m-%dT%H:%M:%S%z')
    create_csv(serial_number, 'brussels', brussels_dates, '%Y-%m-%dT%H:%M:%S%z')


