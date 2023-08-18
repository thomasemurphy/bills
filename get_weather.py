import sys
import requests
import json
import time

import datetime as dt
import pandas as pd

sys.path.append('../..')
from api_keys.baseball_config import api_key

home_lat = 37.8038047
home_long = -122.2142252

def construct_timestamp(date_str, hour_int, tz_str):
    year, month, day = [int(x) for x in date_str.split('-')]
    return pd.Timestamp(year=year, month=month, day=day, hour=hour_int, tz=tz_str)

def generate_api_query(
    api_key = api_key,
    lat = home_lat,
    lon = home_long,
    date_str = '2023-01-01',
    hour_int = 0,
    timezone = 'US/Pacific'
    ):

    dt_int = int(construct_timestamp(date_str, hour_int, timezone).timestamp())
    api_query = f'https://api.openweathermap.org/data/3.0/onecall/timemachine?lat={lat}&lon={lon}&dt={dt_int}&APPID={api_key}'

    return api_query

def make_csv_for_month(year=2023, month=1):
    start_dt_str = str(year) + '-' + str(month) + '-1'
    days_in_month = pd.Period(start_dt_str).days_in_month
    temp_dict = {}
    for i in range(days_in_month):
        date_str = str(year) + '-' + str(month) + '-' + str(i + 1)
        my_query = generate_api_query(date_str = date_str)
        api_response = requests.get(my_query)
        my_content = api_response.content
        my_json = json.loads(my_content)
        # print(my_json)
        # print(my_json['lat'])
        my_temp_K = my_json['data'][0]['temp']
        temp_dict[date_str] = my_temp_K
    my_series = pd.Series(temp_dict)
    my_series.to_csv(f'data/weather/midnite_temps_{year}_{month}.csv')

def loop_thru_years(years = [2023]):
    for year in years:
        for month in range(7):
            print(f'{year}-{month}')
            make_csv_for_month(year, month + 1)
            time.sleep(40)

# # got stuck here - this weather API only grants 365 days lookback access

# def generate_history_query_month(api_key, zip_code=94602, year=2023, month=1):
#     start_dt = str(year) + '-' + str(month) + '-1'
#     days_in_month = pd.Period(start_dt).days_in_month
#     end_dt = str(year) + '-' + str(month) + '-' + str(days_in_month)
#     history_query = f'http://api.weatherapi.com/v1/history.json?key={api_key}&q={zip_code}&dt={start_dt}&end_dt={end_dt}'
#     return history_query

# def generate_history_query_day(api_key, zip_code=94602, year=2023, month=1, day=1):
#     dt = str(year) + '-' + str(month) + '-' + str(day)
#     days_in_month = pd.Period(dt).days_in_month
#     history_query = f'http://api.weatherapi.com/v1/history.json?key={api_key}&q={zip_code}&dt={dt}'
#     return history_query

# def get_json_from_query(query):
#     response = requests.get(query)
#     response_json = json.loads(response.content)
#     return response_json

# def get_mean_temp_for_json_month(history_json):
#     n_days = len(history_json['forecast']['forecastday'])
#     days_temp_list = []
#     for i in range(n_days):
#         day_mean_temp = history_json['forecast']['forecastday'][i]['day']['avgtemp_f']
#         days_temp_list.append(day_mean_temp)
#     month_mean_temp = pd.Series(days_temp_list).mean()
#     print(days_temp_list)
#     return month_mean_temp
