import json
import ast
import requests
import logging
import pandas as pd
from pandas import json_normalize

# URL to look at season information
season_end_url = "https://api-web.nhle.com/v1/standings-season"
season_end_request = requests.get(season_end_url)
season_end_dates = season_end_request.json()

# Get the end dates for each season in the season_end_dates object
season_end = [season.get('standingsEnd') for season in season_end_dates['seasons']]

# Initialize an empty list for the standings data
standings_list = []

# Loop through the end dates in season_end object
for date in season_end:
    try: 
        # Query the NHL API to get standings by the end date of the season
        url = f"https://api-web.nhle.com/v1/standings/{date}"
        response = requests.get(url)
        response.raise_for_status()
        data = response.json()
        # Create a DataFrame object containing the standings on that date
        df = pd.DataFrame(data['standings'])
        # Append the DataFrame to the list
        standings_list.append(df)
    
        logging.info(f"Successful for {date}")
    except Exception as e: 
        logging.error(f"Error for {date}")

# Create one large DataFrame object containing the standing year by year
standings_df = pd.concat(standings_list)
# Export the standings data to a csv file
# standings_df.to_csv('standings.csv', index=False)

# format the dates for standings with around 10-15 games left in season
season_end = ['{year}-03-15'.format(year=year) for year in range(1996, 2025) if year != 2005]
season_end[season_end.index('2020-03-15')] = '2020-02-17'
# Initialize an empty list for the standings data
standings_list = []

# Loop through the end dates in season_end object
for date in season_end:
    try: 
        # Query the NHL API to get standings by the end date of the season
        url = f"https://api-web.nhle.com/v1/standings/{date}"
        response = requests.get(url)
        response.raise_for_status()
        data = response.json()
        # Create a DataFrame object containing the standings on that date
        df = pd.DataFrame(data['standings'])
        # Append the DataFrame to the list
        standings_list.append(df)
    
        logging.info(f"Successful for {date}")
    except Exception as e: 
        logging.error(f"Error for {date}")

# Create one large DataFrame object containing the standing year by year
standings_df = pd.concat(standings_list)
# Export the standings data to a csv file
# standings_df.to_csv('data/raw/standings_03_15.csv', index=False)