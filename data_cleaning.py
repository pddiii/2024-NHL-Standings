import pandas as pd

# load the end of season standings data
end_season = pd.read_csv('data/raw/standings_1980_2024.csv')
# rename the points column for joining to mid_season
end_season = end_season.rename(columns={'points': 'end_season_points'})
end_season['seasonId'] = end_season['seasonId'].astype(str)
# load the season data as of March 15th (03-15)
mid_season = pd.read_csv('data/raw/standings_03_15.csv')
# convert the seasonId to a string
mid_season['seasonId'] = mid_season['seasonId'].astype(str)
# Create an object for the seasons we want
seasons_sub = [str(year1) + str(year2) for year1, year2 in zip(range(1995, 2023), range(1996, 2024))]
seasons_remove = ['20042005', '20122013', '20202021']
seasons_sub = [season for season in seasons_sub if season not in seasons_remove]
# add the end_season_points column to the mid_season_data information
mid_season = mid_season.merge(end_season[['team_name', 'end_season_points', 'seasonId']], on = ['team_name', 'seasonId'])

columns_desired = ['clinchIndicator', 'gamesPlayed', 'goalDifferential',
       'goalDifferentialPctg', 'goalAgainst', 'goalFor', 'goalsForPctg',
       'homeGamesPlayed', 'homeGoalDifferential', 'homeGoalsAgainst',
       'homeGoalsFor', 'homeLosses', 'homeOtLosses', 'homePoints',
       'homeRegulationPlusOtWins', 'homeRegulationWins', 
       'homeWins', 'l10GamesPlayed', 'l10GoalDifferential', 'l10GoalsAgainst',
       'l10GoalsFor', 'l10Losses', 'l10OtLosses', 'l10Points',
       'l10RegulationPlusOtWins', 'l10RegulationWins', 'l10Wins', 'losses', 'otLosses', 
       'placeName', 'pointPctg',
       'points', 'regulationPlusOtWinPctg', 'regulationPlusOtWins',
       'regulationWinPctg', 'regulationWins', 'roadGamesPlayed',
       'roadGoalDifferential', 'roadGoalsAgainst', 'roadGoalsFor',
       'roadLosses', 'roadOtLosses', 'roadPoints', 'roadRegulationPlusOtWins',
       'roadRegulationWins', 'roadWins', 'seasonId',
       'shootoutLosses', 'shootoutWins', 'streakCode', 'streakCount',
       'teamCommonName', 'teamAbbrev', 'wildcardSequence', 'winPctg', 'wins', 'team_name',
       'end_season_points']

# Subset the mid_season data to only include the listed columns above
mid_season = mid_season[columns_desired]
# Isolate the mid_season data for 2023-2024 season
# will make end_season_points estimation on this data
mid_2024 = mid_season.query('seasonId == "20232024"').drop('end_season_points', axis = 1)
# remove the 2024 team data from mid_season (we will use this as our prediction data)
mid_season = mid_season[mid_season['seasonId'].isin(seasons_sub)]
# Load the stanley_cup data
stanley_cup = pd.read_csv('data/raw/stanley_cup_1980_2023.csv')
seasons = [str(year1) + str(year2) for year1, year2 in zip(range(1979, 2023), range(1980, 2024))]
# remove the 20042005 season and 20122013 season
seasons.remove('20042005')
stanley_cup['seasonId'] = seasons
# remove unnecessary columns from the stanley_cup data
stanley_cup = stanley_cup.drop(['season', 'runner_up', 'series_length'], axis = 1)
# merge the stanley_cup data with the end of season data
end_season = pd.merge(end_season, stanley_cup, how='left', left_on=['seasonId', 'team_name'], right_on=['seasonId', 'champion'])
# mutate the champion column to a binary indicator
end_season['champion'] = end_season['champion'].apply(lambda x: 1 if pd.notnull(x) else 0)
# remove the points column from the columns_desired list (doesn't exist in end_season)
columns_desired.remove('points')
# add the champion column to the columns_desired list
columns_desired.append('champion')
# update end_season to only include the columns_desired
end_season = end_season[columns_desired]
# isolate the 2023-2024 season data to make championship prediction on
end_2024 = end_season.query('seasonId == "20232024"').drop('champion', axis = 1)
# subset for only the desired seasons
end_season = end_season[end_season['seasonId'].isin(seasons_sub)]

# Write csv files for creating the model
# mid_season.to_csv('data/model/mid_season.csv', index=False)
# mid_2024.to_csv('data/model/mid_2024.csv', index=False)
# end_season.to_csv('data/model/end_season.csv', index=False)
# end_2024.to_csv('data/model/end_2024.csv', index=False)