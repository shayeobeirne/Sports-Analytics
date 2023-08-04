import pandas as pd
import nba_api
from nba_api.stats.endpoints import leaguedashplayerstats
from nba_api.stats.endpoints import leaguedashlineups

def get_player_data():

  player_data = leaguedashplayerstats.LeagueDashPlayerStats()
  player_df = player_data.get_data_frames()[0]
  return player_df

get_player_data()

def get_lineup_data():

  lineup_data = leaguedashlineups.LeagueDashLineups(group_quantity = 5)
  lineup_df = lineup_data.get_data_frames()[0]
  return lineup_df

get_lineup_data()
