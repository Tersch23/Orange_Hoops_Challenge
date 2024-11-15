# import libraries
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder, MinMaxScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, classification_report
import numpy as np
import os
import matplotlib.pyplot as plt
import joblib
# set working directory
os.chdir()

# bring data in
file_path = 'pbp_updated.csv'  # Update with the correct file path
data = pd.read_csv(file_path)
file_path = 'opp_shot_split.csv'  # Update with the correct file path
opp_shot_split = pd.read_csv(file_path)
file_path = 'opp_season.csv'  # Update with the correct file path
opp_season = pd.read_csv(file_path)
file_path = 'shooter_game.csv'  # Update with the correct file path
shooter_game = pd.read_csv(file_path)
file_path = 'shooter_season.csv'  # Update with the correct file path
shooter_season = pd.read_csv(file_path)
file_path = 'shooting_split.csv'  # Update with the correct file path
shooting_split = pd.read_csv(file_path)

# clean and merge data
opp_shot_split = opp_shot_split[['opponent', 'nonclutch_eFG', 'clutch_eFG', 'p_val']]
opp_shot_split.rename({'nonclutch_eFG':'opp_nonclutch_eFG', 'clutch_eFG':'opp_clutch_eFG', 'p_val':'opp_p_val'})
opp_season = opp_season[['opponent', 'opp_season_clutch_rate', 'opp_season_wpa', 'opp_season_3P_pct', 'opp_season_2P_pct']]
shooter_season = shooter_season[['shooter', 'shot_team', 'season_wpa', 'season_clutch_rate', 'season_3P_pct', 'season_2P_pct', 'season_3PA', 'season_2PA', 'season_eFG', 'season_FTr']]
shooting_split = shooting_split[['shooter', 'shot_team', 'nonclutch_eFG', 'clutch_eFG', 'p_val']]
data = pd.merge(data, opp_shot_split, on=['opponent'], how='left')
data = pd.merge(data, opp_season, on=['opponent'], how ='left')
data = pd.merge(data, shooter_season, on=['shooter', 'shot_team'], how = "left")
data = pd.merge(data, shooting_split, on=['shooter', 'shot_team'], how = "left")

# Standardize player names to ensure consistency (e.g., Jr. Kyle Cuff vs Kyle Cuffe Jr.)
name_mapping = {
    'Jr. Kyle Cuff': 'Kyle Cuffe Jr.'
}
data['shooter'] = data['shooter'].replace(name_mapping)

# Filter column selection
selected_columns = [
    'game_id', 'secs_remaining', 'action_team', 'shot_type',
    'shooter', 'shot_team', 'play_length', 'shot_outcome', 'score_diff',
    'home_score', 'away_score', 'prev_naive_wp',
    'opponent', 'clutch', 'potential_wpa', 'opp_season_clutch_rate',
    'opp_season_3P_pct', 'opp_season_2P_pct', 'season_wpa',
    'season_clutch_rate', 'season_3P_pct', 'season_2P_pct',
    'season_eFG', 'p_val_x', 'p_val_y', 'season_3PA', 'season_2PA',
    'home', 'away'
]
model_data = data[selected_columns].copy()

# Adjust score differential to be relative to shooting team
model_data['score_differential'] = model_data.apply(
    lambda row: row['home_score'] - row['away_score'] if row['shot_team'] == row['home'] else row['away_score'] - row['home_score'],
    axis=1
)

# Encode categorical variables to numeric values
outcome_encoder = LabelEncoder()
model_data['shot_outcome_encoded'] = outcome_encoder.fit_transform(model_data['shot_outcome'])

# Filter out all non shot attempts, encode home/away to numerical dummy
model_data = model_data.dropna()
model_data['action_team'] = model_data['action_team'].replace({'home': 1, 'away': 0})

# Select Model Data
X = model_data[
    ['secs_remaining', 'action_team', 'shot_type',
     'score_differential', 'play_length', 'prev_naive_wp', 'potential_wpa', 'clutch',
     'opp_season_clutch_rate', 'opp_season_3P_pct', 'opp_season_2P_pct', 'season_clutch_rate',
     'season_3P_pct', 'season_2P_pct', 'season_eFG', 'p_val_x', 'p_val_y', 'season_3PA',
     'season_2PA']
]
y = model_data['shot_outcome_encoded']

# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=42
)

# Train RandomForestClassifier
rf_expanded_model = RandomForestClassifier(random_state=42)
rf_expanded_model.fit(X_train, y_train)

# Test Model Accuracy: Make is Class 0, Miss is Class 1
probabilities = rf_expanded_model.predict_proba(X_test)
print(classification_report(y_test, rf_expanded_model.predict(X_test), target_names=['Make', 'Miss']))
# Export Model for Power BI
joblib.dump(rf_expanded_model, 'random_forest_model.pkl')

# Get the feature importances from the trained model
importances = rf_expanded_model.feature_importances_

# Create a DataFrame for easy visualization
feature_names = X_train.columns  # If X_train is a DataFrame
importance_df = pd.DataFrame({'Feature': feature_names, 'Importance': importances})

# Sort the importance values in descending order
importance_df = importance_df.sort_values(by='Importance', ascending=False)

# Plotting the feature importances as a horizontal bar chart
plt.figure(figsize=(10, 6))
plt.barh(importance_df['Feature'], importance_df['Importance'], color='skyblue')
plt.xlabel('Importance')
plt.ylabel('Features')
plt.title('Feature Importance in Random Forest Model')
plt.gca().invert_yaxis()  # To display the most important features at the top
plt.show()