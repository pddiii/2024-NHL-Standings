library(tidyverse)
library(tidymodels)
library(xgboost)
library(doParallel)
library(vip)
library(caret)
library(Boruta)
library(car)
library(randomForest)
library(hockeyR)
# load in the mid_season and end_season data files
mid_season <- read_csv("data/model/mid_season.csv")
mid_24 <- read_csv("data/model/mid_2024.csv")
end_season <- read_csv("data/model/end_season.csv")
end_24 <- read_csv("data/model/end_2024.csv")
game_ids <- 
  read_csv("data/raw/game_ids.csv") %>% 
  distinct(game_id, .keep_all = TRUE)

# date <- as.Date(game_ids$date, format = "%m/%d/%y")
# 
# date_years <- year(date)
# 
# date_years <- 
#   ifelse(date_years == 2067, 1967, 
#        ifelse(date_years == 2068, 1968, date_years))
# 
# mm_dd <- str_extract_all(date, "-\\d\\d-\\d\\d") %>% unlist()
# 
# game_ids$date <- str_c(date_years, mm_dd, sep = "")
# 
# rm(date, date_years, mm_dd)
# 
# reg <-
#   game_ids %>% 
#   filter(game_type == "REG")
#   
# home <- 
#   reg %>% 
#   filter(home_abbr == "BOS") %>% 
#   group_by(season_full) %>% 
#   select(season_full, date, home_abbr, game_played) %>% 
#   ungroup() %>% 
#   rename(team = home_abbr)
#   
# away <- 
#   reg %>% 
#   filter(away_abbr == "BOS") %>% 
#   group_by(season_full) %>% 
#   select(season_full, date, away_abbr, game_played) %>% 
#   ungroup() %>% 
#   rename(team = away_abbr)
# 
# full <- bind_rows(home, away)
# 
# games_data <- 
#   full %>% 
#   arrange(date) %>% 
#   group_by(season_full) %>% 
#   mutate(season_games = cumsum(game_played))
# 
# num_games_season <- 
#   games_data %>% 
#   rename(total_games = season_games) %>% 
#   dplyr::slice(n()) %>% 
#   ungroup() %>% 
#   mutate(season_games = total_games - 15)
# 
# mid_marks <- 
#   games_data %>% inner_join(num_games_season %>% 
#                             select(-date, -team, -game_played), 
#                           by = c("season_full", "season_games"))
# 
# dates <- 
#   mid_marks %>% 
#   ungroup() %>% 
#   filter(season_full != 20122013) %>% 
#   select(date)

# write_csv(dates, "data/raw/dates.csv")

### Data Collection process for the full season schedules 
### Utilized hockeyR

# years <- list("sub1" = 1968:1981,
#               "sub2" = 1982:1994,
#               "sub3" = c(1996:2004, 2006:2010),
#               "sub4" = c(2011:2012, 2014:2020, 2022:2024))
# data <- list()
# 
# 
# for (seasons in years[["sub1"]]) {
#   data[[as.character(seasons)]] <- hockeyR::get_game_ids(season = seasons)
#   }
# for (seasons in years[["sub2"]]) {
#     data[[as.character(seasons)]] <- hockeyR::get_game_ids(season = seasons)
#   }
# for (seasons in years[["sub3"]]) {
#     data[[as.character(seasons)]] <- hockeyR::get_game_ids(season = seasons)
#   }
# for (seasons in years[["sub4"]]) {
#     data[[as.character(seasons)]] <- hockeyR::get_game_ids(season = seasons)
#   }



nineties <- unique(mid_season$seasonId)[1:5]
two_thousands <- unique(mid_season$seasonId)[6:14]
twenty10s <- unique(mid_season$seasonId)[15:23]
twenty20s <- unique(mid_season$seasonId)[24:25]
# Load in the teams stats at the "middle" of the season (around March 15th)
mid_season <- 
  mid_season %>% 
  replace_na(list(clinchIndicator = "none")) %>%
  relocate(c(team_name, seasonId), .after = clinchIndicator) %>%  
  mutate(clinchIndicator = factor(clinchIndicator),
         decade = factor(ifelse(seasonId %in% nineties, "1990s", 
                                ifelse(seasonId %in% two_thousands, "2000s",
                                       ifelse(seasonId %in% twenty10s, "2010s",
                                              "2020s"
                                       ) 
                                ) 
         ), 
         levels = c("1990s", "2000s", "2010s", "2020s") 
         )
  )

# Load in the 2023-2024 team stats as of 03-15-2024
mid_24 <- 
  mid_24 %>% 
  mutate(clinchIndicator = as.character(clinchIndicator)) %>% 
  replace_na(list(clinchIndicator = "none")) %>%
  relocate(c(team_name, seasonId), .after = clinchIndicator) %>%  
  mutate(clinchIndicator = factor(clinchIndicator),
         decade = factor(ifelse(seasonId %in% nineties, "1990s", 
                                ifelse(seasonId %in% two_thousands, "2000s",
                                       ifelse(seasonId %in% twenty10s, "2010s",
                                              "2020s"
                                       ) 
                                ) 
         ), 
         levels = c("1990s", "2000s", "2010s", "2020s") 
         )
  )

# Load in the end season stats for 1995-1996 to 2022-2023
end_season <-
  end_season %>% 
  replace_na(list(clinchIndicator = "e")) %>% 
  relocate(c(team_name, seasonId), .after = clinchIndicator) %>%  
  mutate(clinchIndicator = factor(clinchIndicator),
         decade = factor(ifelse(seasonId %in% nineties, "1990s", 
                                ifelse(seasonId %in% two_thousands, "2000s",
                                       ifelse(seasonId %in% twenty10s, "2010s",
                                              "2020s"
                                       ) 
                                ) 
         ), 
         levels = c("1990s", "2000s", "2010s", "2020s") 
         )
  )

# subset the data for the model
data <- 
  mid_season %>% 
  select(-c(teamCommonName, teamAbbrev, team_name, seasonId, placeName, 
            streakCode, streakCount, l10GamesPlayed, goalDifferentialPctg,
            goalsForPctg, homeGoalDifferential, homeGamesPlayed, 
            roadGamesPlayed, regulationPlusOtWinPctg, regulationWinPctg, 
            homePoints, roadPoints, homeGoalsAgainst, roadGoalsAgainst,
            homeGoalsFor, roadGoalsFor, l10GoalsAgainst, l10GoalsFor,
            roadGoalDifferential, winPctg, pointPctg, roadLosses, 
            homeLosses, homeOtLosses, goalFor, goalAgainst, roadOtLosses),
         -contains("wins"))

# subset the 2023-2024 to match up with the above data
mid_pred <- 
  mid_24 %>% 
  select(-c(teamCommonName, teamAbbrev, team_name, seasonId, placeName, 
            streakCode, streakCount, l10GamesPlayed, goalDifferentialPctg,
            goalsForPctg, homeGoalDifferential, homeGamesPlayed, 
            roadGamesPlayed, regulationPlusOtWinPctg, regulationWinPctg, 
            homePoints, roadPoints, homeGoalsAgainst, roadGoalsAgainst,
            homeGoalsFor, roadGoalsFor, l10GoalsAgainst, l10GoalsFor,
            roadGoalDifferential, winPctg, pointPctg, roadLosses, 
            homeLosses, homeOtLosses, goalFor, goalAgainst, roadOtLosses),
         -contains("wins"))

# Correlation matrix
cor_mat <- cor(data %>% select_if(is.numeric) %>% select(-end_season_points))
# melted matrix for ggplot2
melted_cor_mat <- reshape2::melt(cor_mat)
# correlation heatmap
cor_plot <- 
  ggplot(melted_cor_mat, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(x = "", y = "", title = "Heat Map of Model Variables") +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(face = "bold"))

# Boruta feautre importance algorithm
# 4.23 secs
# all attributes confirmed important
# set.seed(1)
# boruta <- Boruta(end_season_points ~ ., data = data,
#                  maxRuns = 1000)
# 
# # Extract the feature importance attributes
# boruta_feat_imp <- data.frame(attStats(boruta))
# boruta_feat_imp <- boruta_feat_imp %>%
#   arrange(desc(meanImp)) %>%
#   select(-normHits)
# # Select only the confirmed features of the mid_preded variables
# boruta_feat <- getSelectedAttributes(boruta)
# paste0(boruta_feat, collapse = " + ")

# Linear regression model
lm_model <-
  lm(end_season_points ~ clinchIndicator + gamesPlayed + goalDifferential +
       l10GoalDifferential + l10Losses + l10OtLosses + l10Points +
       losses + otLosses + points + shootoutLosses + wildcardSequence +
       decade, data = data)

# Summary of the linear regression (not great model)
lm_summary <- summary(lm_model)

# update the lm_model to not have outliers
lm_model <-
  lm(end_season_points ~ clinchIndicator + gamesPlayed + goalDifferential +
       l10GoalDifferential + l10Losses + l10OtLosses + l10Points +
       losses + otLosses + points + shootoutLosses + wildcardSequence +
       decade, data = data)
# improvement in model, but still not great
lm_summary <- summary(lm_model)
# make some predictions anyways
lm_predictions <-
  lm_model %>%
  predict(mid_pred)

# Create the recipe for the models we will create
model_recipe <- 
  recipe(end_season_points ~ clinchIndicator + gamesPlayed + goalDifferential + 
           l10GoalDifferential + l10Losses + l10OtLosses + l10Points + 
           losses + otLosses + points + shootoutLosses + wildcardSequence + 
           decade, data = data) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

set.seed(1)
folds <- vfold_cv(data = data, v = 10)

# knn_model <-
#   nearest_neighbor(neighbors = 10,
#                    dist_power = 1.108175512757,
#                    weight_func = "triangular") %>% 
#   set_mode("regression") %>% 
#   set_engine("kknn")
# 
# knn_wf <-
#   workflow() %>% 
#   add_model(knn_model) %>% 
#   add_recipe(model_recipe)

# knn_grid <- grid_latin_hypercube(
#   neighbors(range = c(1, 10)),
#   dist_power(range = c(1, 2)),
#   dials::weight_func(),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# knn_tune_res <- tune_grid(
#   knn_wf,
#   resamples = folds,
#   grid = knn_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(knn_tune_res, "rmse")[, 1:3]
# 
# knn_model <-
#   finalize_model(knn_model, best_params)
# 
# knn_wf <-
#   workflow() %>% 
#   add_model(knn_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# knn_crossval <-
#   knn_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# knn_crossval_metrics <-
#   knn_crossval %>% 
#   collect_metrics()
# 
# knn_fit <-
#   knn_wf %>% 
#   fit(data = data)
# 
# knn_predictions <-
#   knn_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# create a polynomial support vector machine (SVM) model 
# svm_model <-
#   svm_poly(degree = 2.0278376044364,
#            cost = 1.9291764687123,
#            scale_factor = 0.0272485016815718,
#            margin = 0.0874848593806538) %>% 
#   set_engine("kernlab") %>% 
#   set_mode("regression")
# 
# svm_wf <-
#   workflow() %>% 
#   add_model(svm_model) %>% 
#   add_recipe(model_recipe)

# svm_grid <- grid_latin_hypercube(
#   degree(range = c(1, 3)),
#   cost(range = c(-10, 5)),
#   scale_factor(range = c(-10, -1)),
#   dials::svm_margin(range = c(0, 0.2)),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# svm_tune_res <- tune_grid(
#   svm_wf,
#   resamples = folds,
#   grid = svm_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(svm_tune_res, "rmse")[, 1:4]
# 
# svm_model <-
#   finalize_model(svm_model, best_params)
# 
# svm_wf <-
#   workflow() %>% 
#   add_model(svm_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# svm_crossval <-
#   svm_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# svm_crossval_metrics <-
#   svm_crossval %>% 
#   collect_metrics()
# 
# svm_fit <-
#   svm_wf %>% 
#   fit(data = data)

# SVM model cannot make a numerical prediction on the San Jose Sharks
# not sure why but only observation in the predictions where a NaN is produced
# svm_predictions <-
#   svm_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# Random Forest Model

# rf_model <-
#   rand_forest(mtry = 9,
#               trees = 149,
#               min_n = 7) %>% 
#   set_engine("ranger", importance = "impurity") %>% 
#   set_mode("regression")
# 
# rf_wf <-
#   workflow() %>% 
#   add_model(rf_model) %>% 
#   add_recipe(model_recipe)

# rf_grid <- grid_latin_hypercube(
#   mtry(range = c(5, 15)),
#   trees(range = c(50, 150)),
#   min_n(c(5, 15)),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# set.seed(1)
# rf_tune_res <- tune_grid(
#   rf_wf,
#   resamples = folds,
#   grid = rf_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(rf_tune_res, "rmse")[, 1:3]
# 
# rf_model <-
#   finalize_model(rf_model, best_params)
# 
# rf_wf <-
#   workflow() %>% 
#   add_model(rf_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# rf_crossval <-
#   rf_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# rf_crossval_metrics <-
#   rf_crossval %>% 
#   collect_metrics()
# 
# rf_fit <-
#   rf_wf %>% 
#   fit(data = data)
# 
# rf_predictions <-
#   rf_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# xgboost_model
# boost_model <-
#   boost_tree(mtry = 11,
#              trees = 127,
#              min_n = 6,
#              tree_depth = 6,
#              learn_rate = 0.0433970914939939,
#              loss_reduction = 0.12363789646074,
#              sample_size = c(1),
#              stop_iter = c(5)) %>% 
#   set_engine("xgboost") %>% 
#   set_mode("regression")
# 
# boost_wf <-
#   workflow() %>% 
#   add_model(boost_model) %>% 
#   add_recipe(model_recipe)

# boost_grid <- grid_latin_hypercube(
#   mtry(range = c(5, 15)),
#   trees(range = c(50, 150)),
#   min_n(range = c(5, 15)),
#   tree_depth(range = c(5, 15)), 
#   learn_rate(range = c(-10, -1), trans = transform_log10()),
#   loss_reduction(range = c(-10, 1.5), trans = transform_log10()),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# set.seed(1)
# boost_tune_res <- tune_grid(
#   boost_wf,
#   resamples = folds,
#   grid = boost_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(boost_tune_res, "rmse")[, 1:6]
# 
# boost_model <-
#   finalize_model(boost_model, best_params)

# boost_wf <-
#   workflow() %>% 
#   add_model(boost_model) %>% 
#   add_recipe(model_recipe)
# 
# set.seed(1)
# boost_crossval <-
#   boost_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# boost_crossval_metrics <-
#   boost_crossval %>% 
#   collect_metrics()
# 
# boost_fit <-
#   boost_wf %>% 
#   fit(data = data)
# 
# boost_predictions <-
#   boost_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# mlp_model
# mlp_model <-
#   mlp(hidden_units = 4,
#       penalty = 0.0336536122368586,
#       epochs = 194) %>% 
#   set_engine("nnet") %>% 
#   set_mode("regression")
# 
# mlp_wf <-
#   workflow() %>% 
#   add_model(mlp_model) %>% 
#   add_recipe(model_recipe)

# mlp_grid <- grid_latin_hypercube(
#   hidden_units(range = c(1, 10)),
#   penalty(range = c(-10, 0), trans = transform_log10()),
#   epochs(range = c(100, 250)),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# set.seed(1)
# mlp_tune_res <- tune_grid(
#   mlp_wf,
#   resamples = folds,
#   grid = mlp_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(mlp_tune_res, "rmse")[, 1:3]
# 
# mlp_model <-
#   finalize_model(mlp_model, best_params)
# 
# mlp_wf <-
#   workflow() %>% 
#   add_model(mlp_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# mlp_crossval <-
#   mlp_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# mlp_crossval_metrics <-
#   mlp_crossval %>% 
#   collect_metrics()
# 
# mlp_fit <-
#   mlp_wf %>% 
#   fit(data = data)
# 
# mlp_predictions <-
#   mlp_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# Create a dataframe containing the model cross-validation metrics
# model_metrics <- 
#   rbind(knn_crossval_metrics, svm_crossval_metrics, rf_crossval_metrics, boost_crossval_metrics, mlp_crossval_metrics) %>% 
#   select(-.config) %>% 
#   mutate(model = c(rep("KNN", 3), rep("SVM", 3), rep("Rand. Forest", 3), 
#                    rep("Boosted Tree", 3), rep("MLP", 3) ) )
# 
# model_predictions <- cbind(lm_predictions, knn_predictions, svm_predictions, 
#                            rf_predictions, boost_predictions, mlp_predictions)
# colnames(model_predictions) <- c("Linear Reg.", "KNN", "SVM", "Rand. Forest", 
#                                  "Boosted Tree", "MLP")
# 
# model_by_rmse <- 
#   model_metrics %>% 
#   filter(.metric == "rmse") %>% 
#   arrange(mean)
# 
# model_by_ccc <-
#   model_metrics %>% 
#   filter(.metric == "ccc") %>% 
#   arrange(desc(mean))

### Model with outliers removed

# Find outliers using standardized residuals
outliers <- which(abs(scale(lm_model$residuals)) >= 2.5)
# # update the lm_model to not have outliers
# lm_model <- 
#   lm(end_season_points ~ clinchIndicator + gamesPlayed + goalDifferential + 
#        l10GoalDifferential + l10Losses + l10OtLosses + l10Points + 
#        losses + otLosses + points + shootoutLosses + wildcardSequence + 
#        decade, data = data[-outliers, ])
# # improvement in model, but still not great
# lm_summary <- summary(lm_model)
# # make some predictions anyways
# lm_predictions <-
#   lm_model %>% 
#   predict(mid_pred)

# Create the recipe for the models we will create
model_recipe <- 
  recipe(end_season_points ~ clinchIndicator + gamesPlayed + goalDifferential + 
           l10GoalDifferential + l10Losses + l10OtLosses + l10Points + 
           losses + otLosses + points + shootoutLosses + wildcardSequence + 
           decade, data = data[-outliers, ]) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

set.seed(1)
folds <- vfold_cv(data = data[-outliers, ], v = 10)

# knn_model <-
#   nearest_neighbor(neighbors = 10,
#                    dist_power = 1.108175512757,
#                    weight_func = "triangular") %>% 
#   set_mode("regression") %>% 
#   set_engine("kknn")
# 
# knn_wf <-
#   workflow() %>% 
#   add_model(knn_model) %>% 
#   add_recipe(model_recipe)

# knn_grid <- grid_latin_hypercube(
#   neighbors(range = c(1, 10)),
#   dist_power(range = c(1, 2)),
#   dials::weight_func(),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# knn_tune_res <- tune_grid(
#   knn_wf,
#   resamples = folds,
#   grid = knn_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(knn_tune_res, "rmse")[, 1:3]
# 
# knn_model <-
#   finalize_model(knn_model, best_params)
# 
# knn_wf <-
#   workflow() %>% 
#   add_model(knn_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# knn_crossval <-
#   knn_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# knn_crossval_metrics <-
#   knn_crossval %>% 
#   collect_metrics()
# 
# knn_fit <-
#   knn_wf %>% 
#   fit(data = data[-outliers, ])
# 
# knn_predictions <-
#   knn_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# create a polynomial support vector machine (SVM) model 
# svm_model <-
#   svm_poly(degree = 2.0278376044364,
#            cost = 1.9291764687123,
#            scale_factor = 0.0272485016815718,
#            margin = 0.0874848593806538) %>% 
#   set_engine("kernlab") %>% 
#   set_mode("regression")
# 
# svm_wf <-
#   workflow() %>% 
#   add_model(svm_model) %>% 
#   add_recipe(model_recipe)

# svm_grid <- grid_latin_hypercube(
#   degree(range = c(1, 3)),
#   cost(range = c(-10, 5)),
#   scale_factor(range = c(-10, -1)),
#   dials::svm_margin(range = c(0, 0.2)),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# svm_tune_res <- tune_grid(
#   svm_wf,
#   resamples = folds,
#   grid = svm_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(svm_tune_res, "rmse")[, 1:4]
# 
# svm_model <-
#   finalize_model(svm_model, best_params)
# 
# svm_wf <-
#   workflow() %>% 
#   add_model(svm_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# svm_crossval <-
#   svm_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# svm_crossval_metrics <-
#   svm_crossval %>% 
#   collect_metrics()
# 
# svm_fit <-
#   svm_wf %>% 
#   fit(data = data[-outliers, ])

# SVM model cannot make a numerical prediction on the San Jose Sharks
# not sure why but only observation in the predictions where a NaN is produced
# svm_predictions <-
#   svm_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# Random Forest Model

# rf_model <-
#   rand_forest(mtry = 9,
#               trees = 149,
#               min_n = 7) %>% 
#   set_engine("ranger", importance = "impurity") %>% 
#   set_mode("regression")
# 
# rf_wf <-
#   workflow() %>% 
#   add_model(rf_model) %>% 
#   add_recipe(model_recipe)

# rf_grid <- grid_latin_hypercube(
#   mtry(range = c(5, 15)),
#   trees(range = c(50, 150)),
#   min_n(c(5, 15)),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# set.seed(1)
# rf_tune_res <- tune_grid(
#   rf_wf,
#   resamples = folds,
#   grid = rf_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(rf_tune_res, "rmse")[, 1:3]
# 
# rf_model <-
#   finalize_model(rf_model, best_params)
# 
# rf_wf <-
#   workflow() %>% 
#   add_model(rf_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# rf_crossval <-
#   rf_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# rf_crossval_metrics <-
#   rf_crossval %>% 
#   collect_metrics()
# 
# rf_fit <-
#   rf_wf %>% 
#   fit(data = data[-outliers, ])
# 
# rf_predictions <-
#   rf_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# xgboost_model
boost_model <-
  boost_tree(mtry = 11,
             trees = 127,
             min_n = 6,
             tree_depth = 6,
             learn_rate = 0.0433970914939939,
             loss_reduction = 0.12363789646074,
             sample_size = c(1),
             stop_iter = c(5)) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

boost_wf <-
  workflow() %>% 
  add_model(boost_model) %>% 
  add_recipe(model_recipe)

boost_grid <- grid_latin_hypercube(
  mtry(range = c(5, 15)),
  trees(range = c(50, 150)),
  min_n(range = c(5, 15)),
  tree_depth(range = c(5, 15)),
  learn_rate(range = c(-10, -1), trans = transform_log10()),
  loss_reduction(range = c(-10, 1.5), trans = transform_log10()),
  size = 1500
)
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# set.seed(1)
# boost_tune_res <- tune_grid(
#   boost_wf,
#   resamples = folds,
#   grid = boost_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(boost_tune_res, "rmse")[, 1:6]
# 
# boost_model <-
#   finalize_model(boost_model, best_params)

boost_wf <-
  workflow() %>% 
  add_model(boost_model) %>% 
  add_recipe(model_recipe)

set.seed(1)
boost_crossval <-
  boost_wf %>% 
  fit_resamples(resamples = folds,
                metrics = reg_metrics)

boost_crossval_metrics <-
  boost_crossval %>% 
  collect_metrics()

boost_fit <-
  boost_wf %>% 
  fit(data = data[-outliers, ])

boost_predictions <-
  boost_fit %>% 
  predict(new_data = mid_pred) %>% 
  rename(estimate = .pred)

# stopCluster(cl)

# mlp_model
# mlp_model <-
#   mlp(hidden_units = 4,
#       penalty = 0.0336536122368586,
#       epochs = 194) %>% 
#   set_engine("nnet") %>% 
#   set_mode("regression")
# 
# mlp_wf <-
#   workflow() %>% 
#   add_model(mlp_model) %>% 
#   add_recipe(model_recipe)

# mlp_grid <- grid_latin_hypercube(
#   hidden_units(range = c(1, 10)),
#   penalty(range = c(-10, 0), trans = transform_log10()),
#   epochs(range = c(100, 250)),
#   size = 1500
# )
# 
# # Begin Parallel Processing
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# 
# reg_metrics <- metric_set(rmse, ccc, rsq)
# 
# set.seed(1)
# mlp_tune_res <- tune_grid(
#   mlp_wf,
#   resamples = folds,
#   grid = mlp_grid,
#   control = control_grid(save_pred = TRUE),
#   metrics = reg_metrics
# )
# 
# best_params <- select_best(mlp_tune_res, "rmse")[, 1:3]
# 
# mlp_model <-
#   finalize_model(mlp_model, best_params)
# 
# mlp_wf <-
#   workflow() %>% 
#   add_model(mlp_model) %>% 
#   add_recipe(model_recipe)

# set.seed(1)
# mlp_crossval <-
#   mlp_wf %>% 
#   fit_resamples(resamples = folds,
#                 metrics = reg_metrics)
# 
# mlp_crossval_metrics <-
#   mlp_crossval %>% 
#   collect_metrics()
# 
# mlp_fit <-
#   mlp_wf %>% 
#   fit(data = data[-outliers, ])
# 
# mlp_predictions <-
#   mlp_fit %>% 
#   predict(new_data = mid_pred) %>% 
#   rename(estimate = .pred)

# stopCluster(cl)

# Create a dataframe containing the model cross-validation metrics
# model_metrics_no_outliers <- 
#   rbind(knn_crossval_metrics, svm_crossval_metrics, rf_crossval_metrics, boost_crossval_metrics, mlp_crossval_metrics) %>% 
#   select(-.config) %>% 
#   mutate(model = c(rep("KNN (no outliers)", 3), rep("SVM (no outliers)", 3), 
#                    rep("Rand. Forest (no outliers)", 3), 
#                    rep("Boosted Tree (no outliers)", 3), 
#                    rep("MLP (no outliers)", 3) ) )
# 
# model_predictions_no_outliers <- cbind(lm_predictions, knn_predictions, svm_predictions, 
#                                        rf_predictions, boost_predictions, mlp_predictions)
# colnames(model_predictions_no_outliers) <- 
#   c("Linear Reg. (no outliers)", "KNN (no outliers)", "SVM (no outliers)", 
#     "Rand. Forest (no outliers)", "Boosted Tree (no outliers)", 
#     "MLP (no outliers)")
# 
# model_w_no_outliers_rmse <- 
#   model_metrics_no_outliers %>% 
#   filter(.metric == "rmse") %>% 
#   arrange(mean)
# 
# model_w_no_outliers_ccc <- 
#   model_metrics_no_outliers %>% 
#   filter(.metric == "ccc") %>% 
#   arrange(desc(mean))

## From model metrics above I have decided to use the boosted model trained
## without outliers. The models performance improvement in both RMSE and CCC
## were satisfactory enough that I find it worth while.
## SVM had better metrics in both regards, but both versions of the model
## predicted an NaN value for the 2023-2024 San Jose Sharks, and thus I cannot
## precede with these predictions.

# Add the boosted tree model predictions to the `mid_24` object as a
# whole number
# mid_24 <-
#   mid_24 %>%
#   mutate(
#     end_season_points = round(
#       model_predictions_no_outliers$`Boosted Tree (no outliers)`, digits = 0)
#     )

# Add the 2023-2024 season data back to the `mid_season` object for championship
# predictions
# mid_season <- rbind(mid_season, mid_24)

vip_plot <-
  boost_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point") + 
  labs(title = "Variable Importance Plot") +
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 10))

rm(cor_mat, data, end_24, end_season, folds, mid_24, mid_season, 
   mid_pred, melted_cor_mat, boost_predictions, boost_crossval, nineties,
   twenty10s, twenty20s, two_thousands, outliers)

# rm_objects <- ls()[c(1:7, 13:20, 22, 26:31, 42:55)]
# rm(list = rm_objects)

# ggsave("data/model/end_season_points/plots/cor_plot.png", plot = cor_plot)
# ggsave("data/model/end_season_points/plots/vip_plot.png", plot = vip_plot)
# write_csv(mid_season, "data/model/end_season_points/mid_season_w_preds.csv")
# write_csv(model_metrics, "data/model/end_season_points/model_metrics.csv")
# write_csv(model_metrics_no_outliers,
#           "data/model/end_season_points/model_metrics_no_outliers.csv")
# write_csv(model_predictions, 
#           "data/model/end_season_points/model_predictions.csv")
# write_csv(model_predictions_no_outliers,
#           "data/model/end_season_points/model_predictions_no_outliers.csv")