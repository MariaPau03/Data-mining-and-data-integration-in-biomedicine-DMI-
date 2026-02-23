# install.packages("recipes")
# install.packages("rpart")
# install.packages("caret")
# install.packages("rpart.plot")
library(rpart.plot)
library(caret)
library(rpart)
library(recipes)
library(dplyr)

proteomic_recipe <- recipe(group ~ ., data = df_metdata) %>%
  # Tell R ms_id_b is just a label, not a predictor
  update_role(ms_id_b, new_role = "id") %>% 
  # Handle the 'sex' variable by turning it into a 0 or 1
  step_dummy(all_nominal_predictors()) %>%
  # Fill in any missing values (KNN cannot handle NAs)
  step_impute_median(all_numeric_predictors()) %>%
  # ONLY scale and center the numeric columns
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

ctrl <- trainControl(
  method = "cv",
  number = 10
)

knn_model <- train(
  proteomic_recipe,
  data = df_metdata,
  method = "knn",
  tuneGrid = data.frame(k=c(1,3,5,7,9,15)),
  trControl = ctrl
)