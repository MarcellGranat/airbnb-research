library(tidyverse)
load("C:/school/szem_8/TDK-airbnb/airbnb-research/dat_cleaned.RData")

library(parallel)

cl <- makeCluster(7)
clusterExport(cl, list("dat"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(tidyverse))

reviews_parsed <- parLapply(cl = cl, dat, function(x) { 
  tryCatch({
    tibble(text = x$comments) %>% 
      mutate(language = textcat::textcat(text)) %>% 
      tail(-2) %>% 
      mutate(id = x[["source"]][["id"]])
  }, error = function(e) NULL)
}
) %>% 
  {reduce(Filter(f = Negate(is.null), .), rbind)}

stopCluster(cl)

load('C:/school/szem_8/TDK-airbnb/airbnb-research/room_list.RData')

room_list <- room_list_total %>% 
  group_by(id) %>% 
  summarise(n_reviews = max(n_reviews, na.rm = T), assesment = mean(assesment, na.rm = T)) %>% 
  ungroup()

reviews_parsed <- room_list %>% 
  filter(n_reviews >= 0) %>% 
  mutate(
    rating = as.numeric(Hmisc::cut2(assesment, g = 10, levels.mean = T)),
    rating = case_when(
      rating == min(rating) ~ 'Neg',
      rating == max(rating) ~ 'Pos',
      T ~ 'Middle'
    ),
  ) %>% 
  merge(reviews_parsed) %>% 
  filter(language %in% c('hungarian')) %>% 
  select(rating, text) %>% 
  filter(rating != 'Middle')

library(tidymodels)

set.seed(123)
review_split <- initial_split(reviews_parsed, strata = rating)
review_train <- training(review_split)
review_test <- testing(review_split)

library(textrecipes)

review_rec <- recipe(rating ~ text, data = review_train) %>%
  step_tokenize(text) %>%
  step_stopwords(text, language = "hu") %>%
  step_tokenfilter(text, max_tokens = 500) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())

review_prep <- prep(review_rec)

review_prep

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(review_rec) %>%
  add_model(lasso_spec)

lasso_wf

lambda_grid <- grid_regular(penalty(), levels = 40)

set.seed(123)
review_folds <- bootstraps(review_train, strata = rating)
review_folds

doParallel::registerDoParallel()

set.seed(2020)
lasso_grid <- tune_grid(
  lasso_wf,
  resamples = review_folds,
  grid = lambda_grid,
  metrics = metric_set(roc_auc, ppv, npv)
)

best_auc <- lasso_grid %>%
  select_best("roc_auc")

final_lasso <- finalize_workflow(lasso_wf, best_auc)

final_lasso


library(vip)

final_lasso %>%
  fit(review_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = best_auc$penalty) %>%
  group_by(Sign) %>%
  top_n(20, wt = abs(Importance)) %>%
  ungroup() %>%
  mutate(
    Importance = abs(Importance),
    Variable = str_remove(Variable, "tfidf_text_"),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sign, scales = "free_y") +
  labs(y = NULL)

ggsave('C:/school/szem_8/TDK-airbnb/airbnb-research/words_result_hun.png', device = 'png')
