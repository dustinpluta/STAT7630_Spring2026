## ============================================================
## STAT7630: Complete Logistic Regression Data Analysis Pipeline
## Dataset: student_exam_performance_dataset.csv
## Outcome: pass_fail
## Event of interest: Pass
##
## Analysis goals:
##   1. Domain-informed model design
##   2. EDA
##   3. Logistic regression modeling
##   4. Model comparison
##   5. Diagnostics
##   6. Calibration and prediction plots
##   7. LASSO logistic regression for prediction / variable selection
##   8. Interpretation and conclusion
## ============================================================

## ------------------------------------------------------------
## 0. Package setup
## ------------------------------------------------------------

needed <- c("tidyverse", "broom", "pROC", "car", "glmnet", "splines")

for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(broom)
library(pROC)
library(car)
library(glmnet)
library(splines)

## ------------------------------------------------------------
## 1. Load data
## ------------------------------------------------------------
# Data can be found here: https://www.kaggle.com/datasets/ssssws/student-exam-performance-dataset?resource=download

# Set home directory and read data
setwd("C:/Users/dpluta/AU/Teaching/STAT7630_Spring2026/")
dat_raw <- read.csv("data/student_exam_performance_dataset.csv")

cat("\n--- Data dimensions ---\n")
print(dim(dat_raw))

cat("\n--- Variable names ---\n")
print(names(dat_raw))

cat("\n--- Data structure ---\n")
str(dat_raw)

cat("\n--- First few rows ---\n")
print(head(dat_raw))

## ------------------------------------------------------------
## 2. Domain-informed model design
## ------------------------------------------------------------

## Outcome:
##   pass_fail: Pass vs Fail
##
## Event:
##   Pass
##
## Variables to exclude from primary model:
##
## 1. student_id
##    Identifier only; not substantively meaningful.
##
## 2. grade_category
##    Likely derived from course/exam performance and closely related to pass_fail.
##    Including it would likely create data leakage.
##
## 3. final_exam_score
##    The outcome is Pass/Fail in student/course/exam data.
##    If pass_fail is defined from final_exam_score, then final_exam_score
##    should not be used as a predictor of pass/fail.
##
## Variables potentially included:
##   gender, age, parental_education, family_income, internet_access,
##   study_environment, study_hours_per_day, attendance_rate, sleep_hours,
##   social_media_hours, assignment_completion_rate, participation_score,
##   online_courses_completed, tutoring, math_score, reading_score,
##   writing_score, science_score, previous_gpa
##
## Note on online_courses_completed:
##   This variable may be a pre-course preparation variable or it may reflect
##   student engagement during the course. Whether it should be included depends
##   on timing and causal interpretation. We include it in the predictive model
##   but also fit a sensitivity model excluding it.

dat <- dat_raw %>%
  mutate(
    pass_binary = ifelse(pass_fail == "Pass", 1, 0),
    pass_fail = factor(pass_fail, levels = c("Fail", "Pass")),
    gender = factor(gender),
    parental_education = factor(parental_education),
    family_income = factor(family_income),
    internet_access = factor(internet_access),
    study_environment = factor(study_environment),
    tutoring = factor(tutoring),
    grade_category = factor(grade_category)
  )

cat("\n--- Outcome distribution ---\n")
print(table(dat$pass_fail))
print(prop.table(table(dat$pass_fail)))

## ------------------------------------------------------------
## 3. Missing data assessment
## ------------------------------------------------------------

missing_tab <- data.frame(
  variable = names(dat),
  n_missing = sapply(dat, function(x) sum(is.na(x))),
  pct_missing = sapply(dat, function(x) mean(is.na(x)) * 100)
)

cat("\n--- Missing data summary ---\n")
print(missing_tab %>% arrange(desc(n_missing)))

## For this classroom demo, use complete cases for candidate predictors.
## In a real analysis, consider multiple imputation if missingness is nontrivial.

analysis_vars <- c(
  "pass_fail", "pass_binary",
  "gender", "age", "parental_education", "family_income",
  "internet_access", "study_environment",
  "study_hours_per_day", "attendance_rate", "sleep_hours",
  "social_media_hours", "online_courses_completed", "tutoring",
  "previous_gpa", "math_score", "reading_score", "science_score", "writing_score" 
)

dat_analysis <- dat %>%
  select(all_of(analysis_vars)) %>%
  drop_na()

cat("\n--- Complete-case analysis dimensions ---\n")
print(dim(dat_analysis))

## ------------------------------------------------------------
## 4. Exploratory data analysis
## ------------------------------------------------------------

## 4.1 Outcome proportion
ggplot(dat_analysis, aes(x = pass_fail, fill = pass_fail)) +
  geom_bar() +
  labs(
    title = "Distribution of Pass/Fail Outcome",
    x = "Outcome",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## 4.2 Numeric predictor summaries by outcome
numeric_vars <- dat_analysis %>%
  select(where(is.numeric)) %>%
  select(-pass_binary) %>%
  names()

numeric_summary <- dat_analysis %>%
  group_by(pass_fail) %>%
  summarise(
    across(
      all_of(numeric_vars),
      list(mean = mean, sd = sd, median = median),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

cat("\n--- Numeric predictor summaries by outcome ---\n")
print(numeric_summary[, 5:10])

## 4.3 Boxplots for numeric predictors by pass/fail
v <- numeric_vars[6]
p <- ggplot(dat_analysis, aes(x = pass_fail, y = .data[[v]], fill = pass_fail)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = paste(v, "by Pass/Fail Status"),
    x = "Pass/Fail",
    y = v
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p)


## 4.4 Categorical predictors by outcome
categorical_vars <- c(
  "gender", "parental_education", "family_income",
  "internet_access", "study_environment", "tutoring"
)

for (v in categorical_vars) {
  p <- ggplot(dat_analysis, aes(x = .data[[v]], fill = pass_fail)) +
    geom_bar(position = "fill") +
    labs(
      title = paste("Pass/Fail Proportion by", v),
      x = v,
      y = "Proportion",
      fill = "Outcome"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
}

## 4.5 Correlation among numeric predictors
cor_mat <- cor(dat_analysis %>% select(all_of(numeric_vars)), use = "complete.obs")

cat("\n--- Correlation matrix among numeric predictors ---\n")
print(round(cor_mat, 2))

## Simple base R correlation heatmap
heatmap(
  cor_mat,
  symm = TRUE,
  main = "Correlation Heatmap: Numeric Predictors"
)

## ------------------------------------------------------------
## 5. Train/test split
## ------------------------------------------------------------

set.seed(7630)

n <- nrow(dat_analysis)
train_id <- sample(seq_len(n), size = round(0.70 * n))

train <- dat_analysis[train_id, ]
test  <- dat_analysis[-train_id, ]

cat("\n--- Train/test sizes ---\n")
print(nrow(train))
print(nrow(test))

cat("\n--- Outcome distribution in train ---\n")
print(prop.table(table(train$pass_fail)))

cat("\n--- Outcome distribution in test ---\n")
print(prop.table(table(test$pass_fail)))

## ------------------------------------------------------------
## 6. Logistic regression model fitting
## ------------------------------------------------------------

## Model 0: null model
m0 <- glm(
  pass_fail ~ 1,
  data = train,
  family = binomial
)

## Model 1: domain-informed academic preparation model
m_academic <- glm(
  pass_fail ~
    study_hours_per_day +
    attendance_rate +
    tutoring +
    previous_gpa,
  data = train,
  family = binomial
)

## Model 2: full domain-informed model
m_full <- glm(
  pass_fail ~
    gender +
    age +
    parental_education +
    family_income +
    internet_access +
    study_environment +
    study_hours_per_day +
    attendance_rate +
    sleep_hours +
    social_media_hours +
    online_courses_completed +
    tutoring +
    previous_gpa,
  data = train,
  family = binomial
)

## Model 3: sensitivity model excluding online_courses_completed 
#           and subject scores
m_no_online <- glm(
  pass_fail ~
    gender +
    age +
    parental_education +
    family_income +
    internet_access +
    study_environment +
    study_hours_per_day +
    attendance_rate +
    sleep_hours +
    social_media_hours +
    tutoring +
    previous_gpa,
  data = train,
  family = binomial
)

## Model 4: allow nonlinear effects for selected continuous variables
## Use splines for variables where nonlinearity is plausible.
m_spline <- glm(
  pass_fail ~
    gender +
    age +
    parental_education +
    family_income +
    internet_access +
    study_environment +
    ns(study_hours_per_day, df = 3) +
    ns(attendance_rate, df = 3) +
    ns(sleep_hours, df = 3) +
    ns(social_media_hours, df = 3) +
    online_courses_completed +
    tutoring +
    previous_gpa,
  data = train,
  family = binomial
)

cat("\n--- Model summaries ---\n")
print(summary(m_academic))
print(summary(m_full))
print(summary(m_no_online))
print(summary(m_spline))

# Degenerate Model including subject scores
m_degenerate <- glm(
  pass_fail ~
    gender +
    age +
    parental_education +
    family_income +
    internet_access +
    study_environment +
    study_hours_per_day +
    attendance_rate +
    sleep_hours +
    social_media_hours +
    tutoring +
    math_score +
    reading_score +
    writing_score +
    science_score +
    previous_gpa,
  data = train,
  family = binomial
)
summary(m_degenerate)
## ------------------------------------------------------------
## 7. Odds ratios and confidence intervals
## ------------------------------------------------------------

or_table <- function(model) {
  tidy(model, conf.int = TRUE) %>%
    mutate(
      OR = exp(estimate),
      OR_low = exp(conf.low),
      OR_high = exp(conf.high)
    ) %>%
    select(term, estimate, std.error, statistic, p.value, OR, OR_low, OR_high)
}

cat("\n--- Odds ratios: academic model ---\n")
print(or_table(m_academic))

cat("\n--- Odds ratios: full model ---\n")
print(or_table(m_full))

## ------------------------------------------------------------
## 8. Model comparison
## ------------------------------------------------------------

model_comp <- data.frame(
  model = c("Null", "Academic", "Full", "No online courses", "Spline"),
  df = c(df.residual(m0), df.residual(m_academic), df.residual(m_full),
         df.residual(m_no_online), df.residual(m_spline)),
  AIC = c(AIC(m0), AIC(m_academic), AIC(m_full),
          AIC(m_no_online), AIC(m_spline)),
  BIC = c(BIC(m0), BIC(m_academic), BIC(m_full),
          BIC(m_no_online), BIC(m_spline)),
  deviance = c(deviance(m0), deviance(m_academic), deviance(m_full),
               deviance(m_no_online), deviance(m_spline))
) %>%
  arrange(AIC)

cat("\n--- Model comparison table ---\n")
print(model_comp)

cat("\n--- Likelihood ratio tests for nested comparisons ---\n")
cat("\nAcademic vs Full:\n")
print(anova(m_academic, m_full, test = "Chisq"))

cat("\nNo-online vs Full:\n")
print(anova(m_no_online, m_full, test = "Chisq"))

cat("\nFull vs Spline:\n")
print(anova(m_full, m_spline, test = "Chisq"))

## ------------------------------------------------------------
## 9. Prediction performance utilities
## ------------------------------------------------------------

brier_score <- function(y01, phat) {
  mean((y01 - phat)^2)
}

classification_metrics <- function(y01, phat, threshold = 0.5) {
  pred <- ifelse(phat >= threshold, 1, 0)
  
  TP <- sum(pred == 1 & y01 == 1)
  TN <- sum(pred == 0 & y01 == 0)
  FP <- sum(pred == 1 & y01 == 0)
  FN <- sum(pred == 0 & y01 == 1)
  
  accuracy <- (TP + TN) / length(y01)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  
  data.frame(
    threshold = threshold,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    PPV = ppv,
    NPV = npv
  )
}

evaluate_model <- function(model, train, test, model_name) {
  phat_train <- predict(model, newdata = train, type = "response")
  phat_test  <- predict(model, newdata = test, type = "response")
  
  auc_train <- as.numeric(auc(train$pass_binary, phat_train))
  auc_test  <- as.numeric(auc(test$pass_binary, phat_test))
  
  data.frame(
    model = model_name,
    train_brier = brier_score(train$pass_binary, phat_train),
    test_brier = brier_score(test$pass_binary, phat_test),
    train_auc = auc_train,
    test_auc = auc_test
  )
}

perf <- bind_rows(
  evaluate_model(m_academic, train, test, "Academic"),
  evaluate_model(m_full, train, test, "Full"),
  evaluate_model(m_no_online, train, test, "No online courses"),
  evaluate_model(m_spline, train, test, "Spline")
) %>%
  arrange(test_brier)

cat("\n--- Predictive performance on train/test ---\n")
print(perf)

## Choose a working final model.
## This can be changed depending on model comparison results.
## For teaching, we choose the full model unless spline clearly improves
## predictive performance and calibration.

final_model <- m_no_online
final_model_name <- "Final Model logistic regression (no online courses completion included)"

cat("\n--- Final model chosen for detailed diagnostics ---\n")
cat(final_model_name, "\n")

## ------------------------------------------------------------
## 10. Calibration plots
## ------------------------------------------------------------

calibration_plot <- function(y01, phat, n_bins = 10,
                             main = "Calibration Plot") {
  breaks <- quantile(phat, probs = seq(0, 1, length.out = n_bins + 1),
                     na.rm = TRUE)
  breaks <- unique(breaks)
  
  if (length(breaks) < 3) {
    breaks <- seq(min(phat), max(phat), length.out = n_bins + 1)
  }
  
  bin <- cut(phat, breaks = breaks, include.lowest = TRUE)
  
  cal <- data.frame(
    y = y01,
    phat = phat,
    bin = bin
  ) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(phat),
      obs_rate = mean(y),
      n = n(),
      .groups = "drop"
    )
  
  plot(
    cal$mean_pred,
    cal$obs_rate,
    xlim = c(0, 1),
    ylim = c(0, 1),
    pch = 19,
    xlab = "Mean predicted probability",
    ylab = "Observed pass rate",
    main = main
  )
  abline(0, 1, lty = 2, lwd = 2)
  lines(cal$mean_pred, cal$obs_rate, lwd = 2)
  text(cal$mean_pred, cal$obs_rate, labels = cal$n, pos = 3, cex = 0.8)
  
  invisible(cal)
}

phat_train_final <- predict(final_model, newdata = train, type = "response")
phat_test_final  <- predict(final_model, newdata = test, type = "response")

par(mfrow = c(1, 2))
cal_train <- calibration_plot(
  train$pass_binary,
  phat_train_final,
  main = "Calibration: Train Set"
)
cal_test <- calibration_plot(
  test$pass_binary,
  phat_test_final,
  main = "Calibration: Test Set"
)
par(mfrow = c(1, 1))

cat("\n--- Calibration table: test set ---\n")
print(cal_test)

## ------------------------------------------------------------
## 11. ROC curve
## ------------------------------------------------------------

roc_obj <- roc(test$pass_binary, phat_test_final)

plot(
  roc_obj,
  main = paste("ROC Curve:", final_model_name),
  lwd = 2
)

cat("\n--- Test AUC ---\n")
print(auc(roc_obj))

## ------------------------------------------------------------
## 12. Distribution of predicted probabilities
## ------------------------------------------------------------

test_pred_df <- test %>%
  mutate(phat = phat_test_final)

ggplot(test_pred_df, aes(x = phat, fill = pass_fail)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(
    title = "Distribution of Predicted Probabilities by Observed Outcome",
    x = "Predicted probability of Pass",
    y = "Count",
    fill = "Observed outcome"
  ) +
  theme_minimal()

## ------------------------------------------------------------
## 13. Diagnostic plots for logistic regression
## ------------------------------------------------------------

pearson_resid <- residuals(final_model, type = "pearson")
deviance_resid <- residuals(final_model, type = "deviance")
fitted_prob <- fitted(final_model)
lev <- hatvalues(final_model)
cook <- cooks.distance(final_model)

par(mfrow = c(2, 2))

plot(
  fitted_prob, pearson_resid,
  pch = 19, col = rgb(0, 0, 0, 0.25),
  xlab = "Fitted probability",
  ylab = "Pearson residual",
  main = "Pearson Residuals vs Fitted"
)
abline(h = 0, lwd = 2)

plot(
  fitted_prob, deviance_resid,
  pch = 19, col = rgb(0, 0, 0, 0.25),
  xlab = "Fitted probability",
  ylab = "Deviance residual",
  main = "Deviance Residuals vs Fitted"
)
abline(h = 0, lwd = 2)

qqnorm(deviance_resid, main = "Q-Q Plot: Deviance Residuals")
qqline(deviance_resid, lwd = 2)

plot(
  lev, deviance_resid,
  pch = 19, col = rgb(0, 0, 0, 0.25),
  xlab = "Leverage",
  ylab = "Deviance residual",
  main = "Leverage vs Deviance Residual"
)
abline(h = 0, lwd = 2)

par(mfrow = c(1, 1))

## Cook's distance
plot(
  cook,
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's distance",
  xlab = "Observation index"
)
abline(h = 4 / nrow(train), lty = 2, lwd = 2)

cat("\n--- Largest Cook's distances ---\n")
print(
  data.frame(
    row_id = seq_along(cook),
    cooks_distance = cook
  ) %>%
    arrange(desc(cooks_distance)) %>%
    head(10)
)

## ------------------------------------------------------------
## 14. Multicollinearity check
## ------------------------------------------------------------

cat("\n--- VIF for final model ---\n")
print(vif(final_model))

cat("\nVIF interpretation:\n")
cat("* VIF near 1 indicates little collinearity.\n")
cat("* VIF > 5 may suggest moderate concern.\n")
cat("* VIF > 10 may suggest serious multicollinearity.\n")

## ------------------------------------------------------------
## 15. Effect plots for key predictors
## ------------------------------------------------------------

## Function to create prediction grid holding other variables fixed
make_reference_row <- function(data) {
  ref <- data[1, , drop = FALSE]
  
  for (nm in names(data)) {
    if (is.numeric(data[[nm]])) {
      ref[[nm]] <- mean(data[[nm]], na.rm = TRUE)
    } else if (is.factor(data[[nm]])) {
      ref[[nm]] <- levels(data[[nm]])[1]
    }
  }
  
  ref
}

ref <- make_reference_row(train)

## Remove outcome columns from reference row
ref_pred <- ref %>%
  select(-pass_fail, -pass_binary)

## Effect plot for continuous predictors
plot_continuous_effect <- function(model, data, var_name, n_grid = 100) {
  ref <- make_reference_row(data) %>%
    select(-pass_fail, -pass_binary)
  
  x_grid <- seq(
    min(data[[var_name]], na.rm = TRUE),
    max(data[[var_name]], na.rm = TRUE),
    length.out = n_grid
  )
  
  newdat <- ref[rep(1, n_grid), ]
  newdat[[var_name]] <- x_grid
  
  pred <- predict(model, newdata = newdat, type = "link", se.fit = TRUE)
  
  eta <- pred$fit
  se <- pred$se.fit
  
  out <- data.frame(
    x = x_grid,
    phat = plogis(eta),
    lower = plogis(eta - 1.96 * se),
    upper = plogis(eta + 1.96 * se)
  )
  
  ggplot(out, aes(x = x, y = phat)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(
      title = paste("Predicted Probability of Passing vs", var_name),
      x = var_name,
      y = "Predicted probability of Pass"
    ) +
    theme_minimal()
}

key_continuous <- c(
  "social_media_hours"
)

for (v in key_continuous) {
  print(plot_continuous_effect(final_model, train, v))
}

## Effect plot for categorical predictor
plot_categorical_effect <- function(model, data, var_name) {
  ref <- make_reference_row(data) %>%
    select(-pass_fail, -pass_binary)
  
  levs <- levels(data[[var_name]])
  newdat <- ref[rep(1, length(levs)), ]
  newdat[[var_name]] <- factor(levs, levels = levs)
  
  pred <- predict(model, newdata = newdat, type = "link", se.fit = TRUE)
  
  out <- data.frame(
    level = levs,
    phat = plogis(pred$fit),
    lower = plogis(pred$fit - 1.96 * pred$se.fit),
    upper = plogis(pred$fit + 1.96 * pred$se.fit)
  )
  
  ggplot(out, aes(x = level, y = phat)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
    labs(
      title = paste("Predicted Probability of Passing by", var_name),
      x = var_name,
      y = "Predicted probability of Pass"
    ) +
    theme_minimal()
}

for (v in categorical_vars) {
  print(plot_categorical_effect(final_model, train, v))
}

## ------------------------------------------------------------
## 16. LASSO logistic regression
## ------------------------------------------------------------

## LASSO can be used for prediction and variable selection.
## Here we include all candidate predictors, excluding:
##   student_id, final_exam_score, grade_category
##
## glmnet requires x matrix and y numeric 0/1.

x_train <- model.matrix(
  pass_binary ~
    gender +
    age +
    parental_education +
    family_income +
    internet_access +
    study_environment +
    study_hours_per_day +
    attendance_rate +
    sleep_hours +
    social_media_hours +
    online_courses_completed +
    tutoring +
    previous_gpa,
  data = train
)[, -1]

y_train <- train$pass_binary

x_test <- model.matrix(
  pass_binary ~
    gender +
    age +
    parental_education +
    family_income +
    internet_access +
    study_environment +
    study_hours_per_day +
    attendance_rate +
    sleep_hours +
    social_media_hours +
    online_courses_completed +
    tutoring +
    previous_gpa,
  data = test
)[, -1]

y_test <- test$pass_binary

set.seed(7630)
cv_lasso <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "binomial",
  alpha = 1,
  nfolds = 10,
  standardize = TRUE
)

plot(cv_lasso)
title("Cross-Validated LASSO Logistic Regression", line = 2.5)

cat("\n--- LASSO chosen lambdas ---\n")
cat("lambda.min:", cv_lasso$lambda.min, "\n")
cat("lambda.1se:", cv_lasso$lambda.1se, "\n")

coef_min <- coef(cv_lasso, s = "lambda.min")
coef_1se <- coef(cv_lasso, s = "lambda.1se")

selected_min <- rownames(coef_min)[as.numeric(coef_min) != 0]
selected_1se <- rownames(coef_1se)[as.numeric(coef_1se) != 0]

cat("\n--- Variables selected at lambda.min ---\n")
print(selected_min)

cat("\n--- Variables selected at lambda.1se ---\n")
print(selected_1se)

## Predictive performance for LASSO
phat_lasso_min <- as.numeric(predict(cv_lasso, newx = x_test, s = "lambda.min", type = "response"))
phat_lasso_1se <- as.numeric(predict(cv_lasso, newx = x_test, s = "lambda.1se", type = "response"))

lasso_perf <- data.frame(
  model = c("LASSO lambda.min", "LASSO lambda.1se"),
  test_brier = c(
    brier_score(y_test, phat_lasso_min),
    brier_score(y_test, phat_lasso_1se)
  ),
  test_auc = c(
    as.numeric(auc(y_test, phat_lasso_min)),
    as.numeric(auc(y_test, phat_lasso_1se))
  )
)

cat("\n--- LASSO predictive performance ---\n")
print(lasso_perf)

## Compare final logistic model to LASSO
final_perf <- data.frame(
  model = final_model_name,
  test_brier = brier_score(test$pass_binary, phat_test_final),
  test_auc = as.numeric(auc(test$pass_binary, phat_test_final))
)

cat("\n--- Final model vs LASSO ---\n")
print(bind_rows(final_perf, lasso_perf))

par(mfrow = c(1, 2))
calibration_plot(y_test, phat_lasso_min, main = "Calibration: LASSO lambda.min")
calibration_plot(y_test, phat_lasso_1se, main = "Calibration: LASSO lambda.1se")
par(mfrow = c(1, 1))

## ------------------------------------------------------------
## 17. Optional: threshold-based classification summaries
## ------------------------------------------------------------

cat("\n--- Classification metrics for final model, threshold = 0.5 ---\n")
print(classification_metrics(test$pass_binary, phat_test_final, threshold = 0.5))

cat("\n--- Classification metrics for LASSO lambda.min, threshold = 0.5 ---\n")
print(classification_metrics(y_test, phat_lasso_min, threshold = 0.5))

## Explore multiple thresholds
thresholds <- seq(0.2, 0.8, by = 0.1)

threshold_tab <- bind_rows(
  lapply(thresholds, function(th) {
    classification_metrics(test$pass_binary, phat_test_final, threshold = th)
  })
)

cat("\n--- Threshold sensitivity for final model ---\n")
print(threshold_tab)

## ------------------------------------------------------------
## 18. Written interpretation templates
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("INTERPRETATION GUIDE\n")
cat("============================================================\n")

cat("\n1. Domain-informed exclusion decisions:\n")
cat("\n   student_id was excluded because it is an identifier.")
cat("\n   grade_category, subject scores, participation score, and final_exam_score were excluded because they are likely")
cat("\n   downstream summaries or direct components of pass/fail status, creating")
cat("\n   potential data leakage.\n")

cat("\n\n2. Logistic regression interpretation:")
cat("\n   Coefficients are on the log-odds scale.")
cat("\n   Exponentiated coefficients are odds ratios.")
cat("\n   Predicted probabilities are often easier to communicate to nontechnical audiences.\n")

cat("\n\n3. Model diagnostics:")
cat("\n   Calibration plots evaluate whether predicted probabilities match observed pass rates.")
cat("\n   ROC/AUC evaluates discrimination: how well the model ranks passing students above failing students.")
cat("\n   Pearson/deviance residuals and influence diagnostics identify unusual or influential cases.\n")

cat("\n\n4. LASSO interpretation:")
cat("\n   LASSO is useful for prediction and variable selection.")
cat("\n   Variables selected by LASSO should be treated as data-driven and should not automatically")
cat("\n   be interpreted as causal predictors.\n")

## ------------------------------------------------------------
## 19. Final conclusion summary
## ------------------------------------------------------------

cat("\n\n============================================================\n")
cat("CONCLUSION / SUMMARY OF RESULTS\n")
cat("============================================================\n")

cat("\nA complete student pass/fail analysis should report:\n")
cat("\n1. The event rate: proportion of students who pass.")
cat("\n2. Key EDA findings: which predictors differ between Pass and Fail groups.")
cat("\n3. Domain-informed model design: which variables were included/excluded and why.")
cat("\n4. Logistic regression results: odds ratios, confidence intervals, and p-values.")
cat("\n5. Model comparison: AIC/BIC, likelihood ratio tests, and train/test predictive metrics.")
cat("\n6. Diagnostics: calibration, residual diagnostics, influence, and multicollinearity.")
cat("\n7. Prediction results: AUC, Brier score, and threshold-dependent classification metrics.")
cat("\n8. LASSO results: selected variables and predictive performance comparison.")
cat("\n9. Practical interpretation: which student/course factors are most associated with passing.")
cat("\n10. Limitations: observational data, potential leakage, timing of variables, and non-causal interpretation.\n")

cat("\n\nAnalysis complete.\n")