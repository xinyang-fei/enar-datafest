#******************************************************************************
# This is the code for ENAR 2025 datafest.
# Participants Weiran Yang, Xinyang Fei, Junlu Wang
# From Department of Biostatistics, New York University
# Analysis for data from NHANES 1999-2020            Last edit on Feb 17,2025
#******************************************************************************
library(survey)
library(randomForest)
library(caret)
library(tidyverse)
library(broom)
library(data.table)
library(survey)
library(GGally)
load("nhanes_data.rda")

##Calculates the Blood Pressure Control Rate by Year, and plot
bp_control_jnc7_by_year <- nhanes_data %>%
  group_by(svy_year) %>%
  summarise(
    bp_control_rate = mean(bp_control_jnc7 == "Yes", na.rm = TRUE)
  )
ggplot(bp_control_jnc7_by_year, aes(x = svy_year, y = bp_control_rate, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  expand_limits(y = 0) +
  labs(
    title = "Trend in Blood control Pressure Defined by JNC7 Guideline",
    x = "NHANES Survey Cycle",
    y = "Blood Pressure Control Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##visualizing prevalence of comorbidities over time
prev_data <- nhanes_data %>%
  group_by(svy_year) %>%
  summarise(
    bmi35plus = mean(cc_bmi == "35+", na.rm = TRUE),
    diabetes = mean(cc_diabetes == "Yes", na.rm = TRUE),
    ckd = mean(cc_ckd == "Yes", na.rm = TRUE),
    med_use = mean(bp_med_use == "Yes", na.rm = TRUE),
    cvd_ascvd = mean(cc_cvd_ascvd == "Yes", na.rm = TRUE),
    smoke = mean(cc_smoke == "Yes", na.rm = TRUE),
    cvd_mi = mean(cc_cvd_mi == "Yes", na.rm = TRUE),
    cvd_chd = mean(cc_cvd_chd == "Yes", na.rm = TRUE),
    cvd_stroke = mean(cc_cvd_stroke == "Yes", na.rm = TRUE),
    cvd_hf = mean(cc_cvd_hf == "Yes", na.rm = TRUE),
    cvd_any = mean(cc_cvd_any == "Yes", na.rm = TRUE)
  )

plot_bmi35plus <- ggplot(prev_data, aes(x = svy_year, y = bmi35plus, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  scale_x_discrete(labels = function(x) str_replace(x, "-", "\n")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  expand_limits(y = 0) +
  labs(title = "Prevalence of BMI >= 35",
       x = "Survey Year",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_diabetes <- ggplot(prev_data, aes(x = svy_year, y = diabetes, group = 1)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  scale_x_discrete(labels = function(x) str_replace(x, "-", "\n")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  expand_limits(y = 0) +
  labs(title = "Prevalence of Diabetes",
       x = "Survey Year",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_ckd <- ggplot(prev_data, aes(x = svy_year, y = ckd, group = 1)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point(color = "darkorange", size = 2) +
  scale_x_discrete(labels = function(x) str_replace(x, "-", "\n")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  expand_limits(y = 0) +
  labs(title = "Prevalence of CKD",
       x = "Survey Year",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_med_use <- ggplot(prev_data, aes(x = svy_year, y = med_use, group = 1)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_x_discrete(labels = function(x) str_replace(x, "-", "\n")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  expand_limits(y = 0) +
  labs(title = "Prevalence of med_use",
       x = "Survey Year",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_smoke <- ggplot(prev_data, aes(x = svy_year, y = med_use, group = 1)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = "skyblue", size = 2) +
  scale_x_discrete(labels = function(x) str_replace(x, "-", "\n")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  expand_limits(y = 0) +
  labs(title = "Prevalence of smoke",
       x = "Survey Year",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cvd_data <- prev_data %>%
  select(svy_year, starts_with("cvd")) %>%
  pivot_longer(
    cols = -svy_year,
    names_to = "cvd_type",
    names_prefix = "cvd_",
    values_to = "prevalence"
  )

plot_cvd_combined <- ggplot(cvd_data, aes(x = svy_year, y = prevalence, 
                                          color = cvd_type, group = cvd_type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_discrete(labels = function(x) str_replace(x, "-", "\n")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Set1") +
  expand_limits(y = 0) +
  labs(title = "Prevalence of Cardiovascular Conditions",
       x = "Survey Year",
       y = "Prevalence (%)",
       color = "CVD Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
par(mfrow = c(3, 1)) 
print(plot_bmi35plus)
print(plot_diabetes)
print(plot_ckd)
print(plot_smoke)
print(plot_med_use)
print(plot_cvd_combined)

##sub-dataset
early_cycles <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012")
late_cycles <- c("2013-2014", "2015-2016", "2017-2020")
pre_2013 <- nhanes_data[nhanes_data$svy_year %in% early_cycles, ]
post_2013 <- nhanes_data[nhanes_data$svy_year %in% late_cycles, ]

# UNIVARIATE ANALYSIS: Disease prevalence over time
combined <- rbind(
  pre_2013[, period := "1999-2012"],
  post_2013[, period := "2013-2020"]
)
comorbidity_vars <- c("cc_ckd", "cc_diabetes", "cc_cvd_ascvd", "cc_bmi", "cc_smoke")
lapply(comorbidity_vars, function(var) {
  prev_table <- table(combined$period, combined[[var]])
  prop_table <- prop.table(prev_table, margin = 1)
  
  cat("\n=== Variable: ", var, "===\n")
  print(prop_table)
  print(chisq.test(prev_table))
})
# BIVARIATE ANALYSIS: Chi-square tests for each variable with blood pressure control
lapply(comorbidity_vars, function(var) {
  formula_txt <- paste(var, "vs bp_control_jnc7")
  
  # Pre-2013 Analysis
  cat("\n\n=== Pre-2013:", formula_txt, "===")
  tbl_pre <- table(pre_2013[[var]], pre_2013$bp_control_jnc7)
  print(prop.table(tbl_pre, margin = 1))
  cat("\nChi-square test:", formula_txt, "(Pre-2013)\n")
  print(chisq.test(tbl_pre))
  
  # Post-2013 Analysis
  cat("\n\n=== Post-2013:", formula_txt, "===")
  tbl_post <- table(post_2013[[var]], post_2013$bp_control_jnc7)
  print(prop.table(tbl_post, margin = 1))
  cat("\nChi-square test:", formula_txt, "(Post-2013)\n")
  print(chisq.test(tbl_post))
})

##analyze the factors influencing blood pressure control, build a generalized linear model (GLM) for prediction.
nhanes_design <- svydesign(
  id = ~svy_psu, 
  strata = ~svy_strata,
  weights = ~svy_weight_mec, 
  data = nhanes_data,
  nest = TRUE
)
#Define generalized linear model formula
#Dependent variable is bp_control_jnc7, predictors include age, gender, race, and health indicators
model_formula <- bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race + 
  cc_bmi + cc_diabetes + cc_ckd + 
  cc_cvd_ascvd + cc_smoke
model <- svyglm(model_formula, design = nhanes_design, family = quasibinomial())
summary(model)

##total accurancy and importance
nhanes_data_complete <- nhanes_data[complete.cases(nhanes_data[, c("bp_control_jnc7", "demo_age_years", "demo_gender", "demo_race", "cc_bmi", "cc_diabetes", "cc_ckd", "cc_cvd_ascvd", "cc_smoke")]), ]
nhanes_data_complete$demo_gender <- as.factor(nhanes_data_complete$demo_gender)
nhanes_data_complete$demo_race <- as.factor(nhanes_data_complete$demo_race)
nhanes_data_complete$cc_bmi <- as.factor(nhanes_data_complete$cc_bmi)
nhanes_data_complete$cc_diabetes <- as.factor(nhanes_data_complete$cc_diabetes)
nhanes_data_complete$cc_ckd <- as.factor(nhanes_data_complete$cc_ckd)
nhanes_data_complete$cc_cvd_ascvd <- as.factor(nhanes_data_complete$cc_cvd_ascvd)
nhanes_data_complete$cc_smoke <- as.factor(nhanes_data_complete$cc_smoke)
rf_model <- randomForest(bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race + 
                           cc_bmi + cc_diabetes + cc_ckd + 
                           cc_cvd_ascvd + cc_smoke,
                         data = nhanes_data_complete,
                         importance = TRUE,
                         ntree = 500)
print(rf_model)
varImpPlot(rf_model)

##importance divided by 2013
pre_2013_complete <- pre_2013[complete.cases(pre_2013[, c("bp_control_jnc7", "demo_age_years", "demo_gender", "demo_race", "cc_bmi", "cc_diabetes", "cc_ckd", "cc_cvd_ascvd", "cc_smoke")]), ]
pre_2013_complete$demo_gender <- factor(pre_2013_complete$demo_gender)
pre_2013_complete$demo_race <- factor(pre_2013_complete$demo_race)
pre_2013_complete$cc_bmi <- factor(pre_2013_complete$cc_bmi)
pre_2013_complete$cc_diabetes <- factor(pre_2013_complete$cc_diabetes)
pre_2013_complete$cc_ckd <- factor(pre_2013_complete$cc_ckd)
pre_2013_complete$cc_cvd_ascvd <- factor(pre_2013_complete$cc_cvd_ascvd)
pre_2013_complete$cc_smoke <- factor(pre_2013_complete$cc_smoke)
set.seed(666)
train_index_pre <- createDataPartition(pre_2013_complete$bp_control_jnc7, p = 0.8, list = FALSE)
train_data_pre <- pre_2013_complete[train_index_pre, ]
test_data_pre <- pre_2013_complete[-train_index_pre, ]
ctrl <- trainControl(method = "cv", number = 4, classProbs = TRUE, summaryFunction = twoClassSummary)
ctrl$classProbs.lev <- c("No", "Yes")
rf_model_pre <- train(bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race + cc_bmi + cc_diabetes + cc_ckd + cc_cvd_ascvd + cc_smoke, data = train_data_pre, method = "rf", trControl = ctrl, metric = "ROC", importance = TRUE, ntree = 500)
importance_pre <- varImp(rf_model_pre, scale = FALSE)
print(importance_pre)
predictions_pre <- predict(rf_model_pre, test_data_pre)
accuracy_pre <- confusionMatrix(predictions_pre, test_data_pre$bp_control_jnc7)$overall['Accuracy']
print(paste("Pre-2013 accuracy on test set:", accuracy_pre))
post_2013_complete <- post_2013[complete.cases(post_2013[, c("bp_control_jnc7", "demo_age_years", "demo_gender", "demo_race", "cc_bmi", "cc_diabetes", "cc_ckd", "cc_cvd_ascvd", "cc_smoke")]), ]
post_2013_complete$demo_gender <- factor(post_2013_complete$demo_gender)
post_2013_complete$demo_race <- factor(post_2013_complete$demo_race)
post_2013_complete$cc_bmi <- factor(post_2013_complete$cc_bmi)
post_2013_complete$cc_diabetes <- factor(post_2013_complete$cc_diabetes)
post_2013_complete$cc_ckd <- factor(post_2013_complete$cc_ckd)
post_2013_complete$cc_cvd_ascvd <- factor(post_2013_complete$cc_cvd_ascvd)
post_2013_complete$cc_smoke <- factor(post_2013_complete$cc_smoke)
set.seed(666)
train_index_post <- createDataPartition(post_2013_complete$bp_control_jnc7, p = 0.8, list = FALSE)
train_data_post <- post_2013_complete[train_index_post, ]
test_data_post <- post_2013_complete[-train_index_post, ]
rf_model_post <- train(bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race + cc_bmi + cc_diabetes + cc_ckd + cc_cvd_ascvd + cc_smoke, data = train_data_post, method = "rf", trControl = ctrl, metric = "ROC", importance = TRUE, ntree = 500)
importance_post <- varImp(rf_model_post, scale = FALSE)
print(importance_post)
predictions_post <- predict(rf_model_post, test_data_post)
accuracy_post <- confusionMatrix(predictions_post, test_data_post$bp_control_jnc7)$overall['Accuracy']
print(paste("Post-2013 accuracy on test set:", accuracy_post))
print("Sample sizes:")
print(paste("Pre-2013 total samples:", nrow(pre_2013_complete)))
print(paste("Pre-2013 training samples:", nrow(train_data_pre)))
print(paste("Pre-2013 testing samples:", nrow(test_data_pre)))
print(paste("Post-2013 total samples:", nrow(post_2013_complete)))
print(paste("Post-2013 training samples:", nrow(train_data_post)))
print(paste("Post-2013 testing samples:", nrow(test_data_post)))

##uses a logistic regression model to explicitly examine how the effect of diabetes on blood pressure control might have changed between these periods
pre_2013_complete$demo_gender <- as.factor(pre_2013_complete$demo_gender)
pre_2013_complete$demo_race <- as.factor(pre_2013_complete$demo_race)
pre_2013_complete$cc_bmi <- as.factor(pre_2013_complete$cc_bmi)
pre_2013_complete$cc_diabetes <- as.factor(pre_2013_complete$cc_diabetes)
pre_2013_complete$cc_ckd <- as.factor(pre_2013_complete$cc_ckd)
pre_2013_complete$cc_cvd_ascvd <- as.factor(pre_2013_complete$cc_cvd_ascvd)
pre_2013_complete$cc_smoke <- as.factor(pre_2013_complete$cc_smoke)
post_2013_complete$demo_gender <- as.factor(post_2013_complete$demo_gender)
post_2013_complete$demo_race <- as.factor(post_2013_complete$demo_race)
post_2013_complete$cc_bmi <- as.factor(post_2013_complete$cc_bmi)
post_2013_complete$cc_diabetes <- as.factor(post_2013_complete$cc_diabetes)
post_2013_complete$cc_ckd <- as.factor(post_2013_complete$cc_ckd)
post_2013_complete$cc_cvd_ascvd <- as.factor(post_2013_complete$cc_cvd_ascvd)
post_2013_complete$cc_smoke <- as.factor(post_2013_complete$cc_smoke)
rf_model_pre <- randomForest(bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race +
                               cc_bmi + cc_diabetes + cc_ckd +
                               cc_cvd_ascvd + cc_smoke, 
                             data = pre_2013_complete, 
                             importance = TRUE)
rf_model_post <- randomForest(bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race +
                                cc_bmi + cc_diabetes + cc_ckd +
                                cc_cvd_ascvd + cc_smoke,
                              data = post_2013_complete,
                              importance = TRUE)
importance_pre <- importance(rf_model_pre)
importance_post <- importance(rf_model_post)
importance_pre
importance_post
combined_data <- bind_rows(
  pre_2013_complete %>% mutate(period = "pre_2013"),
  post_2013_complete %>% mutate(period = "post_2013")
)
model <- glm(bp_control_jnc7 ~ demo_age_years + demo_gender + demo_race +
               cc_bmi + cc_diabetes * period + cc_ckd +
               cc_cvd_ascvd + cc_smoke,
             data = combined_data, 
             family = binomial())
summary(model)

##Is the increase significant?
nhanes_data <- nhanes_data %>%
  mutate(svy_year_num = as.numeric(str_extract(svy_year, "^\\d{4}")))
nhanes_data <- nhanes_data %>%
  mutate(
    bmi35plus = ifelse(cc_bmi == "35+", 1, 0),
    diabetes = ifelse(cc_diabetes == "Yes", 1, 0),
    ckd = ifelse(cc_ckd == "Yes", 1, 0),
    cvd = ifelse(cc_cvd_ascvd == "Yes", 1, 0)
  )
logistic_reg <- function(data, outcome) {
  formula <- as.formula(paste(outcome, "~ svy_year_num"))
  model <- glm(formula, data = data, family = binomial())
  tidy(model)
}
reg_results <- tibble(
  outcome = c("bmi35plus", "diabetes", "ckd", "cvd"),
  data = list(nhanes_data, nhanes_data, nhanes_data, nhanes_data)
) %>%
  mutate(reg_output = map2(data, outcome, logistic_reg)) %>%
  unnest(reg_output)
p_values <- reg_results %>%
  filter(term == "svy_year_num") %>%
  mutate(outcome = case_when(
    outcome == "bmi35plus" ~ "BMI >= 35",
    outcome == "diabetes" ~ "Diabetes",
    outcome == "ckd" ~ "CKD",
    outcome == "cvd" ~ "CVD"
  )) %>%
  mutate(p_value = format.pval(p.value, digits = 3, eps = 0.001)) %>%
  select(outcome, p_value)
print(p_values)

##Instructions for drug use
library(data.table)
target_population <- nhanes_data[cc_diabetes == "Yes" & 
                                   cc_ckd == "Yes" & 
                                   cc_cvd_any == "Yes" & 
                                   htn_jnc7 == "No"]
total_people <- nrow(target_population)

target_population$num_medications <- rowSums(target_population[, .(bp_med_ace, bp_med_aldo, bp_med_alpha, bp_med_angioten, bp_med_beta, bp_med_ccb, bp_med_central, bp_med_renin_inhibitors, bp_med_vasod, bp_med_diur_loop, bp_med_diur_Ksparing, bp_med_diur_thz)] == "Yes", na.rm = TRUE)

num_no_medications <- sum(target_population$num_medications == 0, na.rm = TRUE)
num_one_medication <- sum(target_population$num_medications == 1, na.rm = TRUE)
num_multiple_medications <- sum(target_population$num_medications > 1, na.rm = TRUE)

medication_stats <- data.frame(
  ACE_inhibitors = sum(target_population$bp_med_ace == "Yes", na.rm = TRUE),
  Aldosterone_antagonists = sum(target_population$bp_med_aldo == "Yes", na.rm = TRUE),
  Alpha_blockers = sum(target_population$bp_med_alpha == "Yes", na.rm = TRUE),
  Angiotensin_receptor_blockers = sum(target_population$bp_med_angioten == "Yes", na.rm = TRUE),
  Beta_blockers = sum(target_population$bp_med_beta == "Yes", na.rm = TRUE),
  Calcium_channel_blockers = sum(target_population$bp_med_ccb == "Yes", na.rm = TRUE),
  Central_alpha_agonists = sum(target_population$bp_med_central == "Yes", na.rm = TRUE),
  Direct_renin_inhibitors = sum(target_population$bp_med_renin_inhibitors == "Yes", na.rm = TRUE),
  Direct_vasodilators = sum(target_population$bp_med_vasod == "Yes", na.rm = TRUE),
  Loop_diuretics = sum(target_population$bp_med_diur_loop == "Yes", na.rm = TRUE),
  Potassium_sparing_diuretics = sum(target_population$bp_med_diur_Ksparing == "Yes", na.rm = TRUE),
  Thiazide_diuretics = sum(target_population$bp_med_diur_thz == "Yes", na.rm = TRUE)
)

medication_stats_percentage <- medication_stats / total_people * 100
cat("There are", total_people, "people in the target population.\n")
cat("-", num_no_medications, "people (", round(num_no_medications / total_people * 100, 2), "%) are taking no medications.\n")
cat("-", num_one_medication, "people (", round(num_one_medication / total_people * 100, 2), "%) are taking one medication.\n")
cat("-", num_multiple_medications, "people (", round(num_multiple_medications / total_people * 100, 2), "%) are taking multiple medications.\n\n")
cat("Medication usage statistics:\n")
for (i in 1:ncol(medication_stats)) {
  drug_name <- colnames(medication_stats)[i]
  drug_count <- medication_stats[, i]
  drug_percentage <- medication_stats_percentage[, i]
  cat("-", drug_count, "people (", round(drug_percentage, 2), "%) are taking", drug_name, "\n")
}

##Who is more obvious?
calc_prevalence <- function(data, condition_var) {
  design <- svydesign(
    ids = ~svy_psu,
    strata = ~svy_strata,
    weights = ~svy_weight_mec,
    nest = TRUE,
    data = data
  )
  formula_str <- paste0("~", condition_var)
  svymean(as.formula(formula_str), design, na.rm = TRUE)
}
diabetes_prev <- list(
  pre = calc_prevalence(pre_2013_complete, "cc_diabetes"),
  post = calc_prevalence(post_2013_complete, "cc_diabetes")
)
ckd_prev <- list(
  pre = calc_prevalence(pre_2013_complete, "cc_ckd"),
  post = calc_prevalence(post_2013_complete, "cc_ckd")
)
rf_formula <- bp_control_jnc7 ~ cc_diabetes + cc_ckd + demo_age_cat + 
  demo_race + cc_bmi + cc_cvd_any + cc_smoke
set.seed(666)
pre_model <- randomForest(rf_formula, data = pre_2013_complete, importance = TRUE)
post_model <- randomForest(rf_formula, data = post_2013_complete, importance = TRUE)
importance_pre <- importance(pre_model)
importance_post <- importance(post_model)
decompose_effect <- function(post_data) {
  scenario1_data <- copy(post_data)
  scenario1_data[, cc_diabetes := factor(
    ifelse(runif(.N) < diabetes_prev$pre[1], "Yes", "No"),
    levels = c("No", "Yes")
  )]
  scenario1_data[, cc_ckd := factor(
    ifelse(runif(.N) < ckd_prev$pre[1], "Yes", "No"),
    levels = c("No", "Yes")
  )]
  scenario1_pred <- predict(post_model, newdata = scenario1_data, type = "prob")[,2]
  scenario2_pred <- predict(pre_model, newdata = post_data, type = "prob")[,2]
  actual_pred <- predict(post_model, newdata = post_data, type = "prob")[,2]
  effect_decomposition <- data.table(
    scenario_prev = mean(scenario1_pred) - mean(actual_pred),
    scenario_importance = mean(scenario2_pred) - mean(actual_pred)
  )
  return(effect_decomposition)
}
set.seed(666)
effect_results <- decompose_effect(post_2013_complete)
bootstrap_decomposition <- function(data, n_boot = 1000) {
  boot_results <- rbindlist(lapply(1:n_boot, function(i) {
    boot_sample <- data[sample(.N, replace = TRUE)]
    decompose_effect(boot_sample)
  }))
  return(boot_results)
}
boot_results <- bootstrap_decomposition(post_2013_complete, n_boot = 1000)
ci_results <- boot_results[, .(
  prev_lower = quantile(scenario_prev, 0.025),
  prev_upper = quantile(scenario_prev, 0.975),
  importance_lower = quantile(scenario_importance, 0.025),
  importance_upper = quantile(scenario_importance, 0.975)
)]
final_result <- data.table(
  Effect = c("Prevalence Change", "Importance Change"),
  Estimate = c(effect_results$scenario_prev, effect_results$scenario_importance),
  CI_Lower = c(ci_results$prev_lower, ci_results$importance_lower),
  CI_Upper = c(ci_results$prev_upper, ci_results$importance_upper)
)
print(final_result)

