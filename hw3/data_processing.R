library(cardioStatsUSA)
library(tidyverse)
library(dplyr)
library(caret)

# svy_year: categorical (10 levels)
# demo_race: categorical/factor with five levels
# demo_age_years: continuous
# demo_pregnant: binary
# demo_gender: binary
# htn_jnc7: binary, response
# htn_aware: binary
# bp_med_use: binary
# bp_med_recommended_jnc7: binary
# bp_med_n_class: categorical
# bp_med_ace: binary
# bp_med_aldo: binary
# bp_med_alpha: binary
# bp_med_angioten: binary
# bp_med_beta: binary

# select data points that have available data on outcome
df = nhanes_data[svy_subpop_htn == 1] %>%
  # hypertension according to JNC7 guidelines
  # kept only demographic, medication, and comorbidity variables
  select(svy_year, demo_race, demo_age_years, demo_pregnant, demo_gender,
         htn_jnc7, htn_aware, bp_med_use, bp_med_recommended_jnc7, bp_med_n_class, 
         bp_med_n_class, bp_med_ace, bp_med_aldo, bp_med_alpha, bp_med_angioten, 
         bp_med_beta, bp_med_ccb, bp_med_ccb_dh, bp_med_ccb_ndh, bp_med_central, 
         bp_med_renin_inhibitors, bp_med_vasod, bp_med_diur_loop, bp_med_diur_Ksparing,
         bp_med_diur_thz, cc_smoke, cc_diabetes, cc_bmi, cc_ckd, cc_cvd_mi, 
         cc_cvd_chd, cc_cvd_stroke, cc_cvd_ascvd, cc_cvd_hf, cc_cvd_any)

# scale continuous variable
df$demo_age_years = scale(df$demo_age_years)

# make binary variables 0/1
df = df %>% 
  mutate(across(c(demo_pregnant, htn_jnc7, htn_aware, bp_med_use, 
                  bp_med_recommended_jnc7,
                  bp_med_ace, bp_med_aldo, bp_med_alpha, bp_med_angioten, 
                  bp_med_beta, bp_med_ccb, bp_med_ccb_dh, bp_med_ccb_ndh, 
                  bp_med_central, bp_med_renin_inhibitors, bp_med_vasod, 
                  bp_med_diur_loop, bp_med_diur_Ksparing,
                  bp_med_diur_thz, cc_diabetes, cc_ckd, cc_cvd_mi, cc_cvd_chd, 
                  cc_cvd_stroke, cc_cvd_ascvd, cc_cvd_hf, cc_cvd_any), 
                ~ as.integer(.x == "Yes")))
df$demo_gender = ifelse(df$demo_gender == "Men", 1, 0)

# select only demographic and comorbidity variables and delete
# rows containing NA's for purposes of HW
df = df %>% select(-starts_with("bp_med")) %>% drop_na()
write.csv(df, "./df.csv")

# fit1 = glm(htn_jnc7 ~ ., data = df, family = binomial)

length(grep("bp_med", names(df))) # number of variables w.r.t. medication use
length(grep("cc", names(df))) # number of variables associated with comorbidity
length(grep("demo", names(df))) # of variables w.r.t. demographics