library(dplyr)
library(cardioStatsUSA)
library(ggplot2)

# select data points that have available hypertension data
# select data points with hypertension
df = nhanes_data %>% filter(svy_subpop_htn == 1,
                            htn_jnc7 == "Yes") 

# sanity check
df %>% 
  group_by(svy_year) %>%
  summarise(bp_control_pct = sum(bp_control_jnc7 == "Yes") / n()) %>%
  ggplot(aes(x = svy_year, y = bp_control_pct)) + geom_point()
## as shown, BP control among patients with hypertension increased steadily 
## until 2009-2010; and has decreased steadily since 2013-2014 survey period.

# select only survey period, demographic, medication use, 
# hypertension awareness, ht-resistant, and comorbidity variables
df = df %>% select(svy_year, demo_age_cat, demo_race, demo_pregnant, demo_gender,
                   bp_control_jnc7, htn_aware, htn_resistant_jnc7, bp_med_use,
                   bp_med_recommended_jnc7, bp_med_n_class, bp_med_n_pills, 
                   bp_med_combination, bp_med_pills_gteq_2, bp_med_ace, 
                   bp_med_aldo, bp_med_alpha, bp_med_angioten, bp_med_beta, 
                   bp_med_ccb, bp_med_central, bp_med_renin_inhibitors, bp_med_vasod,
                   bp_med_diur_loop, bp_med_diur_Ksparing, bp_med_diur_thz,
                   cc_smoke, cc_bmi, cc_diabetes, cc_ckd, cc_cvd_mi, cc_cvd_chd,
                   cc_cvd_stroke, cc_cvd_ascvd, cc_cvd_hf)

# make binary variables 1/0
df = df %>% 
  mutate(across(c(demo_pregnant, bp_control_jnc7, htn_resistant_jnc7, 
                  htn_aware, htn_resistant_jnc7, bp_med_use, 
                  bp_med_recommended_jnc7, bp_med_combination,
                  bp_med_pills_gteq_2, bp_med_ace, bp_med_aldo, bp_med_alpha, 
                  bp_med_angioten, bp_med_beta, bp_med_ccb, bp_med_central, 
                  bp_med_renin_inhibitors, bp_med_vasod, bp_med_diur_loop, 
                  bp_med_diur_Ksparing, bp_med_diur_thz, cc_diabetes, 
                  cc_ckd, cc_cvd_mi, cc_cvd_chd, cc_cvd_stroke, cc_cvd_ascvd, 
                  cc_cvd_hf), 
                ~ as.integer(.x == "Yes")))

# save to directory
write.csv(df, "./data/df.csv")
