library(readr)
library(dplyr)
library(tidyverse)
library(rstan)
library(ggplot2)
library(bayesplot)

library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))


# data wrangling for rstan
df = read_csv("../data/df.csv")[-1] %>%
  mutate(svy_year = case_when(svy_year == "1999-2000" ~ 1,
                              svy_year == "2001-2002" ~ 2,
                              svy_year == "2003-2004" ~ 3, 
                              svy_year == "2005-2006" ~ 4,
                              svy_year == "2007-2008" ~ 5,
                              svy_year == "2009-2010" ~ 6,
                              svy_year == "2011-2012" ~ 7,
                              svy_year == "2013-2014" ~ 8,
                              svy_year == "2015-2016" ~ 9,
                              svy_year == "2017-2020" ~ 10)) %>%
  rename(cc_bmi_ = cc_bmi, demo_age_cat_ = demo_age_cat) %>%
  drop_na() %>% # disregard missed observations
  mutate(cc_bmi_ = case_when(cc_bmi_ == "<25" ~ "Normal",
                             cc_bmi_ == "25 to <30" ~ "Overweight",
                             cc_bmi_ == "30 to <35" ~ "Class_1_Obese",
                             cc_bmi_ == "35+" ~ "Class_2_3_Obese"),
         bp_med_n_pills = case_when(bp_med_n_pills == "Four or more" ~ "Four",
                                    TRUE ~ bp_med_n_pills),
         bp_med_n_class = case_when(bp_med_n_class == "Four or more" ~ "Four",
                                    TRUE ~ bp_med_n_class))

# set reference levels
df$demo_age_cat_ = factor(df$demo_age_cat_, levels = c("18 to 44", "45 to 64", 
                                                       "65 to 74", "75+"))
df$bp_med_n_class = factor(df$bp_med_n_class, levels = c("None", "One", "Two",
                                                         "Three", "Four"))
df$bp_med_n_pills = factor(df$bp_med_n_pills, levels = c("None", "One", "Two",
                                                         "Three", "Four"))
df$cc_smoke = factor(df$cc_smoke, levels = c("Never", "Former", "Current"))
df$cc_bmi_ = factor(df$cc_bmi_, levels = c("Normal", "Overweight", "Class_1_Obese",
                                           "Class_2_3_Obese"))
df$demo_gender = factor(df$demo_gender, levels = c("Women", "Men"))
df$demo_race = factor(df$demo_race, levels = c("Non-Hispanic White", "Hispanic",
                                               "Other", "Non-Hispanic Black",
                                               "Non-Hispanic Asian"))
df_mtx = model.matrix(bp_control_jnc7 ~ ., data = df)[, -1] %>% 
  janitor::clean_names()
write.csv(df, "../data/processed_df.csv")
group_var = as.integer(df$svy_year)
stan_df = list(N = nrow(df_mtx),
               P = ncol(df_mtx) - 1,
               group = group_var,
               X = df_mtx[, !colnames(df_mtx) %in% 
                            c("svy_year")],
               Y = df$bp_control_jnc7)

# run ADVI
m1_stan = stan_model(file = "./model.stan")
fit_advi <- vb(m1_stan,
               data = stan_df,
               iter = 10000,
               tol_rel_obj = 0.0001,
               algorithm = "meanfield")
print(fit_advi)
## switch to MCMC

# run MCMC
fit_mcmc <- stan("./model.stan", 
                 data = stan_df, 
                 iter = 10000, 
                 warmup = 2000,
                 chains = 4)

# convergence diagnostics
range(summary(fit_mcmc)$summary[1:472, 10])
color_scheme_set("mix-blue-pink")
p1 <- mcmc_trace(fit_mcmc,  pars = c("lambda"))
ggsave("./lambda.png",p1)
p2 = mcmc_trace(fit_mcmc, pars = c("beta[5,9]"))
ggsave("./beta_5_9.png", p2)
p3 = mcmc_trace(fit_mcmc, pars = c("beta[21,8]"))
ggsave("./beta_21_8.png", p3)
p4 = mcmc_trace(fit_mcmc, pars = c("beta[18,4]"))
ggsave("./beta_18_4.png", p4)
p5 = mcmc_trace(fit_mcmc, pars = c("beta[9,1]"))
ggsave("./beta_9_1.png", p5)

# extract betas, make into a dataframe
betas = exp(summary(fit_mcmc)$summary[2:471, 1])
covariates = as.numeric(sub("beta\\[(\\d+),.*", "\\1", names(betas)))
year <- as.numeric(sub("beta\\[\\d+,(\\d+)\\]", "\\1", names(betas)))
test_df <- data.frame(
  covariates = covariates,
  year = year,
  value = betas
)
betas_df <- pivot_wider(test_df, 
                        names_from = year, 
                        values_from = value, 
                        id_cols = covariates)
betas_df$covariates = colnames(df_mtx)[-1]
colnames(betas_df) = c("covariates", "1999-2000", "2001-2002", "2003-2004",
                       "2005-2006", "2007-2008", "2009-2010", "2011-2012",
                       "2013-2014", "2015-2016", "2017-2020")
demo_betas = betas_df[1:9, ] %>%
  pivot_longer(cols = -covariates, names_to = "period", values_to = "beta") %>%
  ggplot(aes(x = period, y = beta, group = covariates, color = covariates)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Survey Period", y = "Odds Ratio of BP Control", color = "Covariate",
       title = "B.P. Control v.s. Demographic Covariates")
med_betas = betas_df[10:35, ] %>%
  pivot_longer(cols = -covariates, names_to = "period", values_to = "beta") %>%
  ggplot(aes(x = period, y = beta, group = covariates, color = covariates)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Survey Period", y = "Odds Ratio of BP Control", color = "Covariate",
       title = "B.P. Control v.s. Medication Use-Related Covariates")
cc_betas = betas_df[36:47, ] %>%
  pivot_longer(cols = -covariates, names_to = "period", values_to = "beta") %>%
  ggplot(aes(x = period, y = beta, group = covariates, color = covariates)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Survey Period", y = "Odds Ratio of BP Control", color = "Covariate",
       title = "B.P. Control v.s. Comorbidity Covariates")





