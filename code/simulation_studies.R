# Simulation Study

# Data Generation ----

## Prep ----
source("code/inital_analysis.R")
library(ipumsr)
library(kableExtra)

### education year generation function ----
# extract the list of proportion of each group
edu_prop_data <- cleaned_dat %>% filter(wave == 2009) %>%
  mutate(age_cat = cut(age,4)) %>%
  count(age_cat, edyrs,female) %>%
  group_by(age_cat,female) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(female,age_cat)

get_prop <- function(age, fem){
  edu_prop_data %>% filter(age_cat == age & female == fem) %>% pull(prop)
}
young_m_prop <- get_prop("(25,34.8]", 0)
young_f_prop <- get_prop("(25,34.8]", 1)
mid_m_prop <- get_prop("(34.8,44.5]", 0)
mid_f_prop <- get_prop("(34.8,44.5]", 1)
midold_m_prop <- get_prop("(44.5,54.2]", 0)
midold_f_prop <- get_prop("(44.5,54.2]", 1)
old_m_prop <- get_prop("(54.2,64]", 0)
old_f_prop <- get_prop("(54.2,64]", 1)

# build function
edu_gen <- function(age_cat, fem){
  case_when(age_cat %in% 25:34 & fem == 0 ~ sample(7:18, size = 1, prob = young_m_prop),
            age_cat %in% 25:34 & fem == 1 ~ sample(8:18, size = 1, prob = young_f_prop),
            age_cat %in% 35:44 & fem == 0 ~ sample(6:18, size = 1, prob = mid_m_prop),
            age_cat %in% 35:44 & fem == 1 ~ sample(8:18, size = 1, prob = mid_f_prop),
            age_cat %in% 45:54 & fem == 0 ~ sample(7:18, size = 1, prob = midold_m_prop),
            age_cat %in% 45:54 & fem == 1 ~ sample(8:18, size = 1, prob = midold_f_prop),
            age_cat %in% 55:64 & fem == 0 ~ sample(c(4,7:18), size = 1, prob = old_m_prop),
            age_cat %in% 55:64 & fem == 1 ~ sample(c(8:18), size = 1, prob = old_f_prop),
            .default = NA)
}

### when give first born ----
age_birth <- read_delim("data/Natality, 2007-2023.txt")
cleaned_age_birth <- age_birth %>% filter(is.na(Notes) == TRUE)
mean_age <- cleaned_age_birth %>% pull(`Average Age of Mother`)
sd_age <- cleaned_age_birth %>% pull(`Standard Deviation for Average Age of Mother`)

ne_age_birth <- function(eduyr){
  n = length(eduyr) # why?
  birth_age <- rnorm(n,0,1)
  case_when(eduyr <= 8 ~ birth_age * sd_age[1] + mean_age[1],
            eduyr > 8 & eduyr < 12 ~ birth_age * sd_age[2] + mean_age[2],
            eduyr == 12 ~ birth_age * sd_age[3] + mean_age[3],
            eduyr == 13 ~ birth_age * sd_age[4] + mean_age[4],
            eduyr == 14 ~ birth_age * sd_age[5] + mean_age[5],
            eduyr > 14 & eduyr <= 16 ~ birth_age * sd_age[6] + mean_age[6],
            eduyr > 16 & eduyr <= 18 ~ birth_age * sd_age[7] + mean_age[7],
            eduyr > 18 ~ birth_age * sd_age[8] + mean_age[8], .default = NA)
}

mw_age_birth <- function(eduyr){
  n = length(eduyr)
  birth_age <- rnorm(n,0,1)
  case_when(eduyr <= 8 ~ birth_age * sd_age[9] + mean_age[9],
            eduyr > 8 & eduyr < 12 ~ birth_age * sd_age[10] + mean_age[10],
            eduyr == 12 ~ birth_age * sd_age[11] + mean_age[11],
            eduyr == 13 ~ birth_age * sd_age[12] + mean_age[12],
            eduyr == 14 ~ birth_age * sd_age[13] + mean_age[13],
            eduyr > 14 & eduyr <= 16 ~ birth_age * sd_age[14] + mean_age[14],
            eduyr > 16 & eduyr <= 18 ~ birth_age * sd_age[15] + mean_age[15],
            eduyr > 18 ~ birth_age * sd_age[16] + mean_age[16], .default = NA)
}

south_age_birth <- function(eduyr){
  n = length(eduyr)
  birth_age <- rnorm(n,0,1)
  case_when(eduyr <= 8 ~ birth_age * sd_age[17] + mean_age[17],
            eduyr > 8 & eduyr < 12 ~ birth_age * sd_age[18] + mean_age[18],
            eduyr == 12 ~ birth_age * sd_age[19] + mean_age[19],
            eduyr == 13 ~ birth_age * sd_age[20] + mean_age[20],
            eduyr == 14 ~ birth_age * sd_age[21] + mean_age[21],
            eduyr > 14 & eduyr <= 16 ~ birth_age * sd_age[22] + mean_age[22],
            eduyr > 16 & eduyr <= 18 ~ birth_age * sd_age[23] + mean_age[23],
            eduyr > 18 ~ birth_age * sd_age[24] + mean_age[24], .default = NA)
}

west_age_birth <- function(eduyr){
  n = length(eduyr)
  birth_age <- rnorm(n,0,1)
  #translate mean, sd
  case_when(eduyr <= 8 ~ birth_age * sd_age[25] + mean_age[25],
            eduyr > 8 & eduyr < 12 ~ birth_age * sd_age[26] + mean_age[26],
            eduyr == 12 ~ birth_age * sd_age[27] + mean_age[27],
            eduyr == 13 ~ birth_age * sd_age[28] + mean_age[28],
            eduyr == 14 ~ birth_age * sd_age[29] + mean_age[29],
            eduyr > 14 & eduyr <= 16 ~ birth_age * sd_age[30] + mean_age[30],
            eduyr > 16 & eduyr <= 18 ~ birth_age * sd_age[31] + mean_age[31],
            eduyr > 18 ~ birth_age * sd_age[32] + mean_age[32], .default = NA)
}

### num of children ----
# the probabilities of females having no children, conditional on education level (source: ipums)

# ddi <- read_ipums_ddi("data/usa_00006.xml")
# ipums_dat <- read_ipums_micro(ddi)
# ipums_dat <- ipums_dat %>% select(YEAR, SEX, AGE, PERWT, NCHILD, EDUC)
# cleaned_ipums <- ipums_dat %>% 
#   filter(YEAR %in% c(2012, 2013, 2014) & AGE == 40 & EDUC != 99 & EDUC != 0 & SEX == 2) %>% # women at their age of 40
#   mutate(eduyr = EDUC + 6) %>% 
#   mutate(edu_level = case_when(
#     eduyr < 12 ~ 1, # less than high school grad
#     eduyr >= 12 & eduyr < 16 ~ 2, # high school grad - some college
#     eduyr == 16 ~ 3, # bachelor degree
#     eduyr > 16 ~ 4, # grad school or higher
#     TRUE ~ 0
#   )) %>% group_by(NCHILD, edu_level) %>% 
#   summarize(num_pp = sum(PERWT))
# write_csv(cleaned_ipums, "data/ipums_data_1.csv")
cleaned_ipums <- read_csv("data/ipums_data_1.csv")
cleaned_ipums %>% 
  group_by(edu_level) %>% 
  summarize(num_child = NCHILD, prop = num_pp/sum(num_pp)) %>% 
  filter(num_child == 0)

# the probability of mothers have 1-4 children based on education level (source: https://www.pewresearch.org/social-trends/2015/05/07/family-size-among-mothers/)
less_high_prob <- c(0.13, 0.32, 0.29, 0.26) # less than high school grad
high_grad_prob <- c(0.23, 0.39, 0.24, 0.14) # high school grad - some college
ba_prob <- c(0.22, 0.46, 0.22, 0.10) # bachelor degree
grad_prob <- c(0.23, 0.50, 0.19, 0.8) # grad school or higher

# build function
num_child_gen <- function(edu, fem){
  case_when(
    edu < 12 & fem == 1 ~ sample(1:4, size = 1, prob = less_high_prob),
    edu >= 12 & edu < 16 & fem == 1 ~ sample(1:4, size = 1, prob = high_grad_prob),
    edu == 16 & fem == 1 ~ sample(1:4, size = 1, prob = ba_prob),
    edu > 16 & fem == 1 ~ sample(1:4, size = 1, prob = grad_prob), 
    TRUE ~ 0
  )
}

### Occupations ----
# categorize the occ income score into three parts based on it's distribution, look at the share of people in each occ category group by education level

# ddi_1 <- read_ipums_ddi("data/usa_00005.xml")
# ipums_dat_1 <- read_ipums_micro(ddi_1)
# cleaned_ipums_1 <- ipums_dat_1 %>% filter(YEAR == 2012 & EDUC != 99 & EDUC != 0 & OCCSCORE != 0) %>% mutate(eduyr = EDUC + 6) %>% select(PERWT, eduyr, OCCSCORE)
# write_csv(cleaned_ipums_1, "data/ipums_data_2.csv")
cleaned_ipums_1 <- read_csv("data/ipums_data_2.csv")
cleaned_ipums_1 %>% ggplot(aes(x = OCCSCORE)) + geom_density()
# split the occ score into 3 parts based on the distribution
cleaned_ipums_1 %>% 
  mutate(occ_level = case_when(
    OCCSCORE > 0 & OCCSCORE <=25 ~ 3, # low-income occupation
    OCCSCORE > 25 & OCCSCORE <= 42 ~ 2, # med-income occupation
    TRUE ~ 1 # high-income occupation
  )) %>% 
  mutate(edu_level = case_when(
    eduyr < 12 ~ 1, # less than high school grad
    eduyr >= 12 & eduyr < 16 ~ 2, # high school grad - some college
    eduyr == 16 ~ 3, # bachelor degree
    eduyr > 16 ~ 4, # grad school or higher
    TRUE ~ 0
  )) %>% group_by(edu_level, occ_level) %>% summarize(num_pp = sum(PERWT)) %>% 
  group_by(edu_level) %>% summarize(occ = occ_level, prop = num_pp/sum(num_pp))

# Source: IPUMS Probabilities correpond to high, median, and low income occupation
occ_less_high_prob <- c(0.0008, 0.172, 0.827) # less than high school grad
occ_high_prob <- c(0.0112, 0.324, 0.664) # high school grad - some college
occ_ba_prob <- c(0.0577, 0.568, 0.375) # bachelor degree
occ_grad_prob <- c(0.172, 0.635, 0.193) # grad school or higher

# build function
occ_gen <- function(edu){
  case_when(
    edu < 12 ~ sample(1:3, size = 1, prob = occ_less_high_prob), # less than high school grad
    edu >= 12 & edu < 16 ~ sample(1:3, size = 1, prob = occ_high_prob), # high school grad - some college
    edu == 16 ~ sample(1:3, size = 1, prob = occ_ba_prob), # bachelor degree
    edu > 16 ~ sample(1:3, size = 1, prob = occ_grad_prob), # grad school or higher
    TRUE ~ 0
  )
}


## Simulated Dataset ----
data_gen <- function(n, true_ratio){
  tibble(
    female = rbinom(n, size = 1, prob = 0.5),
    age = round(runif(n, min = 25, max = 64)),
    region = round(runif(n, min = 1, max = 4)),
  ) %>%
    
    # region
    mutate(
      northeast = if_else(region == 1, 1, 0),
      midwest = if_else(region == 2, 1, 0),
      south = if_else(region == 3, 1, 0),
      west = if_else(region == 4, 1, 0)
    ) %>%
    
    # education
    mutate(
      eduyrs = as.numeric(map2(age, female, ~edu_gen(age_cat = .x, fem = .y))),
      ba = if_else(eduyrs >= 16, 1, 0),
      adv = if_else(eduyrs > 16, 1, 0),
      expf = age - eduyrs - 6
    ) %>%
    
    # childbearing
    mutate(
      age_birth = case_when(female == 1 & region == 1 ~ ne_age_birth(eduyrs),
                            female == 1 & region == 2 ~ mw_age_birth(eduyrs),
                            female == 1 & region == 3 ~ south_age_birth(eduyrs),
                            female == 1 & region == 4 ~ west_age_birth(eduyrs),
                            .default = 0),
      age_birth = ifelse(age_birth < 16, 16, age_birth),
      age_birth = round(age_birth),
      #birth_detect = ifelse(female == 1 & runif(n()) <= 0.17, 0, 1),
      birth_detect = case_when(
        female == 0 ~ 0,
        eduyrs < 12 & female == 1 & runif(n()) <= 0.209 ~ 0,
        eduyrs >= 12 & eduyrs < 16 & female == 1 & runif(n()) <= 0.258 ~ 0,
        eduyrs == 16 & female == 1 & runif(n()) <= 0.259 ~ 0,
        eduyrs > 16 & female == 1 & runif(n()) <= 0.266 ~ 0,
        TRUE ~ 1
      ), # assume children won't affect male, the probabilities are getting from ipums
      exp_num_child = as.numeric(map2(eduyrs, female*birth_detect, ~num_child_gen(edu = .x, fem = .y))),
      num_child = case_when(
        birth_detect == 0 ~ 0, # 17% women choose not to give birth
        age < age_birth ~ 0, # current age younger than expected first birth age
        (age-age_birth)%/%2 < exp_num_child ~ (age-age_birth)%/%2, # women haven't reached to their end of fertility
        TRUE ~ exp_num_child
      )
    ) %>%
    
    # occupations
    mutate(
      occ = map_int(eduyrs, occ_gen),
      high_occ = ifelse(occ == 1, 1, 0),
      med_occ = ifelse(occ == 2, 1, 0),
      low_occ = ifelse(occ == 3, 1, 0)
    ) %>%
    # children have no effect on people who are at high-income occupations, as day care is affordable for them
    # For med and low-income occupations, one child will cause 2 years reduction in work
    # make guess on how many women not working, how many women work part time.
    mutate(
      expf = case_when(
        occ %in% c(2, 3) ~ expf-0.5*num_child, TRUE ~ expf
      ),
      expf_sq = expf^2,
      expp = 2*num_child,
      expp_sq = expp^2
    ) %>%
    
    # log income (simplify that the unexplained gap is only coming from the variable female)
    mutate(
      lnwage = 1.74 + log(true_ratio)*female + 0.07*eduyrs + 0.04*expf - 0.0006*expf_sq + 0.1*ba + 0.2*adv + 0.02*northeast - 0.07*midwest - 0.05*south + 0.6*high_occ - 0.2*low_occ + rnorm(nrow(.),sd = .3)
    )
}

# sim_data_test <- data_gen(500, 0.7)

if(FALSE){
  mod <- glm(female ~ eduyrs + expf + expf_sq + ba + adv + northeast + midwest + south + high_occ + low_occ, data =  sim_data_test, family = "quasibinomial")
  sim_data_test %>%
    mutate(pred = mod %>% predict(type='response')) %>% 
    ggplot(aes(x = pred, fill = factor(female))) + 
    geom_density(alpha = 0.5) +
    labs(x = 'Predicted Probability of Being Female', fill = 'Female')
}

# Estimation Process ----
sim_fit_model <- function(dat, type){
  if(type == 'full'){
    lm(lnwage ~ eduyrs + expf + expf_sq + ba + adv + northeast + midwest + south + high_occ + low_occ, data = dat %>% filter(female == 0))
  }else if(type == "hc"){
    lm(lnwage ~ eduyrs + expf, data = dat %>% filter(female == 0))
  }else if(type == "unadjust"){
    lm(lnwage ~ 1, data = dat %>% filter(female == 0))
  }
}

sim_fit_prop_model <- function(dat, type){
  if(type == 'full'){
    glm(female ~ eduyrs + expf + expf_sq + ba + adv + northeast + midwest + south + high_occ + low_occ, data = dat, family = "quasibinomial")
  }else if(type == "hc"){
    glm(female ~ eduyrs + expf, data = dat, family = "quasibinomial")
  }else if(type == "unadjust"){
    glm(female ~ 1, data = dat, family = "quasibinomial")
  }
}

estimate <- function(df, adjustRI, adjustW){
  mod_RI <- sim_fit_model(dat = df, type = adjustRI)
  ps_mod <- sim_fit_prop_model(dat = df, type = adjustW)
  
  ## Regression Imputation
  sim_data_RI <- df %>% mutate(pred_M = predict(mod_RI, newdata = df))
  RI_out <- sim_data_RI %>% filter(female == 1) %>%
    summarise(mean_female_lnwage = mean(lnwage), cf_RI = mean(pred_M)) %>%
    mutate(ratio_RI = exp(mean_female_lnwage - cf_RI)) %>% select(cf_RI, ratio_RI)
  
  ## Weighting
  sim_data_W <- df %>%
    mutate(fem_prb = mean(female),
           ps = predict(ps_mod, newdata = df, type = "response")) %>%
    mutate(odds = ps/(1 - ps),
           wt = ifelse(female == 1, 0, odds*(1/fem_prb)))
  W_out <- sim_data_W %>%
    summarize(cf_W = sum(wt*lnwage)/sum(wt),
              mean_female_lnwage = sum(female*lnwage)/sum(female)) %>%
    mutate(ratio_W = exp(mean_female_lnwage - cf_W)) %>% select(cf_W, ratio_W)
  
  ## Doubly Robust
  sim_data_DR <- df %>%
    mutate(pred_M = predict(mod_RI, newdata = df)) %>%
    mutate(cf_RI = sum(female*pred_M)/sum(female)) %>%
    mutate(fem_prb = mean(female),
           ps = predict(ps_mod, newdata = df, type = "response")) %>%
    mutate(odds = ps/(1 - ps),
           wt = ifelse(female == 1, 0, odds*(1/fem_prb))) %>%
    mutate(DR_1 = wt*(lnwage - pred_M),
           DR_2 = if_else(female == 1, (pred_M - cf_RI)/fem_prb, 0),
           DR_3 = cf_RI,
           DR = DR_1 + DR_2 + DR_3)
  DR_out <- sim_data_DR %>%
    summarize(cf_DR = mean(DR),
              mean_female_lnwage = sum(female*lnwage)/sum(female)) %>%
    mutate(ratio_DR = exp(mean_female_lnwage - cf_DR)) %>% select(cf_DR, ratio_DR)
  
  cbind(RI_out, W_out, DR_out)
}

# estimate(sim_data_test, "unadjust", "unadjust")

## Bootstrapping ----
# sim_data_boot <- data_gen(500, 0.7)
#bootstrapping (B = 500) on 1 simulated data to get bootstrap SE
# BOOT <- map(1:500,function(i){ sample_n(sim_data_boot,size = nrow(sim_data_boot), replace = TRUE)}) %>%
#   map(estimate, adjustRI = "full", adjustW = "full") %>%
#   list_rbind() 
# 
# BOOT %>%
#   summarize(sd_RI = sd(ratio_RI),sd_W = sd(ratio_W),sd_DR = sd(ratio_DR))
# BOOT %>%
#   summarize(lb_RI = quantile(ratio_RI,0.025), ub_RI = quantile(ratio_RI,0.975),
#             lb_W = quantile(ratio_W,0.025), ub_W = quantile(ratio_W,0.975),
#             lb_DR = quantile(ratio_DR,0.025), ub_DR = quantile(ratio_DR,0.975))

boot_strap <- function(df, adjRI, adjW){
  BOOT <- map(1:500,function(i){ sample_n(df,size = nrow(df), replace = TRUE)}) %>%
    map(estimate, adjustRI = adjRI, adjustW = adjW) %>%
    list_rbind() 
  BOOT %>%
    summarize(SE_Boot_RI = round(sd(ratio_RI), 4),SE_Boot_W = round(sd(ratio_W), 4),SE_Boot_DR = round(sd(ratio_DR), 4))
}
#boot_strap(sim_data_boot, "full", "full")


## Combined function ----
combined <- function(iter, n, true_ratio, adjustRI, adjustW){
  # Data generation
  sim_data <- tibble(
    female = rbinom(n, size = 1, prob = 0.5),
    age = round(runif(n, min = 25, max = 64)),
    region = round(runif(n, min = 1, max = 4)),
  ) %>% 
    
    ## region
    mutate(
      northeast = if_else(region == 1, 1, 0),
      midwest = if_else(region == 2, 1, 0),
      south = if_else(region == 3, 1, 0),
      west = if_else(region == 4, 1, 0)
    ) %>% 
    
    ## education
    mutate(
      eduyrs = as.numeric(map2(age, female, ~edu_gen(age_cat = .x, fem = .y))),
      ba = if_else(eduyrs >= 16, 1, 0),
      adv = if_else(eduyrs > 16, 1, 0),
      expf = age - eduyrs - 6
    ) %>% 
    
    ## childbearing
    mutate(
      age_birth = case_when(female == 1 & region == 1 ~ ne_age_birth(eduyrs),
                            female == 1 & region == 2 ~ mw_age_birth(eduyrs),
                            female == 1 & region == 3 ~ south_age_birth(eduyrs),
                            female == 1 & region == 4 ~ west_age_birth(eduyrs),
                            .default = 0),
      age_birth = ifelse(age_birth < 16, 16, age_birth),
      age_birth = round(age_birth),
      birth_detect = case_when(
        female == 0 ~ 0,
        eduyrs < 12 & female == 1 & runif(n()) <= 0.209 ~ 0,
        eduyrs >= 12 & eduyrs < 16 & female == 1 & runif(n()) <= 0.258 ~ 0,
        eduyrs == 16 & female == 1 & runif(n()) <= 0.259 ~ 0,
        eduyrs > 16 & female == 1 & runif(n()) <= 0.266 ~ 0, 
        TRUE ~ 1
      ), # assume children won't affect male, the probabilities are getting from ipums
      exp_num_child = as.numeric(map2(eduyrs, female*birth_detect, ~num_child_gen(edu = .x, fem = .y))),
      num_child = case_when(
        birth_detect == 0 ~ 0, # 17% women choose not to give birth
        age < age_birth ~ 0, # current age younger than expected first birth age
        (age-age_birth)%/%2 < exp_num_child ~ (age-age_birth)%/%2, # women haven't reached to their end of fertility
        TRUE ~ exp_num_child
      )
    ) %>%
    
    ## occupations
    mutate(
      occ = map_int(eduyrs, occ_gen),
      high_occ = ifelse(occ == 1, 1, 0),
      med_occ = ifelse(occ == 2, 1, 0),
      low_occ = ifelse(occ == 3, 1, 0)
    ) %>% 
    # children have no effect on people who are at high-income occupations, as day care is affordable for them 
    # For med and low-income occupations, one child will cause 2 years reduction in work
    # make guess on how many women not working, how many women work part time.
    mutate(
      expf = case_when(
        occ %in% c(2, 3) ~ expf-0.5*num_child, TRUE ~ expf
      ),
      expf_sq = expf^2,
      expp = 2*num_child,
      expp_sq = expp^2
    ) %>% 
    
    ## log income (simplify that the unexplained gap is only coming from the variable female)
    mutate(
      lnwage = 1.74 + log(true_ratio)*female + 0.07*eduyrs + 0.04*expf - 0.0006*expf_sq + 0.1*ba + 0.2*adv + 0.02*northeast - 0.07*midwest - 0.05*south + 0.6*high_occ - 0.2*low_occ + rnorm(nrow(.),sd = .3)
    )
  
  
  # Estimation process
  # mod_RI <- lm(lnwage ~ eduyrs + expf + expf_sq + ba + adv + northeast + midwest + south + high_occ + low_occ, data = sim_data %>% filter(female == 0))
  # ps_mod <- glm(female ~ eduyrs + expf + expf_sq + ba + adv + northeast + midwest + south + high_occ + low_occ, data = sim_data, family = "quasibinomial")
  mod_RI <- sim_fit_model(dat = sim_data, type = adjustRI)
  ps_mod <- sim_fit_prop_model(dat = sim_data, type = adjustW)
  
  ## Regression Imputation
  sim_data_RI <- sim_data %>% mutate(pred_M = predict(mod_RI, newdata = sim_data))
  RI_out <- sim_data_RI %>% filter(female == 1) %>% 
    summarise(mean_female_lnwage = mean(lnwage), cf_RI = mean(pred_M)) %>% 
    mutate(ratio_RI = exp(mean_female_lnwage - cf_RI)) %>% select(cf_RI, ratio_RI)
  
  ## Weighting
  sim_data_W <- sim_data %>% 
    mutate(fem_prb = mean(female),
           ps = predict(ps_mod, newdata = sim_data, type = "response")) %>% 
    mutate(odds = ps/(1 - ps),
           wt = ifelse(female == 1, 0, odds*(1/fem_prb)))
  W_out <- sim_data_W %>% 
    summarize(cf_W = sum(wt*lnwage)/sum(wt), 
              mean_female_lnwage = sum(female*lnwage)/sum(female)) %>%
    mutate(ratio_W = exp(mean_female_lnwage - cf_W)) %>% select(cf_W, ratio_W)
  
  ## Doubly Robust
  sim_data_DR <- sim_data %>% 
    mutate(pred_M = predict(mod_RI, newdata = sim_data)) %>% 
    mutate(cf_RI = sum(female*pred_M)/sum(female)) %>%
    mutate(fem_prb = mean(female),
           ps = predict(ps_mod, newdata = sim_data, type = "response")) %>% 
    mutate(odds = ps/(1 - ps),
           wt = ifelse(female == 1, 0, odds*(1/fem_prb))) %>% 
    mutate(DR_1 = wt*(lnwage - pred_M),
           DR_2 = if_else(female == 1, (pred_M - cf_RI)/fem_prb, 0),
           DR_3 = cf_RI,
           DR = DR_1 + DR_2 + DR_3)
  DR_out <- sim_data_DR %>% 
    summarize(cf_DR = mean(DR),
              mean_female_lnwage = sum(female*lnwage)/sum(female)) %>% 
    mutate(ratio_DR = exp(mean_female_lnwage - cf_DR)) %>% select(cf_DR, ratio_DR)
  
  ## Output combined
  output <- cbind(RI_out, W_out, DR_out) %>% mutate(iter = iter, n  = n, true_ratio = true_ratio)
  return(output)
}



## Different cases ----
# data size n, repeated times seeds, true ratio, different adjustments of models

# set.seed(123)
# start_time <- 
#   system.time(map(1:10, data_gen_sim, n = 500, true_ratio = 0.7, adjustRI = "full", adjustW = "full"))

### n = 500, truth = 0.7 ----
# repeated 1000 times, data size = 500, truth = 0.7, full specification model
# sim_data_1 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "full", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_1, "data/sim_data/sim_data_1.csv")
sim_data_1 <- read_csv("data/sim_data/sim_data_1.csv")

sim_data_1 %>% 
  ggplot() +
  geom_density(aes(x = ratio_RI), color = "#4e83bd", linewidth = 2.2) +
  geom_density(aes(x = ratio_W), color = "#c1524d", linewidth = 1.2) +
  geom_density(aes(x = ratio_DR), color = "#9dbb59", linewidth = 0.8) +
  geom_vline(xintercept = 0.7, color = "grey") +
  geom_vline(xintercept = mean(sim_data_1$ratio_RI), color = "#4e83bd") +
  geom_vline(xintercept = mean(sim_data_1$ratio_W), color = "#c1524d") +
  geom_vline(xintercept = mean(sim_data_1$ratio_DR), color = "#9dbb59") +
  xlim(0.6,0.8)+
  theme_classic() +
  labs(x = "female-male wage ratio", y = NULL)

est_1 <- sim_data_1 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

sim_data_boot_1 <- data_gen(500, 0.7)
boot_1 <- boot_strap(sim_data_boot_1, adjRI = "full", adjW = "full")

case_1 <- cbind(est_1, boot_1) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)


# repeated 1000 times, data size = 500, truth = 0.7, inaccurate specification models
# sim_data_2 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "hc", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_2, "data/sim_data/sim_data_2.csv")
sim_data_2 <- read_csv("data/sim_data/sim_data_2.csv")

sim_data_2 %>% 
  ggplot() +
  geom_density(aes(x = ratio_RI), color = "#4e83bd", linewidth = 2.2) +
  geom_density(aes(x = ratio_W),color = "#c1524d", linewidth = 1.2) +
  geom_density(aes(x = ratio_DR), color = "#9dbb59", linewidth = 0.8) +
  geom_vline(xintercept = 0.7, color = "black") +
  geom_vline(xintercept = mean(sim_data_2$ratio_RI), color = "#4e83bd") +
  geom_vline(xintercept = mean(sim_data_2$ratio_W), color = "#c1524d") +
  geom_vline(xintercept = mean(sim_data_2$ratio_DR), color = "#9dbb59") +
  xlim(0.6,0.8)+
  theme_classic() +
  labs(x = "female-male wage ratio", y = NULL)

est_2 <- sim_data_2 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_2 <- data_gen(500, 0.7)
boot_2 <- boot_strap(sim_data_boot_1, adjRI = "hc", adjW = "hc")

case_2 <- cbind(est_2, boot_2) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.7, no specification models
# sim_data_3 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "unadjust", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_3, "data/sim_data/sim_data_3.csv")
sim_data_3 <- read_csv("data/sim_data/sim_data_3.csv")

sim_data_3 %>% 
  ggplot() +
  geom_density(aes(x = ratio_RI), color = "#4e83bd", fill = "#4e83bd", alpha = .2) +
  geom_density(aes(x = ratio_W),color = "#c1524d", fill = "#c1524d", alpha = .2) +
  geom_density(aes(x = ratio_DR), color = "#9dbb59", fill = "#9dbb59", alpha = .2) +
  geom_vline(xintercept = mean(sim_data_3$ratio_DR), color = "#9dbb59") +
  geom_vline(xintercept = 0.7, color = "black") +
  xlim(0.6,0.8)+
  theme_classic() +
  labs(x = "ratio", y = NULL)

est_3 <- sim_data_3 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_3 <- data_gen(500, 0.7)
boot_3 <- boot_strap(sim_data_boot_1, adjRI = "unadjust", adjW = "unadjust")

case_3 <- cbind(est_3, boot_3) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.7, RI unadjust, W full
# sim_data_4 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "unadjust", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_4, "data/sim_data/sim_data_4.csv")
sim_data_4 <- read_csv("data/sim_data/sim_data_4.csv")

sim_data_4 %>% 
  ggplot() +
  geom_density(aes(x = ratio_RI), color = "#4e83bd", fill = "#4e83bd", alpha = .2, linewidth = 0.8) +
  geom_density(aes(x = ratio_W), color = "#c1524d", fill = "#c1524d", alpha = .2, linewidth = 2) +
  geom_density(aes(x = ratio_DR), color = "#9dbb59", fill = "#9dbb59", alpha = .2, linewidth = 0.8) +
  geom_vline(xintercept = 0.7, color = "black") +
  geom_vline(xintercept = mean(sim_data_4$ratio_RI), color = "#4e83bd") +
  geom_vline(xintercept = mean(sim_data_4$ratio_W), color = "#c1524d") +
  geom_vline(xintercept = mean(sim_data_4$ratio_DR), color = "#9dbb59") +
  xlim(0.6,0.8)+
  theme_classic() +
  labs(x = "female-male wage ratio", y = NULL)

est_4 <- sim_data_4 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_4 <- data_gen(500, 0.7)
boot_4 <- boot_strap(sim_data_boot_1, adjRI = "unadjust", adjW = "full")

case_4 <- cbind(est_4, boot_4) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.7, RI full, W unadjust
# sim_data_5 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "full", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_5, "data/sim_data/sim_data_5.csv")
sim_data_5 <- read_csv("data/sim_data/sim_data_5.csv")

sim_data_5 %>% 
  ggplot() +
  geom_density(aes(x = ratio_RI), color = "#4e83bd", fill = "#4e83bd", alpha = .2) +
  geom_density(aes(x = ratio_W), color = "#c1524d", fill = "#c1524d", alpha = .2) +
  geom_density(aes(x = ratio_DR), color = "#9dbb59", fill = "#9dbb59", alpha = .2) +
  geom_vline(xintercept = 0.7, color = "black") +
  geom_vline(xintercept = mean(sim_data_5$ratio_RI), color = "#4e83bd") +
  geom_vline(xintercept = mean(sim_data_5$ratio_W), color = "#c1524d") +
  geom_vline(xintercept = mean(sim_data_5$ratio_DR), color = "#9dbb59") +
  xlim(0.6,0.8)+
  theme_classic() +
  labs(x = "ratio", y = NULL)

est_5 <- sim_data_5 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_5 <- data_gen(500, 0.7)
boot_5 <- boot_strap(sim_data_boot_1, adjRI = "full", adjW = "unadjust")

case_5 <- cbind(est_5, boot_5) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.7, RI inaccurate, W unadjust
# sim_data_6 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "hc", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_6, "data/sim_data/sim_data_6.csv")
sim_data_6 <- read_csv("data/sim_data/sim_data_6.csv")

est_6 <- sim_data_6 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_6 <- data_gen(500, 0.7)
boot_6 <- boot_strap(sim_data_boot_1, adjRI = "hc", adjW = "unadjust")

case_6 <- cbind(est_6, boot_6) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.7, RI unadjust, W inaccurate
# sim_data_7 <- map(1:1000, combined, n = 500, true_ratio = 0.7, adjustRI = "unadjust", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_7, "data/sim_data/sim_data_7.csv")
sim_data_7 <- read_csv("data/sim_data/sim_data_7.csv")

est_7 <- sim_data_7 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_7 <- data_gen(500, 0.7)
boot_7 <- boot_strap(sim_data_boot_1, adjRI = "unadjust", adjW = "hc")

case_7 <- cbind(est_7, boot_7) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)


### n = 500, truth = 0.9 ----
# repeated 1000 times, data size = 500, truth = 0.9, full specification model
# sim_data_8 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "full", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_8, "data/sim_data/sim_data_8.csv")
sim_data_8 <- read_csv("data/sim_data/sim_data_8.csv")

est_8 <- sim_data_8 %>%
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4)) 
# %>%
#   mutate(SE_Boot_RI = sd(BOOT3$ratio_RI),SE_Boot_W = sd(BOOT3$ratio_W),SE_Boot_DR = sd(BOOT3$ratio_DR)) %>%
#   select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

#bootstrapping (B = 500) on 1 simulated data to get bootstrap SE
# sim_data_boot_8 <- data_gen(500, 0.9)
# BOOT_8 <- map(1:500,function(i){ sample_n(sim_data_boot_8,size = nrow(sim_data_boot_8), replace = TRUE)}) %>%
#   map(estimate(adjustRI = "full", adjustW = "full")) %>%
#   list_rbind() 

sim_data_boot_2 <- data_gen(500, 0.9)
boot_8 <- boot_strap(sim_data_boot_2, adjRI = "full", adjW = "full")

case_8 <- cbind(est_8, boot_8) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.9, inaccurate specification models
# sim_data_9 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "hc", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_9, "data/sim_data/sim_data_9.csv")
sim_data_9 <- read_csv("data/sim_data/sim_data_9.csv")

est_9 <- sim_data_9 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_9 <- data_gen(500, 0.9)
boot_9 <- boot_strap(sim_data_boot_2, adjRI = "hc", adjW = "hc")

case_9 <- cbind(est_9, boot_9) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.9, no specification models
# sim_data_10 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "unadjust", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_10, "data/sim_data/sim_data_10.csv")
sim_data_10 <- read_csv("data/sim_data/sim_data_10.csv")

est_10 <- sim_data_10 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_10 <- data_gen(500, 0.9)
boot_10 <- boot_strap(sim_data_boot_2, adjRI = "unadjust", adjW = "unadjust")

case_10 <- cbind(est_10, boot_10) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.9, RI unadjust, W full
# sim_data_11 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "unadjust", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_11, "data/sim_data/sim_data_11.csv")
sim_data_11 <- read_csv("data/sim_data/sim_data_11.csv")

est_11 <- sim_data_11 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_11 <- data_gen(500, 0.9)
boot_11 <- boot_strap(sim_data_boot_2, adjRI = "unadjust", adjW = "full")

case_11 <- cbind(est_11, boot_11) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.9, RI full, W unadjust
# sim_data_12 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "full", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_12, "data/sim_data/sim_data_12.csv")
sim_data_12 <- read_csv("data/sim_data/sim_data_12.csv")

est_12 <- sim_data_12 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_12 <- data_gen(500, 0.9)
boot_12 <- boot_strap(sim_data_boot_2, adjRI = "full", adjW = "unadjust")

case_12 <- cbind(est_12, boot_12) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.9, RI inaccurate, W unadjust
# sim_data_13 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "hc", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_13, "data/sim_data/sim_data_13.csv")
sim_data_13 <- read_csv("data/sim_data/sim_data_13.csv")

est_13 <- sim_data_13 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_13 <- data_gen(500, 0.9)
boot_13 <- boot_strap(sim_data_boot_2, adjRI = "hc", adjW = "unadjust")

case_13 <- cbind(est_13, boot_13) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, data size = 500, truth = 0.9, RI unadjust, W inaccurate
# sim_data_14 <- map(1:1000, combined, n = 500, true_ratio = 0.9, adjustRI = "unadjust", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_14, "data/sim_data/sim_data_14.csv")
sim_data_14 <- read_csv("data/sim_data/sim_data_14.csv")

est_14 <- sim_data_14 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_14 <- data_gen(500, 0.9)
boot_14 <- boot_strap(sim_data_boot_2, adjRI = "unadjust", adjW = "hc")

case_14 <- cbind(est_14, boot_14) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)


### n = 250, truth = 0.7 ----
# repeated 1000 times, n = 250, truth = 0.7, RI full, W full
# sim_data_15 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "full", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_15, "data/sim_data/sim_data_15.csv")
sim_data_15 <- read_csv("data/sim_data/sim_data_15.csv")

est_15 <- sim_data_15 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

sim_data_boot_3 <- data_gen(250, 0.7)
boot_15 <- boot_strap(sim_data_boot_3, adjRI = "full", adjW = "full")

case_15 <- cbind(est_15, boot_15) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.7, RI inaccurate, W inaccurate
# sim_data_16 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "hc", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_16, "data/sim_data/sim_data_16.csv")
sim_data_16 <- read_csv("data/sim_data/sim_data_16.csv")

est_16 <- sim_data_16 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_16 <- data_gen(250, 0.7)
boot_16 <- boot_strap(sim_data_boot_3, adjRI = "hc", adjW = "hc")

case_16 <- cbind(est_16, boot_16) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.7, RI unadjust, W unadjust
# sim_data_17 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "unadjust", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_17, "data/sim_data/sim_data_17.csv")
sim_data_17 <- read_csv("data/sim_data/sim_data_17.csv")

est_17 <- sim_data_17 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_17 <- data_gen(250, 0.7)
boot_17 <- boot_strap(sim_data_boot_3, adjRI = "unadjust", adjW = "unadjust")

case_17 <- cbind(est_17, boot_17) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.7, RI unadjust, W full
# sim_data_18 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "unadjust", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_18, "data/sim_data/sim_data_18.csv")
sim_data_18 <- read_csv("data/sim_data/sim_data_18.csv")

est_18 <- sim_data_18 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_18 <- data_gen(250, 0.7)
boot_18 <- boot_strap(sim_data_boot_3, adjRI = "unadjust", adjW = "full")

case_18 <- cbind(est_18, boot_18) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.7, RI full, W unadjust
# sim_data_19 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "full", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_19, "data/sim_data/sim_data_19.csv")
sim_data_19 <- read_csv("data/sim_data/sim_data_19.csv")

est_19 <- sim_data_19 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_19 <- data_gen(250, 0.7)
boot_19 <- boot_strap(sim_data_boot_3, adjRI = "full", adjW = "unadjust")

case_19 <- cbind(est_19, boot_19) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.7, RI inaccurate, W unadjust
# sim_data_20 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "hc", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_20, "data/sim_data/sim_data_20.csv")
sim_data_20 <- read_csv("data/sim_data/sim_data_20.csv")

est_20 <- sim_data_20 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_20 <- data_gen(250, 0.7)
boot_20 <- boot_strap(sim_data_boot_3, adjRI = "hc", adjW = "unadjust")

case_20 <- cbind(est_20, boot_20) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.7, RI unadjust, W inaccurate
# sim_data_21 <- map(1:1000, combined, n = 250, true_ratio = 0.7, adjustRI = "unadjust", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_21, "data/sim_data/sim_data_21.csv")
sim_data_21 <- read_csv("data/sim_data/sim_data_21.csv")

est_21 <- sim_data_21 %>% 
  summarize(Bias_RI = round(0.7 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.7 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.7 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_21 <- data_gen(250, 0.7)
boot_21 <- boot_strap(sim_data_boot_3, adjRI = "unadjust", adjW = "hc")

case_21 <- cbind(est_21, boot_21) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

### n = 250, truth = 0.9 ----
# repeated 1000 times, n = 250, truth = 0.9, RI full, W full
# sim_data_22 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "full", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_22, "data/sim_data/sim_data_22.csv")
sim_data_22 <- read_csv("data/sim_data/sim_data_22.csv")

est_22 <- sim_data_22 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

sim_data_boot_4 <- data_gen(250, 0.9)
boot_22 <- boot_strap(sim_data_boot_4, adjRI = "full", adjW = "full")

case_22 <- cbind(est_22, boot_22) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.9, RI inaccurate, W inaccurate
# sim_data_23 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "hc", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_23, "data/sim_data/sim_data_23.csv")
sim_data_23 <- read_csv("data/sim_data/sim_data_23.csv")

est_23 <- sim_data_23 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_23 <- data_gen(250, 0.9)
boot_23 <- boot_strap(sim_data_boot_4, adjRI = "hc", adjW = "hc")

case_23 <- cbind(est_23, boot_23) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.9, RI unadjust, W unadjust
# sim_data_24 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "unadjust", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_24, "data/sim_data/sim_data_24.csv")
sim_data_24 <- read_csv("data/sim_data/sim_data_24.csv")

est_24 <- sim_data_24 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_24 <- data_gen(250, 0.9)
boot_24 <- boot_strap(sim_data_boot_4, adjRI = "unadjust", adjW = "unadjust")

case_24 <- cbind(est_24, boot_24) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.9, RI unadjust, W full
# sim_data_25 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "unadjust", adjustW = "full") %>% bind_rows()
# write_csv(sim_data_25, "data/sim_data/sim_data_25.csv")
sim_data_25 <- read_csv("data/sim_data/sim_data_25.csv")

est_25 <- sim_data_25 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_25 <- data_gen(250, 0.9)
boot_25 <- boot_strap(sim_data_boot_4, adjRI = "unadjust", adjW = "full")

case_25 <- cbind(est_25, boot_25) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.9, RI full, W unadjust
# sim_data_26 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "full", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_26, "data/sim_data/sim_data_26.csv")
sim_data_26 <- read_csv("data/sim_data/sim_data_26.csv")

est_26 <- sim_data_26 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_26 <- data_gen(250, 0.9)
boot_26 <- boot_strap(sim_data_boot_4, adjRI = "full", adjW = "unadjust")

case_26 <- cbind(est_26, boot_26) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.9, RI inaccurate, W unadjust
# sim_data_27 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "hc", adjustW = "unadjust") %>% bind_rows()
# write_csv(sim_data_27, "data/sim_data/sim_data_27.csv")
sim_data_27 <- read_csv("data/sim_data/sim_data_27.csv")

est_27 <- sim_data_27 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_27 <- data_gen(250, 0.9)
boot_27 <- boot_strap(sim_data_boot_4, adjRI = "hc", adjW = "unadjust")

case_27 <- cbind(est_27, boot_27) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# repeated 1000 times, n = 250, truth = 0.9, RI unadjust, W inaccurate
# sim_data_28 <- map(1:1000, combined, n = 250, true_ratio = 0.9, adjustRI = "unadjust", adjustW = "hc") %>% bind_rows()
# write_csv(sim_data_28, "data/sim_data/sim_data_28.csv")
sim_data_28 <- read_csv("data/sim_data/sim_data_28.csv")

est_28 <- sim_data_28 %>% 
  summarize(Bias_RI = round(0.9 - mean(ratio_RI),4), SE_RI = round(sd(ratio_RI),4), Bias_W = round(0.9 - mean(ratio_W),4), SE_W = round(sd(ratio_W),4), Bias_DR = round(0.9 - mean(ratio_DR),4), SE_DR = round(sd(ratio_DR),4))

#sim_data_boot_28 <- data_gen(250, 0.9)
boot_28 <- boot_strap(sim_data_boot_4, adjRI = "unadjust", adjW = "hc")

case_28 <- cbind(est_28, boot_28) %>% select(Bias_RI,SE_RI, SE_Boot_RI, Bias_W,SE_W, SE_Boot_W, Bias_DR,SE_DR, SE_Boot_DR)

# Result table ----
case_names <- paste0("case_", 1:28)
case_list <- mget(case_names)
final_df <- do.call(rbind, case_list)
kable(final_df, "latex")

# Visualizations ----
case_labels <- c("(acc, acc)", "(inacc, inacc)", "(unadj, unadj)", "(unadj, acc)", "(acc, unadj)", "(inacc, unadj)", "(unadj, inacc)")
vis_data <- final_df %>% mutate(label = rep(case_labels, 4)) %>% head(7)


# bias plot
vis_data_b <- vis_data %>% 
  pivot_longer(cols = c(Bias_RI, Bias_W, Bias_DR), names_to = "estimator", values_to = "bias") 

vis_data_b %>% 
  ggplot(aes(x = bias, y = label, color = estimator)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_color_manual(breaks = c("Bias_RI", "Bias_W", "Bias_DR"), values = c("#4e83bd", "#c1524d", "#9dbb59"), label = c("RI", "W", "DR")) +
  scale_y_discrete(limits = case_labels) +
  labs(y = NULL, title = "Case: sample size = 500, true ratio = 0.7") +
  theme_minimal()


# SE plot
vis_data_se <- vis_data %>% 
  pivot_longer(cols = c(SE_RI, SE_W, SE_DR), names_to = "estimator", values_to = "se") %>% 
  select(label, estimator, se)

vis_data_se %>% 
  ggplot(aes(x = se, y = label, color = estimator)) +
  geom_point() +
  scale_color_manual(breaks = c("SE_RI", "SE_W", "SE_DR"), values = c("#4e83bd", "#c1524d", "#9dbb59"), label = c("RI", "W", "DR")) +
  scale_y_discrete(limits = case_labels) +
  labs(y = NULL, x = "Standard Error", title = "Case: sample size = 500, true ratio = 0.7") +
  theme_minimal()


# Bootstrapping SE comparison
vis_data_bse <- vis_data %>% 
  pivot_longer(cols = c(SE_Boot_RI, SE_Boot_W, SE_Boot_DR), names_to = "estimator", values_to = "boot_se") %>% select(boot_se)
vis_data_sediff <- cbind(vis_data_se, vis_data_bse) %>% mutate(se_diff = boot_se - se)

vis_data_sediff %>% 
  ggplot(aes(x = se_diff, y = label, color = estimator)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_color_manual(breaks = c("SE_RI", "SE_W", "SE_DR"), values = c("#4e83bd", "#c1524d", "#9dbb59"), label = c("RI", "W", "DR")) +
  scale_y_discrete(limits = case_labels) +
  labs(y = NULL, x = "Diff in Standard Error", title = "Case: sample size = 500, true ratio = 0.7") +
  theme_minimal()

# One simulated data overview
sim_data <- data_gen(500, 0.7)
sim_data <- sim_data %>% 
  mutate(edu_level = case_when(
    eduyrs <= 12 ~ "High School",
    eduyrs > 12 & eduyrs < 16 ~ "Some College",
    eduyrs == 16 ~ "Bachelor",
    eduyrs > 16 ~ "Advanced"
  )) %>% 
  mutate(wage = exp(lnwage))

sim_data %>% 
  ggplot(aes(x = edu_level, fill = as.character(occ))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(label = c("high-income", "med-income", "low-income"), name = "occupations") +
  scale_x_discrete(limits = c("High School", "Some College", "Bachelor", "Advanced")) +
  labs(x = NULL, y = NULL) +
  theme_classic()

sim_data %>% 
  ggplot(aes(x = expf, y = wage, color = as.character(occ))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_discrete(label = c("high-income", "med-income", "low-income"), name = "occupations") +
  labs(x = "experience (year)", y = "hourly wage") +
  theme_classic()

sim_data %>% 
  ggplot(aes(x = exp(lnwage), color = as.character(female))) +
  geom_density() +
  scale_color_discrete(label = c("male", "female"), name = "gender") +
  labs(x = "wage($)", y = NULL) +
  theme_classic()

