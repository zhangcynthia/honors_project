library(haven)
library(tidyverse)
library(dplyr)
library(tidymodels)
library(purrr)
library(ggplot2)
library(ggmosaic)

# data cleaning ----
psid_dat <- read_dta(
  "data/20160995_PSID_regready_sept26.dta",
  encoding = NULL,
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
)

cleaned_dat <- psid_dat %>% 
  filter(farmer == 0 | smsa !="." | annwks >= 26) %>% 
  filter(!is.na(smsa)) %>% 
  rename(expf = yrsftexpfz,
         expp = yrsptexpfz,
         edyrs = schupd,
         construct = constructextractinstall) %>% 
  mutate(ne = ifelse(region == 1, 1, 0),
         nc = ifelse(region == 2, 1, 0),
         govt = ifelse(wtrgov == 1, 1, 0),
         hrwkge50 = ifelse(usualhrwk >= 50, 1, 0),
         expfsq = expf^2,
         exppsq = expp^2,
         pexp = age-edyrs-6) %>% 
  filter(!is.na(govt)) %>% 
  mutate(pexp1 = case_when(pexp < 0 ~ 0,
                          pexp > age-18 ~ age - 18,
                          .default = pexp),
         pexpsq = pexp1^2,
         mgrocc = manager+business+financialop,
         profocc = computer+architect+scientist+socialworker+postseceduc+legaleduc+artist+lawyerphysician+healthcare,
         # occ=313 for nurses 230-234 inclusive is for K12 plus special
         maleprofaer = ifelse(profocc == 1 & occ != 313 & (occ<230 | occ>234), 1, 0))

years <- c(1981, 1990, 1999, 2007, 2009, 2011)





# Method 1 ----
fit_model <- function(df, type, sub){
  #browser()
  if(sub == 'M'){ df <- df %>% filter(ft == 1, wagesamp == 1, female == 0)}
  if(sub == 'F'){ df <- df %>% filter(ft == 1, wagesamp == 1, female == 1)}
  dur_public <- df %>% select(durables:professional) %>% names() %>% str_c(., collapse = " + ")
  buss_tran <- df %>% select(business:production) %>% names() %>% str_c(., collapse = " + ")
  if(type == 'hc'){
  lm(lnrealwg ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace, data = df, weights = famwgt)
  }else if(type == 'full'){
    lm(as.formula(paste('lnrealwg ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace + unjob + govt +', dur_public, '+', buss_tran, "- farmer")), data = df, weights = famwgt) 
  }else if(type == 'unadjust'){
    lm(lnrealwg ~ 1, data = df, weights = famwgt)
  }
}


# unadjusted
dat_rawgap <- data.frame()
for (i in years){
  dat_m <- cleaned_dat %>% filter(ft == 1, wagesamp == 1, female == 0, wave == i) %>% 
    summarize(malemean = weighted.mean(lnrealwg,famwgt,na.rm = TRUE))
  dat_f <- cleaned_dat %>% filter(ft == 1, wagesamp == 1, female == 1, wave == i) %>% 
    summarize(femalemean = weighted.mean(lnrealwg,famwgt,na.rm = TRUE))
  dat <- cbind(dat_m, dat_f)
  dat <- dat %>% mutate(rawgap = exp(femalemean - malemean))
  dat_rawgap <- rbind(dat_rawgap, dat)
}
dat_rawgap


Model_Output_unadj <- cleaned_dat %>% group_by(wave) %>%
  nest() %>%
  mutate(mod_M_unadj = map(data,~fit_model(.x,type = 'unadjust',sub = 'M')), mod_F_unadj = map(data,~fit_model(.x,type = 'unadjust',sub = 'F')) ) %>%
  mutate(pred_M_unadj = map2(data, mod_M_unadj, ~ predict(.y, newdata = .x)), pred_F_unadj = map2(data, mod_F_unadj, ~ predict(.y, newdata = .x))) %>% unnest(c(data, pred_M_unadj, pred_F_unadj)) %>% 
  mutate(resid_M_unadj = lnrealwg - pred_M_unadj, resid_F_unadj = pred_F_unadj - lnrealwg)

Model_Output_unadj %>%
  group_by(wave) %>%
  filter(female == 1,ft == 1, wagesamp == 1) %>%
  summarize(CF_Unexplained_Gap = exp(weighted.mean(resid_M_unadj,famwgt,na.rm = TRUE)))



# hc
Model_Output_hc <- cleaned_dat %>% group_by(wave) %>%
  nest() %>%
  mutate(mod_M_hc = map(data,~fit_model(.x,type = 'hc',sub = 'M')), mod_F_hc = map(data, ~fit_model(.x,type = 'hc',sub = 'F')) ) %>%
  mutate(pred_M_hc = map2(data, mod_M_hc, ~ predict(.y, newdata = .x)), pred_F_hc = map2(data, mod_F_hc, ~ predict(.y, newdata = .x))) %>% unnest(c(data, pred_M_hc, pred_F_hc)) %>% 
  mutate(resid_M_hc = lnrealwg - pred_M_hc, resid_F_hc = pred_F_hc - lnrealwg)

Model_Output_hc %>%
  group_by(wave) %>%
  filter(ft == 1, wagesamp == 1, female == 1) %>%
  summarize(CF_Unexplained_Gap = exp(weighted.mean(resid_M_hc,famwgt,na.rm = TRUE)))


# full
Model_Output_full <- cleaned_dat %>% group_by(wave) %>%
  nest() %>%
  mutate(mod_M_full = map(data,~fit_model(.x,type = 'full',sub = 'M')), mod_F_full = map(data,~fit_model(.x,type = 'full',sub = 'F')))  %>%
  mutate(pred_M_full = map2(data, mod_M_full, ~ predict(.y, newdata = .x)), pred_F_full = map2(data, mod_F_full, ~ predict(.y, newdata = .x))) %>% unnest(c(data, pred_M_full, pred_F_full)) %>% 
  mutate(resid_M_full = lnrealwg - pred_M_full, resid_F_full = lnrealwg - pred_F_full)


Model_Output_full %>%
  group_by(wave) %>%
  filter(ft == 1, wagesamp == 1, female == 1) %>%
  summarize(CF_Unexplained_Gap = exp(weighted.mean(resid_M_full,famwgt,na.rm = TRUE)))


# RI function
RI_estimator <- function(df, type){
  Model_Output <- df %>% group_by(wave) %>%
    nest() %>%
    mutate(mod_M = map(data,~fit_model(.x,type = type, sub = 'M')), mod_F = map(data,~fit_model(.x,type = type, sub = 'F')))  %>%
    mutate(pred_M = map2(data, mod_M, ~ predict(.y, newdata = .x)), pred_F = map2(data, mod_F, ~ predict(.y, newdata = .x))) %>% unnest(c(data, pred_M, pred_F)) %>% 
    mutate(resid_M = lnrealwg - pred_M, resid_F = lnrealwg - pred_F)
  
  Model_Output %>%
    group_by(wave) %>%
    filter(ft == 1, wagesamp == 1, female == 1) %>%
    summarize(RI_est = weighted.mean(pred_M, famwgt, na.rm = TRUE),
              ratio_RI = exp(weighted.mean(resid_M,famwgt,na.rm = TRUE)))
}

RI_estimator(cleaned_dat, "hc")
RI_estimator(cleaned_dat, "full")
RI_estimator(cleaned_dat, "unadjust")



# Method 2 ----
fit_prop_model <- function(df, type){
  #browser()
  df <- df %>% filter(ft == 1, wagesamp == 1)
  dur_public <- df %>% select(durables:professional) %>% names() %>% str_c(., collapse = " + ")
  buss_tran <- df %>% select(business:production) %>% names() %>% str_c(., collapse = " + ")
  if(type == 'hc'){
    glm(female ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace, family = "quasibinomial", data = df, weights = famwgt)
  }else if(type == 'full'){
    glm(as.formula(paste('female ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace + unjob + govt +', dur_public, '+', buss_tran, "- farmer")), family = "quasibinomial", data = df, weights = famwgt) 
  }else if(type == 'unadjust'){
    glm(female ~ 1, family = "quasibinomial", data = df, weights = famwgt)
  }
}


# hc
wt_output_hc <- cleaned_dat %>% group_by(wave) %>%
  nest() %>%
  mutate(ps_mod_hc = map(data, ~fit_prop_model(.x, type = "hc")),
         fem_prb = map(data, ~mean(.x$female, na.rm = TRUE))) %>% 
  mutate(ps_hc = map2(data, ps_mod_hc, ~predict(.y, newdata = .x, type = "response"))) %>% 
  unnest(c(data, ps_hc, fem_prb)) %>% 
  mutate(odds = ps_hc/(1 - ps_hc),
         wt = ifelse(female == 1, 0, odds*(1/fem_prb)))

wt_output_hc %>% group_by(wave) %>%
  filter(ft == 1, wagesamp == 1) %>%
  summarize(mean_lnwage = sum(wt*lnrealwg, na.rm = TRUE)/sum(wt, na.rm = TRUE), mean_female_lnwage = sum(female*lnrealwg, na.rm=TRUE)/sum(female, na.rm=TRUE)) %>%
  mutate(diff = mean_lnwage - mean_female_lnwage, ratio = exp(-diff))


# full
wt_output_full <- cleaned_dat %>% group_by(wave) %>%
  nest() %>%
  mutate(ps_mod_full = map(data, ~fit_prop_model(.x, type = "full")),
         fem_prb = map(data, ~mean(.x$female, na.rm = TRUE))) %>% 
  mutate(ps_full = map2(data, ps_mod_full, ~predict(.y, newdata = .x, type = "response"))) %>% 
  unnest(c(data, ps_full, fem_prb)) %>% 
  mutate(odds = ps_full/(1 - ps_full),
         wt = ifelse(female == 1, 0, odds*(1/fem_prb)))

wt_output_full %>% group_by(wave) %>% 
  filter(ft == 1, wagesamp == 1) %>%
  summarize(mean_lnwage = sum(wt*lnrealwg, na.rm = TRUE)/sum(wt, na.rm = TRUE), mean_female_lnwage = sum(female*lnrealwg, na.rm=TRUE)/sum(female, na.rm=TRUE)) %>%
  mutate(diff = mean_lnwage - mean_female_lnwage, ratio = exp(-diff))


# unadjusted
wt_output_unadj <- cleaned_dat %>% group_by(wave) %>%
  nest() %>%
  mutate(ps_mod_unadj = map(data, ~fit_prop_model(.x, type = "unadjust")),
         fem_prb = map(data, ~mean(.x$female, na.rm = TRUE))) %>% 
  mutate(ps_unadj = map2(data, ps_mod_unadj, ~predict(.y, newdata = .x, type = "response"))) %>% 
  unnest(c(data, ps_unadj, fem_prb)) %>% 
  mutate(odds = ps_unadj/(1 - ps_unadj),
         wt = ifelse(female == 1, 0, odds*(1/fem_prb)))

wt_output_unadj %>% group_by(wave) %>% 
  filter(ft == 1, wagesamp == 1) %>%
  summarize(mean_lnwage = sum(wt*lnrealwg, na.rm = TRUE)/sum(wt, na.rm = TRUE), mean_female_lnwage = sum(female*lnrealwg, na.rm=TRUE)/sum(female, na.rm=TRUE)) %>%
  mutate(diff = mean_lnwage - mean_female_lnwage, ratio = exp(-diff))

# function
weight_estimator <- function(df, type){
  wt_output <- df %>% group_by(wave) %>%
    nest() %>%
    mutate(ps_mod = map(data, ~fit_prop_model(.x, type = type)),
           fem_prb = map(data, ~mean(.x$female, na.rm = TRUE))) %>% 
    mutate(ps = map2(data, ps_mod, ~predict(.y, newdata = .x, type = "response"))) %>% 
    unnest(c(data, ps, fem_prb)) %>% 
    mutate(odds = ps/(1 - ps),
           wt = ifelse(female == 1, 0, odds*(1/fem_prb)))
  
  wt_output %>% group_by(wave) %>%
    filter(ft == 1, wagesamp == 1) %>%
    summarize(mean_female_lnwage = weighted.mean(lnrealwg, famwgt*female, na.rm = TRUE),
              wt_est = sum(wt*lnrealwg*famwgt, na.rm = TRUE)/(sum(wt*famwgt, na.rm = TRUE))) %>% # double check
    mutate(ratio_wt = exp(mean_female_lnwage - wt_est))
}

weight_estimator(cleaned_dat, "hc")
weight_estimator(cleaned_dat, "full")
weight_estimator(cleaned_dat, "unadjust")






# Method 3 ----
# Doubly Robust Estimator
cleaned_dat %>% group_by(wave) %>%
  filter(ft == 1, wagesamp == 1) %>% 
  nest() %>%
  mutate(mod_M_hc = map(data,~fit_model(.x,type = 'hc',sub = 'M'))) %>%
  mutate(pred_M_hc = map2(data, mod_M_hc, ~predict(.y, newdata = .x))) %>% 
  unnest(c(data, pred_M_hc)) %>% 
  mutate(resid_M_hc = lnrealwg - pred_M_hc) %>% group_by(wave) %>%
  nest() %>%
  mutate(ps_mod_hc = map(data, ~fit_prop_model(.x, type = "hc")),
         fem_prb = map(data, ~mean(.x$female, na.rm = TRUE))) %>% 
  mutate(ps_hc = map2(data, ps_mod_hc, ~predict(.y, newdata = .x, type = "response"))) %>% 
  unnest(c(data, ps_hc, fem_prb)) %>% 
  mutate(odds = ps_hc/(1 - ps_hc),
         w = ifelse(female == 1, 0, odds*(1/fem_prb))) %>%
  filter(!is.na(pred_M_hc)) %>% # omit NA
  group_by(wave) %>%
  mutate(RI_est = mean(female*pred_M_hc)) %>%
  select(wave, intnum68, pernum68, pred_M_hc,lnrealwg,female,w,fem_prb, RI_est,famwgt) %>%
  mutate(DR_1 = w*(lnrealwg - pred_M_hc),
         DR_2 = ifelse(female == 0, 0, (pred_M_hc - RI_est)/fem_prb)) %>% 
  group_by(wave) %>%
  summarize(DR_est = mean(DR_1 + DR_2 + RI_est),
            mean_female_lnwage = sum(female*lnrealwg, na.rm=TRUE)/sum(female, na.rm=TRUE)) %>% 
  mutate(ratio = exp(mean_female_lnwage-DR_est))


# function
DR_estimator <- function(df, type){
  df %>% group_by(wave) %>%
    filter(ft == 1, wagesamp == 1) %>% 
    nest() %>%
    mutate(mod_M = map(data,~fit_model(.x,type = type,sub = 'M'))) %>%
    mutate(pred_M = map2(data, mod_M, ~predict(.y, newdata = .x))) %>% 
    unnest(c(data, pred_M)) %>% 
    mutate(resid_M = lnrealwg - pred_M) %>% group_by(wave) %>%
    nest() %>%
    mutate(ps_mod = map(data, ~fit_prop_model(.x, type = type)),
           fem_prb = map(data, ~mean(.x$female, na.rm = TRUE))) %>% 
    mutate(ps = map2(data, ps_mod, ~predict(.y, newdata = .x, type = "response"))) %>% 
    unnest(c(data, ps, fem_prb)) %>% 
    mutate(odds = ps/(1 - ps),
           w = ifelse(female == 1, 0, odds*(1/fem_prb))) %>%
    filter(!is.na(pred_M)) %>%
    group_by(wave) %>%
    mutate(RI_est = weighted.mean(female*pred_M, famwgt*female, na.rm = TRUE)) %>% #Brianna changed from mean()
    select(wave, intnum68, pernum68, pred_M, lnrealwg, female, w, fem_prb, RI_est, famwgt) %>%
    mutate(DR_1 = w*(lnrealwg - pred_M),
           DR_2 = ifelse(female == 0, 0, (pred_M - RI_est)/fem_prb)) %>% 
    group_by(wave) %>%
    summarize(mean_female_lnwage = weighted.mean(lnrealwg, famwgt*female, na.rm = TRUE),
              DR_est = weighted.mean(DR_1 + DR_2 + RI_est, famwgt, na.rm = TRUE)) %>%  # update survey weight
    mutate(ratio_DR = exp(mean_female_lnwage-DR_est))
}

DR_estimator(cleaned_dat, 'hc')
DR_estimator(cleaned_dat, 'full')
DR_estimator(cleaned_dat, 'unadjust')




# Result table & Visuals ----
## Table
estimator_table <- function(type){
  wt_df <- weight_estimator(cleaned_dat, type)
  ri_df <- RI_estimator(cleaned_dat, type)
  DR_estimator(cleaned_dat, type) %>% 
    left_join(wt_df %>% select(wave, wt_est, ratio_wt), by = 'wave') %>% 
    left_join(ri_df, by = 'wave') %>% 
    mutate(adjustment = type)
}

unadj_tbl <- estimator_table("unadjust")
hc_tbl <- estimator_table("hc")
full_tbl <- estimator_table("full")

final_tbl <- rbind(unadj_tbl, hc_tbl, full_tbl)

## visualizations
### by estimation method
final_tbl %>% 
  filter(wave %in% c(1981, 1990, 1999, 2011)) %>% 
  mutate(adjustment = fct_relevel(adjustment, c('unadjust','hc','full'))) %>%
  mutate(lab = str_c(round(ratio_RI, 2)*100,'%')) %>%
  ggplot(aes(x = factor(wave), y = ratio_RI, fill = adjustment)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = lab), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim=c(0.5,.95))+
  scale_x_discrete(labels = c("1980", "1989", "1998", "2010")) +
  scale_fill_manual(values = c("#4e83bd", "#c1524d", "#9dbb59"),guide = FALSE) +
  theme_classic() +
  labs(x = NULL, y = NULL,title = "Female to Male Wage Ratio (RI estimator)")

final_tbl %>% 
  mutate(adjustment = fct_relevel(adjustment, c('unadjust','hc','full'))) %>%
  ggplot(aes(x = factor(wave), y = ratio_wt, fill = adjustment)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(limits=c(0,1)) +
  theme_classic() +
  labs(x = "Wave", y = "log wage ratio", title = "Weighting estimator")

final_tbl %>% 
  mutate(adjustment = fct_relevel(adjustment, c('unadjust','hc','full'))) %>%
  ggplot(aes(x = factor(wave), y = ratio_DR, fill = adjustment)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(limits=c(0,1)) +
  theme_classic() +
  labs(x = "Wave", y = "log wage ratio", title = "DR estimator")


### by adjustment
long_tbl <- final_tbl %>% select(wave, ratio_DR, ratio_wt, ratio_RI, adjustment) %>% 
  pivot_longer(cols = c(ratio_DR, ratio_wt, ratio_RI), names_to = "ratio")

long_tbl %>% 
  #filter(wave %in% c(1981, 1990, 1999, 2011)) %>% 
  mutate(ratio = fct_relevel(ratio, c('ratio_RI','ratio_wt','ratio_DR'))) %>%
  ggplot(aes(x = wave, y = value, color = ratio)) +
  geom_line(linewidth = 1.5) +
  geom_text(x = 1982,y = .78,label = '77%',color = 'darkgrey') +
  geom_text(x = 1983.5,y = 1.01,label = 'No Gap',color = 'darkgrey') +
  geom_hline(yintercept = c(.77,1), color = 'darkgrey') +
  scale_color_manual(values = c("#4e83bd", "#c1524d", "#9dbb59"),name = "Estimators", labels = c("RI", "Weighting", "DR")) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim=c(0.5,1.01))+
  scale_x_continuous(breaks = c(1981, 1990, 1999, 2007, 2009, 2011), labels = c("1980", "1989", "1998", "2006", "2008", "2010")) +
  theme_classic() +
  labs(x = NULL, y = "", title = "Female to Male Wage Ratio (By Estimator)") +
  facet_grid(~adjustment, labeller = labeller(adjustment = c("full" = "full", "hc" = "human captial", "unadjust" = "unadjusted"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Other Visualizations ----
#GENDER
cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(lnrealwg, wave, female,realhrwage) %>% 
  group_by(wave, female) %>% 
  ggplot(aes(x = wave, fill = as.character(female))) +
  geom_bar(position = 'fill') 


# wage (no adjustment)
cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(lnrealwg, wave, female,realhrwage) %>% 
  group_by(wave, female) %>% 
  ggplot(aes(x = as.character(female), y = realhrwage)) +
  geom_boxplot() + 
  facet_grid(~wave) + ylim(0,50)

# education
cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(ba, wave, female) %>% 
  mutate(ba = as.factor(ba), female =as.factor(female)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(ba,female), fill = ba)) +  
  facet_grid(~wave)

# education years
cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(edyrs, wave, female) %>% 
  group_by(wave, female) %>% 
  ggplot(aes(x = as.character(female), y = edyrs)) +
  geom_boxplot() + 
  facet_grid(~wave)

cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(edyrs, wave, female) %>% 
  group_by(wave, female) %>% 
  ggplot(aes(x = edyrs, fill = as.character(female))) +
  geom_bar() + 
  facet_grid(~wave)

cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(edyrs, wave, female) %>% 
  summarise(mean = mean(edyrs), .by = c(wave, female)) %>%
  arrange(wave,female)


# experience (full)
cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(expf, wave, female) %>% 
  group_by(wave, female) %>% 
  ggplot(aes(x = as.character(female), y = expf)) +
  geom_boxplot() + 
  facet_grid(~wave)

# experience (part time)
cleaned_dat %>% filter(ft == 1, wagesamp == 1) %>% 
  select(expp, wave, female) %>% 
  group_by(wave, female) %>% 
  ggplot(aes(x = as.character(female), y = expp)) +
  geom_boxplot() + 
  facet_grid(~wave)



# # modeling + residual ----
# years <- c(1981, 1990, 1999, 2007, 2009, 2011)
# 
# # male hc
# male_hc_resid <- vector('list', length(year))
# for (i in years){
#   #data
#   male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == i)
#   # model
#   male_only <- lm(lnrealwg ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace, data = male_dat, weights = famwgt)
#   # add residual column
#   male_hc_resid[[paste0('rhftptexp', i)]] <- resid(male_only)
# }
# 
# # male full
# male_full_resid <- list()
# for (i in years){
#   #data
#   male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == i)
#   # model
#   male_only <- lm(lnrealwg ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace + unjob + govt + durables + publicadmin + business + transport, data = male_dat, weights = famwgt)
#   # add residual column
#   male_full_resid[[paste0('rfftptexp', i)]] <- resid(male_only)
# }
# 
# 
# # female hc
# fem_hc_resid <- list()
# for (i in years){
#   #data
#   fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == i) %>% 
#     filter(complete.cases(lnrealwg, expf, expfsq, expp, exppsq, edyrs, ba, adv, smsa, northeast, northcentral, south, black, hisp, othrace))
#   # model
#   fem_only <- lm(lnrealwg ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace, data = fem_dat, weights = famwgt)
#   # add residual column
#   fem_hc_resid[[paste0('frhftptexp', i)]] <- resid(fem_only)
# }
# 
# 
# # female full
# fem_full_resid <- list()
# for (i in years){
#   #data
#   fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == i)
#   # model
#   fem_only <- lm(lnrealwg ~ expf + expfsq + expp + exppsq + edyrs + ba + adv + smsa + northeast + northcentral + south + black + hisp + othrace + unjob + govt + durables + publicadmin + business + transport, data = fem_dat, weights = famwgt)
#   # residual
#   fem_full_resid[[paste0('frfftptexp', i)]] <- resid(fem_only)
# }
# fem_full_resid[["frfftptexp1981"]]
# 
# 
# # counterfactual?
# for (i in years){
#   fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == i)
#   
# }
# 
# 
# # 1-99 percentiles (weighted?) ----
# # wave = 1981
# # male
# male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == 1981)
# percentiles <- quantile(male_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# male_81_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   male_81_perc[paste0("lnwm81",i)] = percentiles[[i]]
# }
# # female
# fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == 1981)
# percentiles <- quantile(fem_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# fem_81_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   fem_81_perc[paste0("lnwf81",i)] = percentiles[[i]]
# }
# 
# # wave = 1990
# # male
# male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == 1990)
# percentiles <- quantile(male_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# male_90_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   male_90_perc[paste0("lnwm90",i)] = percentiles[[i]]
# }
# # female
# fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == 1990)
# percentiles <- quantile(fem_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# fem_90_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   fem_90_perc[paste0("lnwf90",i)] = percentiles[[i]]
# }
# 
# # wave = 1999
# # male
# male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == 1999)
# percentiles <- quantile(male_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# male_99_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   male_99_perc[paste0("lnwm99",i)] = percentiles[[i]]
# }
# # female
# fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == 1999)
# percentiles <- quantile(fem_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# fem_99_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   fem_99_perc[paste0("lnwf99",i)] = percentiles[[i]]
# }
# 
# # wave = 2007
# # male
# male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == 2007)
# percentiles <- quantile(male_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# male_07_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   male_07_perc[paste0("lnwm07",i)] = percentiles[[i]]
# }
# # female
# fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == 2007)
# percentiles <- quantile(fem_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# fem_07_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   fem_07_perc[paste0("lnwf07",i)] = percentiles[[i]]
# }
# 
# # wave = 2009
# # male
# male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == 2009)
# percentiles <- quantile(male_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# male_09_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   male_09_perc[paste0("lnwm09",i)] = percentiles[[i]]
# }
# # female
# fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == 2009)
# percentiles <- quantile(fem_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# fem_09_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   fem_09_perc[paste0("lnwf09",i)] = percentiles[[i]]
# }
# 
# # wave = 2011
# # male
# male_dat <- cleaned_dat %>% filter(female == 0, ft == 1, wagesamp == 1, wave == 2011)
# percentiles <- quantile(male_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# male_11_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   male_11_perc[paste0("lnwm11",i)] = percentiles[[i]]
# }
# # female
# fem_dat <- cleaned_dat %>% filter(female == 1, ft == 1, wagesamp == 1, wave == 2011)
# percentiles <- quantile(fem_dat$lnrealwg, probs = seq(0.01, 0.99, by = 0.01))
# fem_11_perc <- data.frame(matrix(ncol = 99, nrow = 1))
# for(i in 1:99){
#   fem_11_perc[paste0("lnwf11",i)] = percentiles[[i]]
# }
# 
# # raw gap (percentiles)
# # create empty table with placeholders
# rawGap <- tibble(
#   wave = c(1981, 1990, 1999, 2007, 2009, 2011)
# )
# for(i in 1:99){
#   rawGap[paste0("rawgap", i)] = -9999
# }
# 
# # add values
# for(i in 1:99){
#   f_var_81 <- paste0("lnwf81", i)
#   m_var_81 <- paste0("lnwm81", i)
#   f_var_90 <- paste0("lnwf90", i)
#   m_var_90 <- paste0("lnwm90", i)
#   f_var_99 <- paste0("lnwf99", i)
#   m_var_99 <- paste0("lnwm99", i)
#   f_var_07 <- paste0("lnwf07", i)
#   m_var_07 <- paste0("lnwm07", i)
#   f_var_09 <- paste0("lnwf09", i)
#   m_var_09 <- paste0("lnwm09", i)
#   f_var_11 <- paste0("lnwf11", i)
#   m_var_11 <- paste0("lnwm11", i)
#   rawGap[rawGap$wave == 1981, paste0("rawgap", i)] = fem_81_perc[[f_var_81]] - male_81_perc[[m_var_81]]
#   rawGap[rawGap$wave == 1990, paste0("rawgap", i)] = fem_90_perc[[f_var_90]] - male_90_perc[[m_var_90]]
#   rawGap[rawGap$wave == 1999, paste0("rawgap", i)] = fem_99_perc[[f_var_99]] - male_99_perc[[m_var_99]]
#   rawGap[rawGap$wave == 2007, paste0("rawgap", i)] = fem_07_perc[[f_var_07]] - male_07_perc[[m_var_07]]
#   rawGap[rawGap$wave == 2009, paste0("rawgap", i)] = fem_09_perc[[f_var_09]] - male_09_perc[[m_var_09]]
#   rawGap[rawGap$wave == 2011, paste0("rawgap", i)] = fem_11_perc[[f_var_11]] - male_11_perc[[m_var_11]]
# }
# rawGap
# 
# 
# # Preparation for table and figure ----
# 
# 
# # Figure and table ----
# 
# 
# 
# 
# 
# 
# 
# 
# # functions
# unexplained_gap <- function(year, adjustment){
#   male_dat <- cleaned_dat %>% filter(sex == 1, wave == year)
#   fem_dat <- cleaned_dat %>% filter(sex == 2, wave == year)
#   
#   if(adjustment == "unadjusted") {
#     female_only <- lm(log(annlabinc/annhrs) ~ 1, data = fem_dat, weights = famwgt) 
#     male_only <- lm(log(annlabinc/annhrs) ~ 1, data = male_dat, weights = famwgt)
#   }else{
#     if(adjustment == "hc"){
#       female_only <- lm(log(annlabinc/annhrs) ~ sch + region + white + black + hisp + yrsftexp + yrsptexp, data = fem_dat, weights = famwgt)
#       male_only <- lm(log(annlabinc/annhrs) ~ sch + region + white + black + hisp + yrsftexp + yrsptexp, data = male_dat, weights = famwgt)
#     }else{
#       female_only <- lm(log(annlabinc/annhrs) ~ sch + region + white + black + hisp + occ + ind + unjob + yrsftexp + yrsptexp, data = fem_dat, weights = famwgt)
#       male_only <- lm(log(annlabinc/annhrs) ~ sch + region + white + black + hisp + occ + ind + unjob + yrsftexp + yrsptexp, data = male_dat, weights = famwgt)
#     }
#   }
#   
#   # E(E(Y|A = 1, X)|A = 0) - CF
#   # E(E(Y|A = 0, X)|A = 0) - Fem
#   # E(E(Y|A = 1, X)|A = 1) - Male
#   tibble(
#     wave = year,
#     adjust = adjustment,
#     CF = male_only %>% predict(newdata = fem_dat) %>% mean(na.rm = TRUE),
#     Fem = female_only %>% predict() %>% mean(na.rm = TRUE),
#     Male = male_only %>% predict() %>% mean(na.rm = TRUE)
#   ) %>%
#     mutate(exp(Male - CF), exp(Fem - CF), exp(Fem - Male))
# }
# 
# 
# model_options <- crossing(year = c(1981,1990,1999,2011),
#          adjustment = c('unadjusted','hc','full'))
# 
# model_options %>%
#   mutate(results = purrr::pmap(model_options, unexplained_gap)) %>%
#   unnest(results) %>%
#   mutate(adjustment = fct_relevel(adjustment, c('unadjusted','hc','full'))) %>%
#   ggplot(aes(x = factor(year) , y = `exp(Fem - CF)`, fill = adjustment)) + 
#   geom_col(position = 'dodge') +
#   scale_y_continuous(limits=c(0,1))
