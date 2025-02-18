# Simulation Study

# Data Generation ----
# gender, age, education years, bachelar degree, occupations (A, B, C), region, experience, number of children, when give birth

## Prep ----
n_1 = 500
n_2 = 5000
n_3 = 10000

# education year generation function
## extract the list of proportion of each group
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

## build function
edu_gen <- function(age_cat, fem){
  if_else(
    age_cat %in% 25:34 & fem == 0, sample(7:18, size = 1, prob = young_m_prop), 
    if_else(
      age_cat %in% 25:34 & fem == 1, sample(8:18, size = 1, prob = young_f_prop),
      if_else(
        age_cat %in% 35:44 & fem == 0, sample(6:18, size = 1, prob = mid_m_prop),
        if_else(
          age_cat %in% 35:44 & fem == 1, sample(8:18, size = 1, prob = mid_f_prop),
          if_else(
            age_cat %in% 45:54 & fem == 0, sample(7:18, size = 1, prob = midold_m_prop),
            if_else(
              age_cat %in% 45:54 & fem == 1, sample(8:18, size = 1, prob = midold_f_prop), 
              if_else(
                age_cat %in% 55:64 & fem == 0, sample(c(4,7:18), size = 1, prob = old_m_prop),
                sample(8:18, size = 1, prob = old_f_prop)
              )
            )
          )
        )
      )
    )
  )
}

# when give first born
age_birth <- read_delim("data/Natality, 2007-2023.txt")
cleaned_age_birth <- age_birth %>% filter(is.na(Notes) == TRUE)
mean_age <- cleaned_age_birth %>% pull(`Average Age of Mother`)
sd_age <- cleaned_age_birth %>% pull(`Standard Deviation for Average Age of Mother`)

ne_age_birth <- function(eduyr){
  case_when(eduyr <= 8 ~ rnorm(n = 1, mean = mean_age[1], sd = sd_age[1]),
            eduyr > 8 & eduyr < 12 ~ rnorm(n = 1, mean = mean_age[2], sd = sd_age[2]),
            eduyr == 12 ~ rnorm(n = 1, mean = mean_age[3], sd = sd_age[3]),
            eduyr == 13 ~ rnorm(n = 1, mean = mean_age[4], sd = sd_age[4]),
            eduyr == 14 ~ rnorm(n = 1, mean = mean_age[5], sd = sd_age[5]),
            eduyr > 14 & eduyr <= 16 ~ rnorm(n = 1, mean = mean_age[6], sd = sd_age[6]),
            eduyr > 16 & eduyr <= 18 ~ rnorm(n = 1, mean = mean_age[7], sd = sd_age[7]),
            eduyr > 18 ~ rnorm(n = 1, mean = mean_age[8], sd = sd_age[8]), .default = NA)
}

mw_age_birth <- function(eduyr){
  case_when(eduyr <= 8 ~ rnorm(n = 1, mean = mean_age[9], sd = sd_age[9]),
            eduyr > 8 & eduyr < 12 ~ rnorm(n = 1, mean = mean_age[10], sd = sd_age[10]),
            eduyr == 12 ~ rnorm(n = 1, mean = mean_age[11], sd = sd_age[11]),
            eduyr == 13 ~ rnorm(n = 1, mean = mean_age[12], sd = sd_age[12]),
            eduyr == 14 ~ rnorm(n = 1, mean = mean_age[13], sd = sd_age[13]),
            eduyr > 14 & eduyr <= 16 ~ rnorm(n = 1, mean = mean_age[14], sd = sd_age[14]),
            eduyr > 16 & eduyr <= 18 ~ rnorm(n = 1, mean = mean_age[15], sd = sd_age[15]),
            eduyr > 18 ~ rnorm(n = 1, mean = mean_age[16], sd = sd_age[16]), .default = NA)
}

south_age_birth <- function(eduyr){
  case_when(eduyr <= 8 ~ rnorm(n = 1, mean = mean_age[17], sd = sd_age[17]),
            eduyr > 8 & eduyr < 12 ~ rnorm(n = 1, mean = mean_age[18], sd = sd_age[18]),
            eduyr == 12 ~ rnorm(n = 1, mean = mean_age[19], sd = sd_age[19]),
            eduyr == 13 ~ rnorm(n = 1, mean = mean_age[20], sd = sd_age[20]),
            eduyr == 14 ~ rnorm(n = 1, mean = mean_age[21], sd = sd_age[21]),
            eduyr > 14 & eduyr <= 16 ~ rnorm(n = 1, mean = mean_age[22], sd = sd_age[22]),
            eduyr > 16 & eduyr <= 18 ~ rnorm(n = 1, mean = mean_age[23], sd = sd_age[23]),
            eduyr > 18 ~ rnorm(n = 1, mean = mean_age[24], sd = sd_age[24]), .default = NA)
}

west_age_birth <- function(eduyr){
  case_when(eduyr <= 8 ~ rnorm(n = 1, mean = mean_age[25], sd = sd_age[25]),
            eduyr > 8 & eduyr < 12 ~ rnorm(n = 1, mean = mean_age[26], sd = sd_age[26]),
            eduyr == 12 ~ rnorm(n = 1, mean = mean_age[27], sd = sd_age[27]),
            eduyr == 13 ~ rnorm(n = 1, mean = mean_age[28], sd = sd_age[28]),
            eduyr == 14 ~ rnorm(n = 1, mean = mean_age[29], sd = sd_age[29]),
            eduyr > 14 & eduyr <= 16 ~ rnorm(n = 1, mean = mean_age[30], sd = sd_age[30]),
            eduyr > 16 & eduyr <= 18 ~ rnorm(n = 1, mean = mean_age[31], sd = sd_age[31]),
            eduyr > 18 ~ rnorm(n = 1, mean = mean_age[32], sd = sd_age[32]), .default = NA)
}



## Dataset set up ----
set.seed(120) 

tibble(
  female = rbinom(n_1, size = 1, prob = 0.5),
  age = round(runif(n_1, min = 25, max = 64)),
  region = round(runif(n_1, min = 1, max = 4)),
) %>% 
  mutate(
    northeast = if_else(region == 1, 1, 0),
    midwest = if_else(region == 2, 1, 0),
    south = if_else(region == 3, 1, 0),
    west = if_else(region == 4, 1, 0)
         ) %>% 
  # education
  mutate(
    eduyrs = edu_gen(age_cat = age, fem = female),
    ba = if_else(eduyrs <= 12, 0, 1),
    adv = if_else(eduyrs > 16, 1, 0),
    exp_check = age - eduyrs - 6
    ) %>% 
  # childbearing
  mutate(
    age_birth = case_when(female == 1 & region == 1 ~ ne_age_birth(eduyrs), # assign the same age for women who have the same region and education years
                          female == 1 & region == 2 ~ mw_age_birth(eduyrs),
                          female == 1 & region == 3 ~ south_age_birth(eduyrs),
                          female == 1 & region == 4 ~ west_age_birth(eduyrs),
                          .default = NA),
    gap = ifelse(age <= 50, age - age_birth, 50 - age_birth),
    num_child = ifelse(runif(n()) < 0.17, 0,
                       ifelse(gap < 0, 0, gap%/%2))
  ) %>% View()
