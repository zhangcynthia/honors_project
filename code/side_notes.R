cleaned_dat %>% filter(wave == 2009) %>% 
  lm(edyrs ~ female + age, data = .) %>% summary()


cleaned_dat %>% filter(wave == 2009) %>%  
  ggplot(aes(x = age, y = edyrs, color = factor(female))) + 
  geom_point() +
  geom_smooth()


cleaned_dat %>% filter(wave == 2009) %>%
  mutate(age_cat = cut(age,4)) %>%
  ggplot(aes(x = factor(edyrs),fill= age_cat)) +
  geom_bar(alpha = .2)


edu_prop_data <- cleaned_dat %>% filter(wave == 2009) %>%
  mutate(age_cat = cut(age,4)) %>%
  count(age_cat, edyrs,female) %>%
  group_by(age_cat,female) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(female,age_cat)

