library(tidyverse)

library(readr)
iv_fluids <- read_csv("/System/Volumes/Data/University of Evansville/FALL 2019/STAT 267/iv_fluids.csv")

iv_fluids
summary(iv_fluids)
# prepare data set for analysis 
# let's tidy the data set

tidy_ivfluids <- iv_fluids %>%
  gather(key = "company", value= "n_particles",Cutter,Abbot,McGaw)
View(tidy_ivfluids)

ggplot() + geom_point(data= tidy_ivfluids, aes(x = company, y = n_particles))

#  numeric description stats
# averages
tidy_ivfluids %>% 
  summarise(grand_mean = mean(n_particles))

tidy_ivfluids %>% 
  group_by(company) %>% 
  summarise(company_avg = mean(n_particles))

tidy_ivfluids %>% 
  group_by(company) %>% 
  summarise(company_sd = sd(n_particles))

# anova table
mod <- lm(n_particles ~ company, data = tidy_ivfluids)
mod

anova(mod)
aov(mod)
