#author:: Kevil Khadka

library(tidyverse)

# read the data file

auditor <- read_csv("auditor.csv")
View(auditor)


# informal analysis
ggplot(auditor) + geom_point(aes(x = method, y = score))
# spread is simmilar ,,, no outliers


ggplot(auditor) + geom_boxplot(aes(x = method, y = score))


auditor %>%  
  group_by(method) %>% 
  summarise(group_sd = sd(score))

# formal anaylsis - ANOVA
# error in the block format
mod <- lm(score ~ method + block, data = auditor)
anova(mod)


# Quick fix
mod <- lm(score ~ method + factor(block), data = auditor)
anova(mod)


# what if CR design were used 
mod_CR <- lm(score ~ method, data = auditor)
anova(mod_CR)


## post-operative pain treatment
pain <- read_table2("dentalpain.txt")
View(pain)

# informal analysis
pain %>% 
  unite(treatment,codeine, 
        acupunc, sep = "/") %>% 
  ggplot(aes(x = treatment, y = relief)) + geom_point()


pain %>% 
  unite(treatment,codeine, 
        acupunc, sep = "/") %>% 
  group_by(treatment) %>% 
  summarise(group_sd = sd(relief))

# anova
mod_pain <- lm(relief ~ codeine + acupunc + codeine: acupunc + factor(block), data = pain)
anova(mod_pain)
