#author: kevil khadka

library(tidyverse)

# Read the data file
auditor <- read_csv("auditor.csv")
View(auditor)

# Informal Analysis
ggplot(auditor) +
  geom_point(aes(x = method,
                 y = score))

ggplot(auditor) +
  geom_boxplot(aes(x = method,
                 y = score))

auditor %>%
  group_by(method) %>%
  summarise(group_sd = sd(score))

# Formal analysis - ANOVA
# Error in the block format
mod <- lm(score ~ method + block,
          data = auditor)

anova(mod)

# Quick Fix
mod <- lm(score ~ method + 
            factor(block),
          data = auditor)

anova(mod)

# What if CR design were used
mod_CR <- lm(score ~ method,
             data = auditor)
anova(mod_CR)

## Post-operative pain treatment
pain <- read_table2("dentalpain.txt")
View(pain)

# Informal Analysis
pain %>%
  unite(treatment, codeine, 
        acupunc, sep = "/") %>%
  ggplot(aes(x = treatment,
             y = relief)) +
  geom_point()

pain %>%
  unite(treatment, codeine, 
        acupunc, sep = "/") %>%
  group_by(treatment) %>%
  summarise(group_sd = sd(relief))

# ANOVA
mod_pain <- lm(relief ~ codeine +
                 acupunc + 
                 codeine:acupunc +
                 factor(block),
               data = pain)
anova(mod_pain)
