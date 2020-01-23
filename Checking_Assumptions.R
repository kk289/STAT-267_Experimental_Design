#author:: kevil khadka

library(tidyverse)

popcorn <- tibble(
  oil = rep(c("canola", "buttery"), each = 2),
  low = c(36, 47, 28, 42),
  high = c(63, 67, 81, 85)
)
popcorn
tidy_popcorn <-
  gather(popcorn, key = "salt", 
         value = "score", low, high)

tidy_popcorn %>%
  group_by(salt) %>%
  summarise(std_dev = sd(score))

ggplot(tidy_popcorn) +
  geom_boxplot(aes(x = salt, y = score))

# Decomposition of data
mod <- lm(score ~ salt + oil, 
          data = tidy_popcorn)

anova(mod)

# Compute fitted values (true value)
fitted.values(mod)

# Compute the residuals
residuals(mod)


# plot true values vs. residuals
ggplot() +
  geom_point(aes(x = fitted.values(mod),
                 y = residuals(mod)))

## QQ-plot
ggplot(mapping = aes(sample = residuals(mod))) +
  stat_qq() +
  stat_qq_line()

# experiment with non-normal data
normal_data <- rnorm(25)
nonnormal <- exp(normal_data)

ggplot() +
  geom_histogram(aes(x = nonnormal))

## QQ-plot
ggplot(mapping = aes(sample = nonnormal)) +
  stat_qq() +
  stat_qq_line()
