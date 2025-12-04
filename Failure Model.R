library(tidyverse)
library(dplyr)
library(survival)

world_data <- read.csv("~/Downloads/FINAL WORLD DATA.csv")
world_data$Index <- seq_len(nrow(world_data))
world_data$W1_0 <- 1
head(world_data)

# Start from the beginning
stages <- c("W1_0", "W1_1", "W1_2", 
            "W4_1", "W4_2", 
            "W8_1", "W8_2", "W8_3", "W8_4")
df <- world_data[, c("Index", stages)]

failure_data <- df %>%
  rowwise() %>%
  mutate(failure_time = {vals <- c_across(all_of(stages))
                        fail_pos <- which(vals == 0)[1]   
                        if (is.na(fail_pos)) length(vals)  
                        else fail_pos - 1},
        failure = ifelse(any(c_across(all_of(stages)) == 0), 1, 0)) %>%
  ungroup()

fit <- survfit(Surv(failure_time, failure) ~ 1, data = failure_data)
summary(fit)
plot(fit, xlab = "Stage", ylab = "Survival Probability")



