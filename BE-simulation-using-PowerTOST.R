library(tidyverse)
library(PowerTOST)

# creating data frame
data <- tibble(
  subject = rep(seq(29), 2),
  sequence = c(rep("T/R", 14), rep("R/T", 15), rep("T/R", 14), rep("R/T", 15)),
  period = c(rep(1, 14), rep(2, 15), rep(2, 14), rep(1, 15)),
  formulation = c(rep("T", 29), rep("R", 29)),
  AUC = rnorm(58, 11.7345, 0.2)
)

# transforming data type into factors
data
data$subject = as.factor(data$subject)
data$sequence = as.factor(data$sequence)
data$period = as.factor(data$period)
data$formulation = as.factor(data$formulation)
data

# modeling and anova
mod <- lm(AUC ~ subject + period + formulation, data = data)
mod
anova(mod)
mse <- anova(mod)[[3]][[4]]
mse
CV <- mse2CV(mse)
CV
mean_T <- mean(filter(data, formulation == "T")$AUC)
mean_R <- mean(filter(data, formulation == "R")$AUC)
GMR <- exp(mean_T - mean_R)
GMR

# counting the BE CI
CI.BE(pe = GMR, CV = CV, n = c(14,15))

# bootstrapping
sampling_result <- sample(seq(29), 12, replace = TRUE)
sampling_result
bootstrap_data <- data %>% filter(subject == 1000)
for(i in sampling_result) print(i)
for ( i in sampling_result){
  bootstrap_data <- bootstrap_data %>% rbind(filter(data, subject == i, formulation %in% c("T", "R")))
}
bootstrap_data

# modeling and anova
mod <- lm(AUC ~ subject + period + formulation, data = bootstrap_data)
mod
anova(mod)
mse <- anova(mod)[[3]][[4]]
mse
CV <- mse2CV(mse)
CV

# power and sample N
power.TOST(CV = CV, n = 12)
sampleN.TOST(CV = CV)
