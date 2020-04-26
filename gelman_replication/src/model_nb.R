#see data exploration for loading and preparing data
#polls_main is dataset from polls_main_dataset.tsv
#polls_extra is dataset from polls_auxiliary_dataset.tsv

#load libraries
library(data.table)
library(tidyverse)
library(extraDistr)
set.seed(1408)

#####Create functions

# sim_srs_result_f <- function (v_r, n_i) {
#   srs_result <- rbinom(length(n_i), size = n_i, prob = v_r)/n_i
# }

#####Parameters

##Hyperparameters
sigma_a <- rnorm(1, mean = 0, sd = 0.2)
#sigma <- rhnorm(1, 0.2)
sigma_b <- rnorm(1, mean = 0, sd = 0.2)
#sigma_b <- rhnorm(1, 0.2)
sigma_tau <- rnorm(1, mean=0, sd = 0.05)
#sigma_tau <- rhnorm(1, 0.05)
  #in the text they modeled this with the half normal but 
  #the stan code suggests they just drew from the normal

mu_a <- rnorm(1, mean = 0, sd = 0.2)
mu_b <- rnorm(1, mean = 0, sd = 0.2)

alpha <- rnorm(1, mean = mu_a, sd = sigma_a)
beta <- rnorm(1, mean = mu_b, sd = sigma_b)
tao_sqr <- rhnorm(1, sigma = sigma_tao)

#####Data
model_data <- polls_main %>%
mutate(specific_elec = fct_cross(election, electionDate),
      logit_v = logit_f(v_r),
      p = 0,
      poll_srs_result = sim_srs_result(f(v_r, n_i)),
      t = days_to_election/30,
      model_y = 0) %>%
select(y_i,
       v_r,
       n_i,
       days_to_election,
       specific_elec,
       poll_bias,
       poll_srs_result,
       t)

#####Specifications
N_poll <- nrow(model_data)
p <- rep(NA, N_poll)

logit_p <- model_data$logit_v + model_data$t * beta + alpha 

for(i in 1:N_poll) {
  p <- invlogit(logit_p)
  model_data$p[i] <- p
  sigma_i <- sqrt( p * (1 -p)/n + )
  model_data$model_y[i] <- normal(model_data$p[i], )
}


g <- levels(model_data$specific_elec)
grouped_model_data <- suppressWarnings(split(model_data, g))






