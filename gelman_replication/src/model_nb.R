#see data exploration for loading and preparing data
#polls_main is dataset from polls_main_dataset.tsv
#polls_extra is dataset from polls_auxiliary_dataset.tsv

#load libraries
library(data.table)
library(tidyverse)

#Generate an election result for each poll in the df using SRS
sim_srs_result_f <- function (v_r, n_i) {
  srs_result <- rbinom(length(n_i), size = n_i, prob = v_r)/n_i
}

polls_main <-polls_main %>% 
  mutate(poll_srs_result = sim_srs_result_f(v_r, n_i))


######Modeling

####Define any Functions
logit_f <- function(v_r){
  v_r / (1 - v_r)
}

log_odds <- function(p){
  p / (1 + p)
}

####Create model data
model_data <- polls_main %>%
  mutate(specific_elec = fct_cross(election, electionDate),
         logit_v = logit_f(v_r)) %>%
  select(y_i,
         v_r,
         n_i,
         days_to_election, #t_i
         specific_elec,) %>%
  arrange(specific_elec)

####Set Parameters
#hyperparameters
sigma_tau <- rep(N_polls, rhnorm(0.05^2))
sigma_beta <- rep(N_polls, rhnorm(0.2^2))
mu_beta <- rep(N_polls, rnorm(0, 0.2^2))
sigma_alpha <- rep(N_polls, rhnorm(0.2^2))
mu_beta <- rep(N_polls, rnorm(0, 0.2^2))

#parameters
alpha_j <- rep(N_polls, rnorm(mu_alpha, sigma_alpha^2))
beta_j <- rep(N_polls, rnorm(mu_beta, sigma_beta^2))
tau_j_sq <- rep(N_polls, rhnorm(sigma_tau^2))

#constants
N_polls <- length(model_data)

#vectors
logit_p_i <- logit_f(v_r) + alpha_j + beta_j*t_i
p_i <- log_odds(logit_p_i)
sigma_sq_i <- (p_i * (1 - p_i))/n_i + tau_j_sq

y_sim <- rep(N_polls, rnorm(p_i, sigma_sq_i))


####preliminaries
#k is number of elections in consideration
k <- length(unique(model_data$specific_elec))

b_r <- 

mu_b <- 1/k * sum()
