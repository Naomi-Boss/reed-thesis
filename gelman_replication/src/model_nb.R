#see data exploration for loading and preparing data
#polls_main is dataset from polls_main_dataset.tsv
#polls_extra is dataset from polls_auxiliary_dataset.tsv

#load libraries
library(data.table)
library(tidyverse)
library(extraDistr)

#Generate an election result for each poll in the df using SRS
sim_srs_result_f <- function (v_r, n_i) {
  srs_result <- rbinom(length(n_i), size = n_i, prob = v_r)/n_i
}

polls_main <-polls_main %>% 
  mutate(poll_srs_result = sim_srs_result_f(v_r, n_i))


# ######Modeling
# 
# ####Define any Functions
# logit_f <- function(v_r){
#   v_r / (1 - v_r)
# }
# 
# log_odds <- function(p){
#   p / (1 + p)
# }
# 
# ####Create model data
# model_data <- polls_main %>%
#   mutate(specific_elec = fct_cross(election, electionDate),
#          logit_v = logit_f(v_r)) %>%
#   select(y_i,
#          v_r,
#          n_i,
#          days_to_election,
#          specific_elec,)
# 
# 
# ####Set Parameters
# #hyperparameters
# sigma_tau <- rhnorm(N_polls, 0.05^2)
# sigma_beta <- rhnorm(N_polls, 0.2^2)
# mu_beta <- rnorm(N_polls, 0.2^2)
# sigma_alpha <- rhnorm(N_polls, 0.2^2)
# mu_alpha <- rnorm(N_polls, 0.2^2)
# 
# #parameters
# alpha_j <- rnorm(mu_alpha, sigma_alpha^2)
# beta_j <- rnorm(mu_beta, sigma_beta^2)
# tau_j_sq <- rhnorm(1, sigma_tau^2)
# 
# #constants
# N_polls <- length(model_data)
# 
# #vectors
# logit_p_i <- logit_f(v_r) + alpha_j + beta_j*t_i
# p_i <- log_odds(logit_p_i)
# sigma_sq_i <- (p_i * (1 - p_i))/n_i + tau_j_sq
# 
# y_sim <- rep(N_polls, rnorm(p_i, sigma_sq_i))
# 
# 
# ####preliminaries
# #k is number of elections in consideration
# k <- length(unique(model_data$specific_elec))
# 
# b_r <- 
# 
# mu_b <- 1/k * sum()
# 

#####Data
model_data <- polls_main %>%
  mutate(specific_elec = fct_cross(election, electionDate),
         logit_v = logit_f(v_r),
         poll_bias = 0) %>%
  select(y_i,
         v_r,
         n_i,
         days_to_election,
         specific_elec,
         poll_bias)


g <- levels(model_data$specific_elec)
model_data <- suppressWarnings(split(model_data, g))

#####Constants
N_elec <- length(g)
S_r <- rep(NA, N_elec)

#####Add poll bias to data

for(i in 1:k){
  
  N_polls <- nrow(model_data[[1]])
  
  #hyperparameters
  sigma_tau <- rhnorm(N_polls, 0.05^2)
  sigma_beta <- rhnorm(N_polls, 0.2^2)
  mu_beta <- rnorm(N_polls, 0.2^2)
  sigma_alpha <- rhnorm(N_polls, 0.2^2)
  mu_alpha <- rnorm(N_polls, 0.2^2)
  
  #parameters
  alpha_j <- rnorm(mu_alpha, sigma_alpha^2)
  beta_j <- rnorm(mu_beta, sigma_beta^2)
  tau_j_sq <- rhnorm(1, sigma_tau^2)
  
  for(j in 1:N_polls){
    t_i <- model_data[[1]][j,4]
    model_data[[j]]$poll_bias <- alpha_j[j] + beta_j[j]*t_i
  }
}

##equations
logit(p_i) = logit(v_{r[i]}) + alpha_{r[i]} + beta_{r[i]}*t_i
logit(q_r) = logit(v_r) + alpha_r
sigma_i^2 = p_i*(1 - p_i) / n_i + tau_{r[i]}^2

#average election bias
mu_b = (1/k)*sum(abs(b_r))

#bias for election r
b_r = (1/lenght(S_r))*sum(p_i - v_r)

#absolute bias on election day
mu_beta0 = (1/k)*sum(abs(q_r - v_r))

#average election level standard deviation
mu_sigma = (1/k)*sum(sigma_r)

sigma_r = (1/length(S_r))*sum(sigma_i)


