library(data.table)
library(janitor)
library(tidyverse)
set.seed(0119)

####### Load Data ######################################## 
#read in data to environment
aux_polls <- fread("polling-errors-master/data/polls_auxiliary_dataset.tsv")
aux_polls <- clean_names(aux_polls) #change to snake case
main_polls <- fread("polling-errors-master/data/polls_main_dataset.tsv")
main_polls <- clean_names(main_polls) #change to snake case

summary(main_polls)

# From the summary we see that there are 4221 observations.
# The following variables have class and mode as
# character:
#   'state', 'poll_name', 'start_date', 'end_date',
#   'election', 'election_date'
# logical:
#   'someone_else', 'undecided', 'refulse_to_answer'
# Respoectively
# ('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.'):
#   'year': (1998, 2002. 2006, 2006, 2010, 2014)
#   'number_of_respondents': (246.0, 577.0, 634.0, 831.2, 820.0, 25017.0)
#   'democratic': (9.00, 42.00, 46.60, 45.82, 50.00, 76.00)
#   'republican': (11.00, 41.00, 45.70, 45.22, 49.00, 82.00)
#   'final_two_party_vs_democratic':
#     (15.03, 46.58, 51.26, 50.74, 55.20, 84.97)
#   'final_two_party_vs_republican':
#     (15.03, 44.80, 48.74, 49.26, 53.42, 84.97)


summary(aux_polls)

# From the summary we see that there are 10444 observations.
# The following variables have class and mode as character:
# 'election', 'state', 'poll_name', 'start_date', 'end_date'
# 'someone_else', 'population_type', 'mode', 'election_date'
# For the following variables, we have respectively
# 'Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max'
# 'year': (2004, 2006, 2008, 2008, 2010, 2012)
# 'number_of_respondents': (1, 599, 666, 643, 760, 3277)
# 'democratic': (2.00, 42.00, 47.00, 45.97, 50.00, 73.00)
# 'republican': (5.00, 40.00, 44.00, 43.95, 48.00, 78.00)
# 'undecided': (42.42, 42.62, 48.46, 49.14, 54.70, 63.10, NA:10317)
# 'refuse_to_answer': (34.31, 43.17, 51.54, 48.80, 54.60, 54.61, NA:10317)
# 'final_two_party_vs_democratic':
#   (20.23, 48.94, 52.83, 52.38, 56.39, 82.13)
# 'final_two_party_vs_repbulican':
#   (17.87, 43.61, 47.17, 47.62, 51.06, 79.77)



#######Process Polls####################################

poll_processing <- function(input_poll_data) {
  input_poll_data %>%
    mutate(pred_r_vs = republican/(democratic + republican)) %>%
    mutate(final_r_vs = final_two_party_vs_republican/100) %>%
    mutate(r_vs_error = pred_r_vs - final_r_vs) %>%
    mutate(days_until_elec = as.integer(as.Date(election_date) - as.Date(end_date))) %>%
    mutate(resp_two_party_pref = round(number_of_respondents*(democratic + republican) / 100)) %>%
    mutate(state_year_concat = paste(as.character(state), as.character(year, sep="_"))) %>%
    drop_na(resp_two_party_pref) %>%
    drop_na(r_vs_error) %>%
    drop_na(state_year_concat) %>%
    drop_na(days_until_elec) %>%
    filter(days_until_elec >= 1) %>%
    filter(days_until_elec <= 100) %>%
    filter(state != "USA") %>%
    filter(election != "House")
}

main_polls <- poll_processing(main_polls)
main_polls %>%
  filter(days_until_elec <= 21)

aux_polls <- poll_processing(aux_polls)


#######Error################################

#Simulate SRS Poll
srs_result_sim <- function(vs_r, n_i) {
  srs_result <- rbinom(length(n_i), size = n_i, prob = v_r) / n_i
  return (srs_result)
}

