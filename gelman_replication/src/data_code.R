library(data.table)
library(janitor)
library(tidyverse)
library(scales)
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
#   'state', 'poll_name', 'start_date', 'poll_end_date',
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
# 'election', 'state', 'poll_name', 'start_date', 'poll_end_date'
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


####
#The following is based on resarh and code availble at 
#https://github.com/stanford-policylab/polling-errors

#######Process Polls####################################

f_poll_data_prep <- function(input_poll_data) {
  input_poll_data <- input_poll_data %>%
    mutate(pred_r_vs = republican/(democratic + republican),
           election_r_vs = final_two_party_vs_republican/100,
           r_vs_error = pred_r_vs - election_r_vs,
           days_until_elec = as.integer(as.Date(election_date) - as.Date(poll_end_date)),
           resp_prefer_major_two = round(number_of_respondents*(democratic + republican)) / 100,
           state_year_concat = paste(as.character(state), as.character(year, sep="_"))) %>%
    drop_na(resp_prefer_major_two,
            r_vs_error,
            state_year_concat,
            days_until_elec) %>%
    filter(days_until_elec >= 1 & days_until_elec <= 100,
           state != "USA",
           election != "House")
}

main_polls <- f_poll_data_prep(main_polls)
main_polls <- main_polls %>%
  filter(days_until_elec <= 21)

df <- main_polls %>%
  arrange(state, election, election_date)

aux_polls <- f_poll_data_prep(aux_polls)



#######Error################################

#Simulate SRS Poll
f_srs_vs_sim <- function(vs_r, n_i) {
  srs_vs_result <- rbinom(length(n_i), size = n_i, prob = vs_r) / n_i
  return (srs_vs_result)
}

each_election_type_srs_polls_count <- 1000000
#choose 1000000 indexes from each election type
selected_polls_indexes <- c( 
  sample(which(main_polls$election == "Sen"), each_election_type_srs_polls_count, replace = T),
  sample(which(main_polls$election == "Pres"), each_election_type_srs_polls_count, replace = T),
  sample(which(main_polls$election == "Gov"), each_election_type_srs_polls_count, replace = T)
)

selected_polls_srs_vs_results <- f_srs_vs_sim(
  main_polls$election_r_vs[selected_polls_indexes],
  main_polls$resp_prefer_major_two[selected_polls_indexes]
)

srs_polls_data <- data.table(
  election = main_polls$election[selected_polls_indexes], #or is it main_polls[selected_polls_indexes]$election
  r_vs_error = selected_polls_srs_vs_results - main_polls$election_r_vs[selected_polls_indexes]
)

main_polls_data <- main_polls%>%
  select(election, r_vs_error)

srs_and_real_plot_data <- 
  bind_rows( "Actual" = main_polls_data, "SRS" = srs_polls_data, .id = "Source")


histogram_bin_size = 0.01
fill_color = "grey"
line_type = "dashed"

error_actual = srs_and_real_plot_data %>% filter(Source == "Actual")

error_srs = srs_and_real_plot_data %>%
  filter(Source == "SRS") %>%
  select(Source,
         election,
         r_vs_error)

density_comparison <- ggplot() +
  geom_histogram(data = error_actual,
                 aes(x = r_vs_error, y = ..density..),
                 binwidth = 0.01,
                 fill = "grey") +
  geom_density(data = error_srs, 
                 aes(x = r_vs_error),
                 linetype = "dashed") +
  scale_x_continuous(name = "Error",
                     breaks = seq(-.1,0.1,0.05),
                     limits = c(-.1, .1),
                     labels = label_percent()) +
  scale_y_continuous(name = "Density", breaks = NULL, expand = c(0,0)) +
  facet_grid(. ~ election)

density_comparison

histogram_comparison <- ggplot() +
  geom_histogram(data = error_actual,
                 aes(x = r_vs_error),
                 binwidth = 0.01,
                 fill = "grey") +
  geom_density(data = error_srs, 
               aes(x = r_vs_error),
               linetype = "dashed") +
  scale_x_continuous(name = "Error",
                     breaks = seq(-.1,0.1,0.05),
                     limits = c(-.1, .1),
                     labels = label_percent()) +
  scale_y_continuous(name = "Number of Polls",
                     expand = c(0,0)) +
  facet_grid(. ~ election)

histogram_comparison

