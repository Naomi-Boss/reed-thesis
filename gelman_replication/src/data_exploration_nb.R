#load libraries
library(data.table)
library(tidyverse)

#function to process polls and remove any corrupted entries

preprocess_polls_f <- function(input_polls_data) {
  input_polls_data <- input_polls_data %>%
    mutate(y_i = republican/(democratic + republican),
           v_r = finalTwoPartyVSRepublican/100,
           twoparty_voteshare_error = y_i - v_r,
           days_to_election = as.integer(as.Date(electionDate) - as.Date(endDate)),
           n_i = round(numberOfRespondents * (democratic + republican)/100),
           state_year_concat = paste(as.character(state), as.character(year), sep = "_")) %>%
    drop_na(n_i,
            twoparty_voteshare_error,
            state_year_concat,
            days_to_election) %>%
    filter(days_to_election >= 1 & days_to_election <= 100,
           state != "USA",
           election != "House")
}

#load data
polls_main <- fread("polling-errors-master/data/polls_main_dataset.tsv") %>%
  preprocess_polls_f()

polls_extra <- fread("polling-errors-master/data/polls_auxiliary_dataset.tsv") %>%
  preprocess_polls_f()


#main dataset only considers polls conducted within three weeks prior to election
polls_main %>% filter(days_to_election <= 21)


