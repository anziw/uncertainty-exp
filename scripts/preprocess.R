# Set working directory to home of repo
source('util.R')
library(tidyverse)

### Preprocess results from image experiment

dat <- read.pcibex("results_full_image.csv")

dat <- create_randomids(dat, 
                        'randid_mapping_image.csv',
                        'results_randomized_image.csv')

## Filter data

non_us_ids <- get_non_us(dat)
low_eng_ids <- get_low_eng_use(dat)
low_acc_ids <- get_low_acc(dat)

exclude <- unique(c(non_us_ids, low_eng_ids, low_acc_ids))

filtered_dat <- dat %>%
  filter(!random_id %in% exclude)

## Add in relevant columns

processed_dat <- filtered_dat %>%
  filter(PennElementName %in% c("slider-1", "slider-2", "slider-3")) %>%
  select(random_id, Order.number.of.item, trial_id, PennElementName, Value) %>%
  mutate(utterance = ifelse(PennElementName == "slider-1", "probably",
                            ifelse(PennElementName == "slider-2", "might", "bare not")),
         outcome = readr::parse_number(trial_id)*10,
         event_type = ifelse(str_detect(trial_id, 'g'), 'gumball', 'election'),
         color = ifelse(str_detect(trial_id, 'p') | str_detect(trial_id, 'x'), 'purple', 'orange')) %>%
  select(-PennElementName) %>%
  rename(value = Value, trial_num = Order.number.of.item) %>%
  group_by(random_id, trial_num) %>%
  mutate(sum = sum(as.numeric(value)),
         prob = ifelse(sum==0, 0, as.numeric(value)/sum)) 

## Save data

write.csv(processed_dat, 'processed_data_image.csv')


### Preprocess results from text experiment

dat <- NULL
dat <- read.pcibex("results_full_text.csv")

dat <- create_randomids(dat, 
                        'randid_mapping_text.csv',
                        'results_randomized_text.csv')

## Filter data

non_us_ids <- get_non_us(dat)
low_eng_ids <- get_low_eng_use(dat)
low_acc_ids <- get_low_acc(dat)

exclude <- unique(c(non_us_ids, low_eng_ids, low_acc_ids))

filtered_dat <- dat %>%
  filter(!random_id %in% exclude)

## Add in relevant columns

processed_dat <- filtered_dat %>%
  filter(PennElementName %in% c("slider-1", "slider-2", "slider-3")) %>%
  select(random_id, Order.number.of.item, trial_id, PennElementName, Value) %>%
  mutate(utterance = ifelse(PennElementName == "slider-1", "probably",
                            ifelse(PennElementName == "slider-2", "might", "bare not")),
         outcome = readr::parse_number(trial_id)*10,
         event_type = ifelse(str_detect(trial_id, 'g'), 'gumball', 'election'),
         color = ifelse(str_detect(trial_id, 'p') | str_detect(trial_id, 'x'), 'purple', 'orange')) %>%
  select(-PennElementName) %>%
  rename(value = Value, trial_num = Order.number.of.item) %>%
  group_by(random_id, trial_num) %>%
  mutate(sum = sum(as.numeric(value)),
         prob = ifelse(sum==0, 0, as.numeric(value)/sum))

## Save data

write.csv(processed_dat, 'processed_data_text.csv')



