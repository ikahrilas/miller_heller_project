#' ---
#' title: "Data Wrangling for Miller Heller project"
#' author: "Ian J. Kahrilas"
#' date: "2021/7/3"
#' output: "html_document"
#' ---
#+ load packages
library(tidyverse)
library(furrr)
library(dtplyr)
library(here)
library(glue)
library(data.table)
#'
#' create vector with file names
#+ vector of all files in working directory
# object with all file names
## pids
pids <- list.files(here("Preprocessed_CW_EEG_Data", "Preprocessed_CTQ_Sample"))[c(-106, -109)]

## mul files
mul_files <- map_chr(pids, ~ {
  list.files(here("Preprocessed_CW_EEG_Data", "Preprocessed_CTQ_Sample", .x)) |> str_subset(".mul")
  })

## evt files
evt_files <- map_chr(pids, ~ {
  list.files(here("Preprocessed_CW_EEG_Data", "Preprocessed_CTQ_Sample", .x)) |> str_subset(".evt")
})

# full file paths
files_mul <- glue("Preprocessed_CW_EEG_Data/Preprocessed_CTQ_Sample/{pids}/{mul_files}") # mul files
files_evt <- glue("Preprocessed_CW_EEG_Data/Preprocessed_CTQ_Sample/{pids}/{evt_files}") # evt files

#' read in evt files as data frames
#+ reading in the evt data
# define vector of number of total possible trials for each to derive proportion of retained trials 
# for each participant total trials vector
tot_trials <- c(64, 64, 62, 124, 32, 30, 32, 32, 64, 64, 124, 32, 32, 32, 32)

# prepare parallelization
plan(multisession, workers = 8)

read_csv2(files_evt[1]) |> 
  rename("block:n_trials" = `Tmu         \tCode\tTriNo\tComnt`) |>
  separate(`block:n_trials`, into = c("block", "n_trials"), sep = ":") |>
  mutate(n_trials = str_remove(n_trials, "avs"),
         n_trials = as.numeric(n_trials),
         prop_trials = n_trials / tot_trials)
## pick up here - refine block variable

# map function to read in data
evt <- future_map(files_evt, ~ {
  read_csv2(.x) |>
    select(-Tmu) |>
    rename("block:n_trials" = `Code\tTriNo\tComnt`) |>
    mutate(`block:n_trials` = str_remove(`block:n_trials`, "42\t200000\t")) |>
    separate(`block:n_trials`, into = c("block", "n_trials"), sep = ":") |>
    mutate(n_trials = str_remove(n_trials, "avs"),
           n_trials = as.numeric(n_trials),
           prop_trials = n_trials / tot_trials,
           pid = as.numeric(str_extract(.x, "[0-9]+"))) |>
    relocate(pid, everything())
}) |>
  reduce(bind_rows)

#' read in mul files as data frames
#+ read in eeg data
# vector of unique trial names
trial_names <- evt |> 
  select(block) |> 
  unique() |> 
  pull()

# read in the data
eeg_dat <- future_map(files_mul, ~ {
  read_table2(.x, skip = 1) %>% 
    mutate(
      pid = as.numeric(str_extract(.x, "[0-9]{3}")),
      trial_type = rep(trial_names,
                       each = (nrow(.) / length(trial_names))),
      ms = rep(seq(-200, 1400,
                   by = ((1600 + (1600 / 400))/ 400)),
               times = 15)) |>
      relocate(pid, trial_type, ms, everything())
}
) |> 
  reduce(bind_rows) |>
  select(-c(EVENT, SCL, SCR, `VEOG1-M1'`, `EKG`, `VEOG2-M1'`, `M1'-M1'`))

# remove "-M1'" from variable names
names(eeg_dat) <- str_remove(names(eeg_dat), "-M1'")

#' finally, merge the event and erp data together and output csv file
#+ merge the data together
dat <- full_join(evt, eeg_dat, by = "pid")
# write csv file
write_csv(dat, "data_wide.csv")
