#' ---
#' title: "Data Wrangling for Miller Heller project"
#' author: "Ian J. Kahrilas"
#' date: "2021/7/3"
#' output: "html_document"
#' ---
#+ load packages
library(tidyverse)
library(here)
library(glue)
#'
#' create vector with file names
#+ vector of all files in working directory
# iterable values in file path names
pids <- c(401, 404, 406, 409)
file_names <- c(532401, 583404, 565406, 526409)
# actual file paths
files_mul <- glue("CW_Exported_Files/CW_{pids}/CW{file_names}_av-export.mul") # mul files
files_evt <- glue("CW_Exported_Files/CW_{pids}/CW{file_names}_av-export.evt") # evt files

#' read in files as data sets
#+ reading in the data
evt <- map_df(files_evt, ~ {
  read_table(here("data", "paper_three", "headspace_evt_files", .x)) %>%
    select(-Tmu) %>%
    rename("block:n_trials" = `Code\tTriNo\tComnt`) %>%
    mutate(`block:n_trials` = str_remove(`block:n_trials`, "42\t200000\t")) %>%
    separate(`block:n_trials`, into = c("block", "n_trials"), sep = ":") %>%
    mutate(n_trials = str_remove(n_trials, "avs"),
           n_trials = as.numeric(n_trials),
           pid = as.numeric(str_extract(.x, "[0-9]+")))
})


#'
#' Trials names
#+ list of trial names
trial_names <- c("fixation",
                 "congruent-block-CT",
                 "incongruent-block-CT",
                 "neutral-block-CT",
                 "pure-congruent-CT",
                 "pure-incongruent-CT",
                 "neutral-congruent-CT",
                 "neutral-incongruent-CT",
                 "congruent-block-AT",
                 "incongruent-block-AT",
                 "neutral-block-AT",
                 "pure-congruent-AT",
                 "pure-incongruent-AT",
                 "neutral-congruent-AT",
                 "neutral-incongruent-AT"
)
#' define vector of number of total possible trials for each to derive proportion of retained trials for each participant
#+ total trials vector
tot_trials <- c(64, 64, 62, 124, 32, 30, 32, 32, 64, 64, 124, 32, 32, 32, 32)
#' Read in files
#+ map over all file names
# preallocate space
eeg_df <- as_tibble(matrix(data = NA_real_, nrow = 648000, ncol = 67))
# read in data
eeg_df <- map2_df(mul_names, evt_names, ~ {
  mul <- read_table2(here("Data", "Batch_Export_2020_5_5", "erp", .x), skip = 1) %>%
    mutate(trial_type = rep(trial_names,
                            each = (nrow(.) / length(trial_names))
    ),
    pid = str_extract(.x, "[0-9]{6}"),
    ms = rep(seq(-200, 1400,
                 by = ((1600 + (1600 / 400))/ 400)),
             times = 15)
    )
  evt <- read_table(here("Data", "Batch_Export_2020_5_5", "evt", .y)) %>%
    separate(`Code\tTriNo\tComnt`, into = c("trial_type", "n_trials"), sep = ":") %>%
    mutate(trial_type = str_extract(trial_type, trial_names),
           n_trials = as.numeric(str_extract(n_trials, "[0-9]{2}")),
           pid = str_extract(.y, "[0-9]{6}"),
           total_trials = tot_trials,
           prop_trials = n_trials / tot_trials) %>%
    select(-Tmu)
  full_join(mul, evt, by = c("pid", "trial_type"))
}
)

eeg_df <- eeg_df %>%
  mutate(EEG59_avr = coalesce(eeg_df$EEG59_avr, eeg_df$"M1'_avr")) %>%
  select(-"M1'_avr")

names(eeg_df) <- c("EXG1",
                   "EXG2",
                   paste0("EEG", 1:59),
                   "trial_type",
                   "pid",
                   "ms",
                   "n_trials",
                   "total_trials",
                   "prop_trials")

cases_to_omit <- c("338006", "316029", "378037", "386039", "351049",
                   "378051", "344059", "366061", "341105", "547213",
                   "545232", "545233", "528246", "548304",
                   "532305", "512309")

eeg_df <- eeg_df %>%
  filter(!(pid %in% cases_to_omit))

write_csv(eeg_df, "Data/created_data/eeg_dat.csv")