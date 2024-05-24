################################################################################
#                                                                              #
# Purpose:       Parvo Plot Script                                             #
#                                                                              #
# Author:        Philipp Reyher                                                #
# Contact:       reyher.philipp@gmail.com                                      #
# Client:        Philipp Reyher                                                #
#                                                                              #
# Code created:  2022-10-28                                                    #
# Last updated:  2023-06-06                                                    #
# Source:        /metabolic_reportR/scripts/                                   #
#                                                                              #
# Comment:       Script aims to automate the creation of plots using the Parvo #
#                Metabolic Cart data                                           #
#                                                                              #
################################################################################
################################### Packages ###################################
library(purrr)
library(data.table)
library(tidytable)
library(ggplot2)
library(signal)
library(gridExtra)
library(grid)
library(here)
############################# Global Vars/Options ##############################

dir <- here::here()
setwd(dir)
gxt_plots_path <- file.path(dir, "output", "plots", "gxt_plots")
threshold_plots_path <- file.path(dir, "output", "plots", "threshold_plots")
file_path <- file.path(dir, "data")
file_list <- list.files(
  path = file_path, pattern = "*.csv",
  ignore.case = TRUE, full.names = TRUE
)

################################## Functions ###################################
source(here::here("scripts/functions/0-globalFuns.R"))
source(here::here("scripts/functions/1-import.R"))
source(here::here("scripts/functions/2-extractRegex.R"))
source(here::here("scripts/functions/3-tidying.R"))
source(here::here("scripts/functions/4-preprocessing.R"))
source(here::here("scripts/functions/5-ventilatoryParametersExtraction.R"))
source(here::here("scripts/functions/6-reportTables.R"))
source(here::here("scripts/functions/7-plots.R"))
source(here::here("scripts/functions/8-export.R"))
#################################### Import ####################################
test_data <- import_filelist(file_list)
################### Extract Demographics and Test-parameters ###################
participant_names <- extract_participant_names(test_data)
participant_names <- gsub("[^[:alnum:]]", "", participant_names)
participant_name_list <- as.list(participant_names)
names(test_data) <- participant_names
metadata <- extract_metadata(test_data)
metadata <- extract_test_date(
  extract_from = file_list,
  append_to = metadata
)
############################### Tidy Up Dataset ################################
test_data <- tidy_up(test_data)
############################### Extend Metadata ################################
metadata <- extract_start_end_indices(
  extract_from = test_data,
  append_to = metadata
)
################################ Preprocessing #################################
# IMPORTANT - CHOOSE ONE FILTER AND ONLY EXECUTE THAT LINE;
# THE FILTERS OVERWRITE EACH OTHER
test_data <- apply_low_pass_filter(test_data)
test_data <- apply_moving_average_filter(test_data, k = 15)
# variable computation
test_data <- compute_ventilatory_vars(test_data)
# Truncation
test_data_truncated <- truncate_data(data_list = test_data, metadata = metadata)
# interpolation
test_data_interpolated_seconds <- interpolate_to_seconds(test_data_truncated)
# binning
test_data_10_binned <- apply_bin_average(test_data_interpolated_seconds,
                                         bin = 10)
########################## Calculation of VT1,VT2,Max ##########################
changepoints_data <- find_vent_thresholds_data(test_data_10_binned)
vo2max_data <- find_vo2max_data(test_data)
summary_tables <- create_summary_tables(changepoints_data, vo2max_data)
################################ Report-Tables #################################
summary_tables_formatted <- format_summary_table(summary_tables)
test_details_formatted <- format_test_details(metadata)
gxt_tables_formatted <- create_gxt_table(test_data_truncated, vo2max_data)
coggans_tables_formatted <- create_coggans_zones_table(changepoints_data)
ais_tables_formatted <- create_ais_zones_table(vo2max_data)
#################################### Plots #####################################
create_gxt_plots(
  test_data, metadata, changepoints_data, participant_name_list,
  gxt_plots_path
)
create_threshold_plots(
  test_data_10_binned, changepoints_data,
  participant_name_list, threshold_plots_path
)
#################################### Export ####################################
combined_list <- create_combined_list()
create_reports(combined_list)
