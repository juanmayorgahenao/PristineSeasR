### ----------------------------------------------------------------------------
### Processing script for eDNA pump and metadata files from the sub
### ----------------------------------------------------------------------------

### Load libraries ---
library(readxl)
library(here)
library(purrr)
library(writexl)
library(tidyverse)

### Locate directory and files ---
#file_dir <- here::here() # If script lives on fast SSD drive
#file_dir <- "/Volumes/SSD-SUB/data" # If script lives on Argo computer or NAS
file_dir <-"/Users/kat/Desktop/sub-files/data"
raw_dir <- file.path(file_dir, "01-raw")
processed_dir <- file.path(file_dir, "02-processed")

### ----------------------------------------------------------------------------
### System Display (Qinsy) -----------------------------------------------------
### ----------------------------------------------------------------------------

### Get system files
qinsy_files <- list.files(file.path(raw_dir, "qinsy"), pattern = "*.txt", full.names = T)
qinsy_files <- qinsy_files[!grepl("~", qinsy_files)] # remove temp files if any

### ---
### Read in all files and wrangle columns
### ---
loadQinsy <- function(f){

  # Load and rename columns
  content <- readr::read_delim(f, delim = ",", show_col_types = F) |>
    janitor::clean_names() |>
    rename_at(vars(contains("steered_node")), ~ str_replace(., "steered_node_", "")) |>
    rename_at(vars(contains("_value")), ~ str_replace(., "_value", "")) |>
    mutate_at(vars(!any_of(c("date", "time", "latitude", "longitude"))), ~as.numeric(.)) |>
    rename_at(vars(any_of(c("prs_14", "prs_15"))), ~ "depth_m") |>
    rename_at(vars(any_of(c("prs_16", "prs_17"))), ~ "prs_1") |>
    mutate(date = lubridate::mdy(str_replace_all(date, "/", "-")),
           year = year(date),
           month = month(date),
           day = day(date),
           time = as.character(time),
           timestamp = as_datetime(paste0(year, "-", month, "-", day, " ", time))) |>
    dplyr::filter(!is.na(timestamp))

  return(content)

}

qinsy_data_raw <- map_df(qinsy_files, loadQinsy) |>
  arrange(timestamp)

### ---
### Clean up lat/lon
### ---

cleanLatLon <- function(dat){

  out <- dat |>
    separate(latitude, c("lat_deg", "lat_min"), sep = ";", remove = F) |>
    separate(longitude, c("lon_deg", "lon_min"), sep = ";", remove = F) |>
    mutate_at(vars(c("lat_deg", "lat_min", "lon_deg", "lon_min")), ~as.numeric(str_replace(., "[A-Z]+", ""))) |>
    mutate(lat = case_when(grepl("S", latitude) ~ -1 * (lat_deg + (lat_min/60)),
                           grepl("N", latitude) ~ (lat_deg + (lat_min/60))),
           lon = case_when(grepl("W", longitude) ~ -1 * (lon_deg + (lon_min/60)),
                           grepl("E", longitude) ~ (lon_deg + (lon_min/60)))) |>
    dplyr::select(-lat_deg, -lat_min, -lon_deg, -lon_min)

  return(out)
}

qinsy_data_clean <- cleanLatLon(qinsy_data_raw)

### ---
### Split files by dive and export raw dive-level data
### ----

splitQinsy <- function(dat){

  out <- dat |>
    mutate(span = lag(timestamp) %--% timestamp,
           time_since_prev = time_length(span, unit = "minute")) |>
    ungroup() |>
    mutate(start_new = case_when(timestamp == min(timestamp) ~ T,
                                 time_since_prev > 60 ~ T))

  # NOTE - The sub system outputs dive number now (and should going forward), but it didn't at the start. This manual specification of dive number won't be necessary to divide later dives and you should just be able to group by dive number.
  summary <- out |> dplyr::filter(!is.na(start_new)) |>
    mutate(dive_num = case_when(!is.na(job_number) ~ job_number,
                                date == "2024-09-16" ~ 42,
                                date == "2024-09-17" ~ 43,
                                date == "2024-09-18" ~ 44,
                                date == "2024-09-21" & time == "00:07:00" ~ 45,
                                date == "2024-09-21" & time == "23:26:21" ~ 46,
                                date == "2024-09-22" ~ 47,
                                date == "2024-09-23" & time == "04:53:05" ~ 48,
                                date == "2024-09-23" & time == "22:57:22" ~ 49,
                                date == "2024-09-25" ~ 50,
                                date == "2024-09-26" ~ 51)) |>
    dplyr::select(date, time, dive_num)

  out <- out |>
    left_join(summary, by = c("date", "time")) |>
    fill(dive_num) |>
    mutate(file_name = case_when(start_new ~ paste0(str_replace_all(date, "-", "_"), "_sub_dive_", dive_num, "_qinsy"))) |>
    fill(file_name) |>
    relocate(dive_num, .before = job_number) |>
    relocate(lat, .before = latitude) |>
    relocate(lon, .before = longitude) |>
    relocate(timestamp, .before = date) |>
    dplyr::select(-job_number, -latitude, -longitude, -year, -month, -day, -span, -time_since_prev, -start_new)

  # And split
  out_list <- split(out, out$file_name)

  return(out_list)

}

qinsy_data_split <- splitQinsy(qinsy_data_clean)

### ---
### Save cleaned up data files
### ----

saveQinsy <- function(dat, save_dir, extension = ".xlsx"){

  new_file <- file.path(save_dir, paste0(unique(dat$file_name), extension))

  has_file <- file.exists(new_file)

  if(!has_file){
    write_xlsx(dat |> dplyr::select(-file_name), new_file)
    Sys.setFileTime(new_file, max(dat$timestamp))
  }
}

# NOTE - This will save the "raw" sub system data as a .xlsx file. These should be put on the SSD drive so the pilots can access them because they need this for their reporting (OR we can find a place on the NAS for them to live if they're ok with accessing them this way).
map(qinsy_data_split, saveQinsy, save_dir = file.path(processed_dir, "qinsy"))

### ---
### Generate summary file
### ----

summarizeQinsy <- function(dat, save_dir, extension = ".xlsx"){

  # Get relevant metadata
  metadata <- dat |>
    summarize(dive_num = unique(dive_num),
              date = min(date, na.rm = T),
              start_time = first(time[depth_m > 2]),
              dive_duration = max(timestamp) -  min(timestamp),
              start_lat_surface = lat[time == start_time],
              start_lon_surface = lon[time == start_time],
              max_depth_m = round(max(depth_m, na.rm = T)),
              time_at_bottom = first(time[depth_m == max(depth_m)]),
              descent_time = hms(time_at_bottom) - hms(start_time),
              start_lat_bottom = lat[time == time_at_bottom],
              start_lon_bottom = lon[time == time_at_bottom],
              file_name = unique(file_name))

  new_file <- file.path(save_dir, paste0(unique(metadata$file_name), "_summary", extension))

  has_file <- file.exists(new_file)

  if(!has_file){
    write_xlsx(metadata |> dplyr::select(-file_name), new_file)
    Sys.setFileTime(new_file, max(dat$timestamp))
  }
}

# NOTE - This saves a summary file for each dive with a single row that has information useful for someone to fill out the fieldbook. This could be updated so that it just adds a row onto an existing file, but that gets more complicated quickly.
map(qinsy_data_split, summarizeQinsy, save_dir = file.path(processed_dir, "qinsy-summaries"))

### ----------------------------------------------------------------------------
### eDNA  ----------------------------------------------------------------------
### ----------------------------------------------------------------------------

# MOLLY AND JUAN - I did not get around to trying to do a similar process of converting/aggregating/processing the raw .txt files for the eDNA system. This is going to be tricky given how messy the output from this system is. Let me know if you want help.

### Get system files
edna_files <- list.files(file.path(raw_dir, "edna"), pattern = "*.txt", full.names = T)
edna_files <- edna_files[!grepl("~", edna_files)] # remove temp files

pump_files <- edna_files[grepl("pump_data", edna_files)]
ui_files <- edna_files[grepl("UI_debug", edna_files)]

### ---
### Pump log
### ---


### ---
### UI files
### ---

