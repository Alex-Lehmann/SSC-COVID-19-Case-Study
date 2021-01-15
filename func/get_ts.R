require(tidyverse)
require(lubridate)


get_confcase_ts = function(mode="report", destfile=NULL, return=TRUE){
  
  #########################################################################################
  #
  # Time Series - Confirmed Cases
  #
  # Downloads most recent Ontario provincial reporting on confirmed cases of COVID-19 and
  # transforms it into a health-unit level time series. Missing dates are estimated by mean
  # delay from next most-reliable date available.
  #
  # Args:
  #   mode ("report" or "episode") Default: "report"
  #     "report" mode will index cases by case reporting date. "episode" mode will index
  #     cases by estimated episode date.
  #   destfile (character; optional)
  #     If present, a .csv file containing the time series will be saved to the passed
  #     disk location.
  #   return (logical) Default: TRUE
  #     Whether or not to return a tibble containing the time series.
  #
  # Returns: tibble
  #
  #########################################################################################
  
  # Download latest case data from Ontario Data Catalogue
  cases = read_csv("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv",
                   col_types=cols()) %>%
    mutate(Date_Episode = as_date(Accurate_Episode_Date),
           Date_Report = as_date(Case_Reported_Date),
           Date_Test = as_date(Test_Reported_Date),
           Date_Specimen = as_date(Specimen_Date)) %>%
    rename(Outcome = Outcome1, Postal_Code = Reporting_PHU_Postal_Code) %>%
    select(Postal_Code, Date_Episode, Date_Report, Date_Test, Date_Specimen, Outcome)
  
  # Replace missing and erroneous episode dates with next best available
  if (mode == "report"){
    cases = cases %>%
      mutate(Date = coalesce(Date_Report, Date_Test, Date_Specimen)) %>%
      select(-c(Date_Episode, Date_Report, Date_Test, Date_Specimen))
  } else if (mode == "episode"){
    report_delay = mean(cases$Date_Report - cases$Date_Episode, na.rm=TRUE)
    test_delay = mean(cases$Date_Test - cases$Date_Episode, na.rm=TRUE)
    specimen_delay = mean(cases$Date_Specimen - cases$Date_Episode, na.rm=TRUE)
    
    cases = cases %>%
      mutate(Date = coalesce(Date_Episode,
                             Date_Report - report_delay,
                             Date_Test - test_delay,
                             Date_Specimen - specimen_delay)) %>%
      select(-c(Date_Episode, Date_Report, Date_Test, Date_Specimen))
  } else (stop("Invalid mode selected"))
  
  # Convert to time series
  ts = cases %>%
    group_by(Date, Postal_Code) %>%
    summarize(New_Cases = n(), New_Fatal = sum(Outcome == "Fatal"), .groups="drop")
  
  # Fill in missing days
  phu_table = read_csv("data/ON_HealthUnit_Dictionary.csv", col_types=cols()) %>%
    filter(!is.na(Postal_Code))
  
  date_range = as_date(min(ts$Date):max(ts$Date))
  spine = tibble(Date = rep(date_range, each=34),
                 Postal_Code = rep(phu_table$Postal_Code, length(date_range)))
  ts = left_join(spine, ts, by = c("Date", "Postal_Code")) %>%
    replace_na(list(New_Cases = 0, New_Fatal = 0))
  
  # Add PHU details
  phu_details = phu_table %>% select(Unit_ID, Name_Full, Name_Short, Postal_Code)
  ts = left_join(ts, phu_details, by = "Postal_Code") %>%
    select(Date, Unit_ID, Name_Full, Name_Short, New_Cases, New_Fatal) %>%
    arrange(Unit_ID, Date)
  
  # Store local copy if requested
  if (!is.null(destfile)){
    write.csv(ts, destfile, row.names=FALSE)
  }
  
  # Return time series data if requested
  if (return){
    return(ts)
  }
}


get_deaths_ts = function(destfile=NULL, return=TRUE){
  
  #########################################################################################
  #
  # Time Series - Deaths
  #
  # Downloads latest COVID-19 Ontario Open Data Working Group time series data for COVID-19
  # deaths and represents it for easy compatibility with other time data.
  #
  # Args:
  #   destfile (character: optional)
  #     If present, a .csv file containing the time series will be saved to the passed
  #     disk location.
  #   return (logical) Default: TRUE
  #     Whether or not to return a tibble containing the time series.
  #
  # Returns: tibble
  #
  #########################################################################################
  
  # Download deaths timeseries from GitHub
  deaths = read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/mortality_timeseries_hr.csv",
                    col_types=cols()) %>%
    filter(province == "Ontario" & health_region != "Not Reported") %>%
    mutate(Date = as_date(date_death_report, format="%d-%m-%y")) %>%
    rename(New_Deaths = deaths, match = health_region) %>%
    select(Date, New_Deaths, match)
  
  # Add PHU details
  phu_table = read_csv("data/ON_HealthUnit_Dictionary.csv", col_types=cols()) %>%
    filter(!is.na(Postal_Code)) %>%
    mutate(match = c("Huron Perth", "Algoma", "Brant", "Durham", "Grey Bruce",
                     "Haldimand-Norfolk", "Haliburton Kawartha Pineridge", "Halton",
                     "Hamilton", "Hastings Prince Edward", "Chatham-Kent",
                     "Kingston Frontenac Lennox & Addington", "Lambton",
                     "Leeds Grenville and Lanark", "Middlesex-London", "Niagara",
                     "North Bay Parry Sound", "Northwestern", "Ottawa", "Peel",
                     "Peterborough", "Porcupine", "Renfrew", "Eastern", "Simcoe Muskoka",
                     "Sudbury", "Thunder Bay", "Timiskaming", "Waterloo",
                     "Wellington Dufferin Guelph", "Windsor-Essex", "York", "Southwestern",
                     "Toronto")) %>%
    select(Unit_ID, Name_Full, Name_Short, match)
  ts = left_join(deaths, phu_table, by="match") %>%
    select(Date, Unit_ID, Name_Full, Name_Short, New_Deaths) %>%
    arrange(Unit_ID, Date)
  
  # Store local copy if requested
  if (!is.null(destfile)){
    write.csv(ts, destfile, row.names=FALSE)
  }
  
  # Return time series data if requested
  if (return){
    return(ts)
  }
}


update_active_ts = function(date = today() - 1){
  setwd("D:/Data Science/COVID-19 Case Study")
  
  # Load active cases file. Check if data input would be redundant
  ts = read_csv("data/ON_Active_Cases.csv", col_types=cols())
  if (date %in% ts$Date) {stop("Date already in file")}
  
  # Get most recent confirmed case data
  cases = read_csv("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv",
                   col_types=cols())
  
  # Count active cases in each PHU
  active = cases %>%
    rename(Postal_Code = Reporting_PHU_Postal_Code) %>%
    filter(Outcome1 == "Not Resolved") %>%
    group_by(Postal_Code) %>%
    summarize(Active = n())
  
  # Merge with PHU information
  phuTable = read_csv("data/ON_HealthUnit_Dictionary.csv", col_types=cols()) %>%
    filter(!is.na(Postal_Code))
  active = left_join(phuTable, active, by="Postal_Code") %>%
    replace_na(list(Active = 0)) %>%
    mutate(Date = date) %>%
    select(Date, Unit_ID, Name_Full, Name_Short, Active)
  
  # Add to file
  write.csv(rbind(ts, active), "data/ON_Active_Cases.csv", row.names=FALSE)
}