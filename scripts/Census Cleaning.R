# Author: Andrea Payne


library(tidyverse)

# loading in data
censusData <- read_csv("ON_HealthUnit_Census.csv")
confCasesTS <- read_csv("cases_ts_Nov16.csv")
deathsTS <- read_csv("deaths_ts_Nov16.csv")

# creating proportions
censusDataProportions = censusData %>% 
  transmute(Unit_ID, Name_Full, Name_Short, Age_Average, Average_Income,
            Population_Density = Population/Land_Area_KM, LIMAT, LICOAT,
            #proportioning ages by population
            Age_0_14_Proportion = Age_0_14/Population, 
            Age_15_19_Proportion = Age_15_19/Population,
            Age_20_24_Proportion = Age_20_24/Population,
            Age_25_29_Proportion = Age_25_29/Population,
            Age_30_34_Proportion = Age_30_34/Population,
            Age_35_39_Proportion = Age_35_39/Population,
            Age_40_44_Proportion = Age_40_44/Population,
            Age_45_49_Proportion = Age_45_49/Population,
            Age_50_54_Proportion = Age_50_54/Population,
            Age_55_59_Proportion = Age_55_59/Population,
            Age_60_64_Proportion = Age_60_64/Population,
            Age_65_69_Proportion = Age_65_69/Population,
            Age_70_74_Proportion = Age_70_74/Population,
            Age_75_79_Proportion = Age_75_79/Population,
            Age_80_84_Proportion = Age_80_84/Population,
            Age_Over85_Proportion = Age_Over85/Population,
            #proportioning people in private households by population
            Persons_in_Households_Proportion = Persons_in_Households/Population,
            #proportioning workforce by population
            Total_Workforce_Proportion = Total_Workforce/Population,
            Retail_Workforce_Proportion = Retail_Workforce/Population,
            Education_Workforce_Proportion = Education_Workforce/Population,
            Healthcare_Workforce_Proportion = Healthcare_Workforce/Population,
            #proportioning minority populations by population
            Visible_Minority_Population_Proportion = 
              Visible_Minority_Population/Population,
            Chinese_Population_Proportion = Chinese_Population/Population,
            First_Nations_Population_Proportion = 
              First_Nations_Population/Population,
            Metis_Population_Proportion = Metis_Population/Population,
            Inuit_Population_Proportion = Inuit_Population/Population,
            Multiple_Indigenous_Indenities_Population_Proportion = 
              Multiple_Indigenous_Indenities_Population/Population,
            #proporitoning public transportion commute by popuation
            Public_Transportation_Commute_Proportion = 
              Public_Transportation_Commute/Population,
            #proportioning highest level of education by population
            No_Certificate_Diploma_Degree_Proportion = 
              No_Certificate_Diploma_Degree/Population,
            Secondary_School_Or_Equivalent_Proportion = 
              Secondary_School_Or_Equivalent/Population,
            Trades_Certificate_Proportion = Trades_Certificate/Population,
            Certificate_Of_Apprenticeship_or_Qualification_Proportion = 
              Certificate_Of_Apprenticeship_or_Qualification/Population,
            College_Or_CEGEP_Diploma_Proportion = 
              College_Or_CEGEP_Diploma/Population,
            University_Diploma_Below_Bachelor_Proportion = 
              University_Diploma_Below_Bachelor/Population,
            University_Bachelors_Degree_Proportion = 
              University_Bachelors_Degree/Population,
            University_Masters_Degree_Proportion = 
              University_Masters_Degree/Population,
            University_Earned_Doctorate_Proportion = 
              University_Earned_Doctorate/Population,
            #proportioning apartments and multi census households by number of private households
            Apartment_Greater_Or_Equal_To_Five_Storeys_Proportion = 
              Apartment_Greater_Or_Equal_To_Five_Storeys/Number_Of_Private_Households,
            Apartment_Less_Than_Five_Storeys_Proportion = 
              Apartment_Less_Than_Five_Storeys/Number_Of_Private_Households,
            Multi_Census_Households_Proportion = 
              Multi_Census_Households/Number_Of_Private_Households)

# getting covid data, summarizing the times-series and creating death proportion
casesWithDeathsTS = inner_join(confCasesTS, deathsTS)
cumulativeCasesAndDeathsByHealthRegion = casesWithDeathsTS %>% group_by(Unit_ID) %>% 
  summarize(Total_Cases = sum(New_Cases), Total_Deaths = sum(New_Deaths)) %>% 
  mutate(deathProportion = Total_Deaths/Total_Cases)


# creating matrix to be used in regression
nonStandardizedRegressionMatrix = inner_join(censusDataProportions, cumulativeCasesAndDeathsByHealthRegion) %>%
  select(-c(Unit_ID,Name_Full, Name_Short, Total_Cases, Total_Deaths))
