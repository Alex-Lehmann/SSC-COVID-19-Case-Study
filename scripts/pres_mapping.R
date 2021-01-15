setwd("D:/Data Science/COVID-19 Case Study")
require(tidyverse)
source("func/mapping.R")

# Shapefile
onShape = st_read("data/mapping/shapefile/Ministry_of_Health_Public_Health_Unit_Boundary.shp")

###################################
# Plot 1: COVID-19 death proportion
###################################

# Load data
cases = read_csv("data/cases_ts_Nov16.csv", col_types=cols())
deaths = read_csv("data/deaths_ts_Nov16.csv", col_types=cols())

# Create spatial data
map1Data = inner_join(cases, deaths, by=c("Date", "Unit_ID", "Name_Full", "Name_Short")) %>%
  group_by(Unit_ID) %>%
  summarize(Cases = sum(New_Cases),
            Deaths = sum(New_Deaths)) %>%
  mutate(`Death Rate` = Deaths / Cases) %>%
  moh_geo_id() %>%
  make_spatial(onShape)

# Map
map1 = tm_shape(map1Data) +
  tm_polygons("Death Rate", border.col="white", palette="Reds")
tmap_save(map1, filename="pres/maps/deathRate.png")

#############################
# Increasing Factors Examples
#############################

census = read_csv("data/ON_HealthUnit_Census.csv", col_types=cols())
inuit = census %>%
  mutate(`Inuit Proportion` = Inuit_Population / Population) %>%
  select(Unit_ID, `Inuit Proportion`)
make_map(inuit, "Inuit Proportion", "YlOrRd", filename="pres/maps/inuit.png")

early60s = census %>%
  mutate(`Proportion Aged 60-64` = `Age_60-64` / Population) %>%
  select(Unit_ID, `Proportion Aged 60-64`)
make_map(early60s, "Proportion Aged 60-64", "YlOrRd", filename="pres/maps/early60s.png")

#############################
# Decreasing Factors Examples
#############################

metis = census %>%
  mutate(`Métis Proportion` = Metis_Population / Population) %>%
  select(Unit_ID, `Métis Proportion`)
make_map(metis, "Métis Proportion", "YlGn", filename="pres/maps/metis.png")

multipleIndigenous = census %>%
  mutate(`Multiple Indigenous Identities Proportion` = Multiple_Indigenous_Indenities_Population / Population) %>%
  select(Unit_ID, `Multiple Indigenous Identities Proportion`)
make_map(multipleIndigenous, "Multiple Indigenous Identities Proportion", "YlGn", filename="pres/maps/multipleIndigenous.png")

###########################
# Overall Increase/Decrease
###########################

increase = census %>%
  transmute(Unit_ID = Unit_ID,
            PopDensity = (Population / Land_Area_KM)*0.0018486792,
            Age50s = (`Age_50-54` / Population)*0.0032887984,
            Age60s = (`Age_60-64` / Population)*0.0106240204,
            HouseholdSize = (Persons_in_Households / Number_Of_Private_Households)*0.0084900587,
            Retail = (Retail_Workforce / Total_Workforce)*0.0004764546,
            Inuit = (Inuit_Population / Population)*0.0112771051) %>%
  transmute(Unit_ID = Unit_ID,
            Increase = PopDensity + Age50s + Age60s + HouseholdSize + Retail + Inuit)
make_map(increase, "Increase", "Reds", filename="pres/maps/increase.png", style="quantile")

decrease = census %>%
  transmute(Unit_ID = Unit_ID,
            Children = (`Age_0-14` / Population)*0.0022949690,
            Early30s = (`Age_30-34` / Population)*0.0007035862,
            Late30s = (`Age_35-39` / Population)*0.0012208098,
            Metis = (Metis_Population / Population)*0.0069268028,
            Multiple = (Multiple_Indigenous_Indenities_Population / Population)*0.0058739493) %>%
  transmute(Unit_ID = Unit_ID,
            Decrease = Children + Early30s + Late30s + Metis + Multiple)
make_map(decrease, "Decrease", "Greens", filename="pres/maps/decrease.png", style="quantile")
