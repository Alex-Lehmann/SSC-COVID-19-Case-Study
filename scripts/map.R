setwd("D:/Data Science/COVID-19 Case Study")
source("func/mapping.R")

data = read_csv("data/LASSO Maps Data.csv") %>%
  rename(`Inuit Population Proportion` = Inuit_Population_Proportion,
         `Proportion Aged 60-64` = Age_60_64_Proportion,
         
         `Proportion Aged 50-54` = Age_50_54_Proportion,
         `Population Density (Persons Per Sq. Km.)` = Population_Density,
         `Retail Proportion of Workforce` = Retail_Workforce_Proportion,
         `Métis Population Proportion` = Metis_Population_Proportion,
         `Multiple Indigenous Identities Population Proportion` = Multiple_Indigenous_Indenities_Population_Proportion,
         `Proportion Aged 0-14` = Age_0_14_Proportion,
         `Proportion Aged 35-39` = Age_35_39_Proportion,
         `Proportion Aged 30-34` = Age_30_34_Proportion)
shp = st_read("data/mapping/shapefile/Ministry_of_Health_Public_Health_Unit_Boundary.shp")

data = moh_geo_id(data)
toMap = make_spatial(data, shp)

# Basic average age map
(map = tm_shape(toMap) +
    tm_polygons("Proportion Aged 35-39", border.col="white", palette="YlGn"))

tmap_save(map, filename="report/maps/age 35-39.png")
