##########################################################################################
# Census Data Aggregator - Ontario Health Regions
# 
# Author: Alex Lehmann
#
# Downloads Statstics Canada census data for Ontario via the CensusMapper API and
# aggregates data to the health unit level. Data for census subdivisions overlapping
# multiple health regions is scaled by the proportion of population actually living in the
# health region. Specific features to retrieve are encoded in the Vector_Map-CA16 file.
# Requires a CensusMapper API key.
#
##########################################################################################

setwd("D:/Data Science/COVID-19 Case Study")

require(tidyverse)
require(cancensus)

options(cancensus.api_key = "CensusMapper_d368730679bb25c2becf6fae8a73d5db")
options(cancensus.cache_path = "data/censusmapper/cache")

# Load and clean reference tables
unit_table = read_csv("data/censusmapper/ONPHU_HR2018_16_updated.csv",
                      col_types=cols()) %>%
  select(CSDUID2016, HRUID2018) %>%
  rename(csd_id = CSDUID2016,
         unit_id = HRUID2018) %>%
  mutate(csd_id = as.character(csd_id),
         unit_id = as.character(unit_id)) %>%
  distinct()

overlap_table = read_csv("data/censusmapper/ONPHU_HR2018_16_overlap.csv",
                         col_types=cols()) %>%
  select(CSDUID2016, HRUID2018_1, HRUID2018_2, POP_1, POP_2) %>%
  rename(csd_id = CSDUID2016,
         unit_1_id = HRUID2018_1,
         unit_2_id = HRUID2018_2,
         unit_1_population = POP_1,
         unit_2_population = POP_2) %>%
  mutate(csd_id = as.character(csd_id),
         unit_1_id = as.character(unit_1_id),
         unit_2_id = as.character(unit_2_id))

vector_map = read_csv("data/censusmapper/Vector_Map-CA16.csv", col_types=cols())
unit_dictionary = read_csv("data/ON_HealthUnit_Dictionary.csv", col_types=cols())

# Load variable vectors
query_vectors = vector_map$Vector

# Download census data for each health unit
province_data = NULL
for (unit in unique(unit_table$unit_id)){
  # Get census data for CSDs in the unit
  query_regions = unit_table %>% filter(unit_id == unit) %>% pull(csd_id)
  csd_data = get_census(dataset="CA16",
                        regions=list(CSD=query_regions),
                        vectors=query_vectors) %>%
    select(-any_of(c("Type", "Region Name", "Area (sq km)",
                     "Population", "Dwellings", "Households"))) %>%
    rename(csd_id = GeoUID)
  colnames(csd_data) = colnames(csd_data) %>%
    str_replace_all(pattern='(?<=:).*|\\(.*\\)|[:\\s]', replacement="")
  
  # Scale appropriate data for CSDs overlapping health unit boundaries
  for (i in 1:dim(csd_data)[1]){
    check_csd = csd_data$csd_id[i]
    if (check_csd %in% overlap_table$csd_id){
      overlap = overlap_table %>% filter(csd_id == check_csd)
      
      # Determine percentage of CSD population in the health unit
      csd_population = overlap[["unit_1_population"]] + overlap[["unit_2_population"]]
      if (unit == overlap[["unit_1_id"]]){
        population_percent = overlap[["unit_1_population"]] / csd_population
      } else { population_percent = overlap[["unit_2_population"]] / csd_population }
      
      # Scale columns as appropriate
      for (j in 2:dim(csd_data)[2]){
        cur_vector = colnames(csd_data)[j]
        if (cur_vector %in% vector_map$Vector){
          if (vector_map %>% filter(Vector == cur_vector) %>% pull(To_Scale)){
            csd_data[i,j] = csd_data[i,j] * population_percent
          }
        }
      }
    }
  }
  
  # Aggregate CSD-level data to health unit-level
  unit_data = cbind((tibble(unit)),
                    filter(unit_dictionary, Unit_ID == unit)$Name_Full,
                    filter(unit_dictionary, Unit_ID == unit)$Name_Short)
  for (column in query_vectors){
    
    aggregation_type = vector_map %>%
      filter(Vector == column) %>%
      pull(Aggregation)
    
    aggregation = switch(aggregation_type,
                         "Sum" = sum(csd_data[[column]], na.rm=TRUE),
                         "Mean" = mean(csd_data[[column]], na.rm=TRUE))
    
    unit_data = cbind(unit_data, aggregation)
  }
  colnames(unit_data) = c("Unit_ID", "Name_Full", "Name_Short", query_vectors)
  
  # Append health unit data to overall provincial data
  province_data = rbind(province_data, unit_data)
}

# Meaningful variable names
var_names = c("Unit_ID", "Name_Full", "Name_Short")
for (column in query_vectors){
  label = vector_map %>% filter(Vector == column) %>% pull(Label)
  var_names = c(var_names, label)
}
colnames(province_data) = var_names

# Save census data to local file
write.csv(province_data %>% arrange(Unit_ID), "data/ON_HealthUnit_Census.csv",
          row.names=FALSE)