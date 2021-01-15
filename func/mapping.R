require(tidyverse)
require(sf)
require(tmap)

moh_geo_id = function(df){
  # Function to replace Ministry of Health PHU IDs with GeoHub IDs
  # df is a data frame with a Unit_ID column
  setwd("D:/Data Science/COVID-19 Case Study")
  
  correspondence = read_csv("data/mapping/correspondence.csv", col_types=cols()) %>%
    select(Unit_ID, Geo_ID)
  
  return(left_join(df, correspondence))
}

make_spatial = function(df, sp){
  # Adds data contained in a dataframe to a shapefile, merging by common IDs
  return(merge(x=sp, y=df, by.x="PHU_ID", by.y="Geo_ID"))
}

make_map = function(df, column, palette, filename=NULL, return=TRUE, style="pretty"){
  # Create spatial data
  mapData = df %>% 
    moh_geo_id() %>%
    make_spatial(sp=st_read("data/mapping/shapefile/Ministry_of_Health_Public_Health_Unit_Boundary.shp"))
  
  # Map
  map = tm_shape(mapData) +
    tm_polygons(column, border.col="white", palette=palette, style=style) +
    tm_layout(legend.width=0.39)
  
  if (!is.null(filename)){
    tmap_save(map, filename=filename)
  }
  
  if (return){
    return(map)
  }
}