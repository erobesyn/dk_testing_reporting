# 2_map_areas

#### 1. LIBRARIES ####
source("./1_load_libraries.R")

#### 2. READ MAP  ####

# 0. GEO_CONNECTIONS
geo_connections <- read_rds("./1 data/3 raw geodata/3 connection arealevels_processed/kommune_landsdel_CLEAN.rds") 
geo_connections <- geo_connections |> rename(LAU_NAME = kommune)

# 1. landsdel (provinces)   # source: Eurostat
nuts3_sf_dk <- gisco_get_nuts(   
  nuts_level = "3",   
  resolution = "1",   
  year = "2021") |> 
  filter(CNTR_CODE == "DK")

nuts3_sf_dk <- nuts3_sf_dk |> mutate(NUTS_NAME_EN = recode(NUTS_NAME,
  "Bornholm" = "Bornholm",
  "Byen København" = "Copenhagen City",
  "Københavns omegn" = "Copenhagen Surroundings",
  "Fyn" = "Funen", 
  "Nordjylland" = "North-Jutland",
  "Vestjylland" = "West-Jutland",
  "Østjylland" = "East-Jutland",
  "Sydjylland" = "South-Jutland",
  "Nordsjælland" = "North-Zealand",
  "Østsjælland" = "East-Zealand",
  "Vest- og Sydsjælland" = "West- and South-Zealand" 
))
                                       
st_crs(nuts3_sf_dk) 
glimpse(nuts3_sf_dk)

# 2. LAU: kommunes (municipalities)     # source: Eurostat
LAU <- st_read("./1 data/3 raw geodata/4 LAU/LAU_RG_01M_2021_3035.shp", options = "ENCODING=UTF-8", stringsAsFactors = FALSE)   
LAU <- LAU |> filter(CNTR_CODE == "DK")
LAU <- LAU |> select(-CNTR_CODE, -GISCO_ID, -POP_DENS_2, -AREA_KM2, -YEAR)  
LAU$LAU_NAME <- gsub("\xf8", "oe", LAU$LAU_NAME, useBytes = TRUE)
LAU$LAU_NAME <- gsub("\xe6", "ae", LAU$LAU_NAME, useBytes = TRUE)
LAU$LAU_NAME <- gsub("\xe5", "aa", LAU$LAU_NAME, useBytes = TRUE)
LAU$LAU_NAME <- gsub("\xc6", "Ae", LAU$LAU_NAME, useBytes = TRUE)
st_crs(LAU)  
glimpse(LAU)

# 3. Apply projected crs of LAU to landsdel
nuts3_sf_dk <- nuts3_sf_dk |> st_transform(st_crs(LAU))
st_crs(nuts3_sf_dk) 

st_write(nuts3_sf_dk, "./1 data/4 processed geodata/5 DK_NUTS3_for_model_visualisation/DK_NUTS3_for_model_visualisation.shp", append=FALSE) 

map_provinces <- tm_shape(nuts3_sf_dk) +                   
  tm_polygons("NUTS_NAME", border.col = "grey", lwd = 2)+ 
  tm_text("NUTS_NAME_EN", size = 0.6) +
  tm_legend(show=FALSE)   
tmap_save(map_provinces, "./5 figs/map_provinces.png")

# 4. OVERLAY kommunes and landsdel

tmap_options(max.categories = 99)

# remove overlapping names from some provinces
geo_connections |> print.all()
LAU_largeareas <- LAU |> left_join(geo_connections) |> 
  filter(! landsdel %in% c("Koebenhavn by", "Koebenhavns omegn", "Nordsjaelland", "Oestsjaelland"))  

overlay_map <- tm_shape(nuts3_sf_dk) +  
  tm_polygons("NUTS_NAME", border.col = "grey", lwd = 2) +  
  tm_legend(show=FALSE) +
  tm_shape(LAU) +
  tm_polygons("LAU_NAME", border.col = "grey",  alpha = 0.3) +   
  tm_shape(LAU_largeareas) + 
  tm_text("LAU_NAME", size = 0.6) +
  tm_legend(show=FALSE) +
  tm_scale_bar() 
overlay_map
tmap_save(overlay_map, "./5 figs/map_kommunes_landsdel.png")

# Innerlines
bord <- ms_innerlines(nuts3_sf_dk)  
bord <-  bord |> st_as_sf() |> mutate(id = 1:10)
tm_shape(bord) + tm_lines("grey60")  

# Inset maps
LAU <- left_join(LAU, geo_connections) |> rename(NUTS_NAME = landsdel)

# overlay_map_inset
nuts3_sf_dk_subset <- nuts3_sf_dk |> filter(NUTS_NAME == "Nordsjælland" )     
LAU_subset <- LAU |> filter(NUTS_NAME == "Nordsjaelland") 

overlay_map_inset <- tm_shape(nuts3_sf_dk_subset) +  
  tm_polygons("NUTS_NAME", border.col = "grey", lwd = 2) +  
  tm_legend(show=FALSE) +
  tm_shape(LAU_subset) +
  tm_polygons("LAU_NAME", border.col = "grey", alph = 0.3) +     
  #tm_borders(alpha = 0.3) +                
  tm_text("LAU_NAME", size = 0.9) + # 0.6
  tm_legend(show=FALSE) +
  tm_scale_bar() +
  tm_layout(main.title  = "North-Zealand", main.title.size = 2, main.title.fontface = "bold") +
  tm_text("LAU_NAME", size = 0.9)  
overlay_map_inset

# overlay_map_inset_2
nuts3_sf_dk_subset_2 <- nuts3_sf_dk |> filter(NUTS_NAME == "Byen København" | NUTS_NAME == "Københavns omegn") 
LAU_subset_2 <- LAU |> filter(NUTS_NAME == "Koebenhavn by" | NUTS_NAME == "Koebenhavns omegn")

overlay_map_inset_2 <- tm_shape(nuts3_sf_dk_subset_2) +  
  tm_polygons("NUTS_NAME", border.col = "grey", lwd = 2) +  
  tm_legend(show=FALSE) +
  tm_shape(LAU_subset_2) +
  tm_polygons("LAU_NAME", border.col = "grey", alph = 0.3) +     
  #tm_borders(alpha = 0.3) +                
  tm_text("LAU_NAME", size = 0.7) +  # 0.6
  tm_legend(show=FALSE) +
  tm_scale_bar() +
  tm_layout(main.title  = "Copenhagen City & Surroundings", main.title.size = 2, main.title.fontface = "bold") +
  tm_text("LAU_NAME", size = 0.7)  
overlay_map_inset_2

# overlay_map_inset_3
nuts3_sf_dk_subset_3 <- nuts3_sf_dk |> filter(NUTS_NAME == "Østsjælland")
LAU_subset_3 <- LAU |> filter(NUTS_NAME == "Oestsjaelland") # NUTS_NAME == "Nordsjaelland" | NUTS_NAME == "Oestsjaelland"

overlay_map_inset_3 <- tm_shape(nuts3_sf_dk_subset_3) +  
  tm_polygons("NUTS_NAME", border.col = "grey", lwd = 2) + 
  tm_legend(show=FALSE) +
  tm_shape(LAU_subset_3) +
  tm_polygons("LAU_NAME", border.col = "grey", alph = 0.3) +     
  tm_text("LAU_NAME", size = 1) + # 0.6
  tm_legend(show=FALSE) +
  tm_scale_bar() +
  tm_layout(main.title  = "East-Zealand", main.title.size = 2, main.title.fontface = "bold") 
overlay_map_inset_3

# save overlay map, with inset
tmap_save(overlay_map_inset , "./5 figs/overlay_map_inset.png")
tmap_save(overlay_map_inset_2 , "./5 figs/overlay_map_inset_2.png")
tmap_save(overlay_map_inset_3 , "./5 figs/overlay_map_inset_3.png")

overlay_map_inset_1_2_3 <- tmap_arrange(overlay_map_inset,overlay_map_inset_2,overlay_map_inset_3, nrow=1)
overlay_map_inset_1_2_3
tmap_save(overlay_map_inset_1_2_3, "./5 figs/overlay_map_inset_1_2_3.png", width = 21,
          height = 8)

# spatial and spatiotemporal sf for modeling 

# 1. spatial sf (99 kommunes)  - ensure LAU has same order as kommune_sf 
kommune_sf <- st_read("./1 data/3 raw geodata/1 DK_shp_kommune_for_model/DK_shp_kommune_for_model.shp") 
kommune_sf <- kommune_sf %>% select(-c_kom, -ID_1)

kommune_sf  # alfabetic        # no crs
LAU         # different order  # projected crs

LAU <- LAU |> mutate(LAU_NAME = as.factor(LAU_NAME))|> arrange(LAU_NAME) 
LAU <- LAU |> mutate(tibble::rowid_to_column(LAU))
LAU <- LAU |> select(id = rowid, everything()) |> select(- LAU_ID)

LAU |> pull(LAU_NAME)
kommune_sf |> pull(kommune) 

kommune_sf 
LAU        

LAU <- LAU |> rename(kommune = LAU_NAME, landsdel = NUTS_NAME)  
st_write(LAU, "./1 data/4 processed geodata/6 DK_LAU_for_model_visualisation/DK_LAU_for_model_visualisation.shp", append=FALSE) 

# 2. spatiotemp sf 
df_st <- as_tibble(1:891) %>% 
  rename(st_id = value) %>%
  mutate(year = rep(2014:2022, each = 99), 
         id = rep(1:99, times = 9))         
df_st
df_st %>% slice(98:102)

LAU_st <- left_join(df_st, LAU) %>% st_as_sf()  
LAU_st <- LAU_st %>% select(st_id, year, id, kommune, landsdel, everything()) 

LAU_st
LAU_st %>% slice(98:102)  

st_write(LAU_st, "./1 data/4 processed geodata/7 DK_LAU_st_for_model_visualisation/DK_LAU_st_for_model_visualisation.shp", append=FALSE) # append=FALSE to overwrite


