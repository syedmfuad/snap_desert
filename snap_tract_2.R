
rm(list=ls())

library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)
library(dplyr) 

library(tidycensus)
library(tidyr)
library(sf) 
library(tidyverse)
library(maps)
library(data.table)
data(state.fips)

setwd("C:/Users/syedm/Desktop/Virtualenv")

set.seed(12345)
library(caret)
data1<-read.csv("data.csv")

data1 = data1[data1$Latitude > 0, ] 
data1 = data1[data1$Longitude < 0, ]

unique(data1$Store.Type)

data <- subset(data1, Store.Type == "Supermarket" | Store.Type == "Large Grocery Store" | Store.Type == "Super Store")

rm(data1)

library(stringr)
data$aut_date <- str_split_fixed(data$Authorization.Date, "/", 3)
data$end_date <- str_split_fixed(data$End.Date, "/", 3)

data <- subset(data, aut_date[,3] >= 1990)


#DP02_0001 = total households acs5/profile


col_income <- get_acs(geography = "tract", 
               variables = c(hh_number="DP02_0001"), 
               state=force(state.fips)$abb,
               year = 2019, 
               geometry = TRUE) 


col_income$row <- as.numeric(rownames(col_income))

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(col_income))

#coord$row <- as.numeric(st_within(coord, col_income)) 

coord$row <- sapply(st_within(coord, col_income), function(x) ifelse(length(x) > 0, as.numeric(x), NA))

new_data <- data 

new_data <- cbind(new_data, coord)
new_data$geometry <- NULL

new_data <- merge(new_data, col_income, by="row", all.x = TRUE)

new_data <- st_as_sf(new_data)

total_household <- new_data 


#B19013_001 = median household income 


col_income <- get_acs(geography = "tract", 
                      variables = c(hh_medianincome = "B19013_001"), 
                      state=force(state.fips)$abb,
                      year = 2019, 
                      geometry = TRUE) 


col_income$row <- as.numeric(rownames(col_income))

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(col_income))

#coord$row <- as.numeric(st_within(coord, col_income)) 

coord$row <- sapply(st_within(coord, col_income), function(x) ifelse(length(x) > 0, as.numeric(x), NA))

new_data <- data 

new_data <- cbind(new_data, coord)
new_data$geometry <- NULL

new_data <- merge(new_data, col_income, by="row", all.x = TRUE)

new_data <- st_as_sf(new_data)

med_income <- new_data 



#B17017_002 = household poverty 


col_income <- get_acs(geography = "tract", 
                      variables = c(hh_poverty="B17017_002"), 
                      state=force(state.fips)$abb,
                      year = 2019, 
                      geometry = TRUE) 


col_income$row <- as.numeric(rownames(col_income))

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(col_income))

#coord$row <- as.numeric(st_within(coord, col_income)) 

coord$row <- sapply(st_within(coord, col_income), function(x) ifelse(length(x) > 0, as.numeric(x), NA))

new_data <- data 

new_data <- cbind(new_data, coord)
new_data$geometry <- NULL

new_data <- merge(new_data, col_income, by="row", all.x = TRUE)

new_data <- st_as_sf(new_data)

household_poverty <- new_data




#B22010_002 = households with snap 


col_income <- get_acs(geography = "tract", 
                      variables = c(hh_snap="B22010_002"), 
                      state=force(state.fips)$abb,
                      year = 2019, 
                      geometry = TRUE) 


col_income$row <- as.numeric(rownames(col_income))

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(col_income))

#coord$row <- as.numeric(st_within(coord, col_income)) 

coord$row <- sapply(st_within(coord, col_income), function(x) ifelse(length(x) > 0, as.numeric(x), NA))

new_data <- data 

new_data <- cbind(new_data, coord)
new_data$geometry <- NULL

new_data <- merge(new_data, col_income, by="row", all.x = TRUE)

new_data <- st_as_sf(new_data)

household_snap <- new_data 




#B08201_002 = households with no vehicle 


col_income <- get_acs(geography = "tract", 
                      variables = c(hh_novehicle="B08201_002"), 
                      state=force(state.fips)$abb,
                      year = 2019, 
                      geometry = TRUE) 


col_income$row <- as.numeric(rownames(col_income))

coord <- data.frame(lat = c(data$Latitude), long = c(data$Longitude)) %>%
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(col_income))

#coord$row <- as.numeric(st_within(coord, col_income)) 

coord$row <- sapply(st_within(coord, col_income), function(x) ifelse(length(x) > 0, as.numeric(x), NA))

new_data <- data 

new_data <- cbind(new_data, coord)
new_data$geometry <- NULL

new_data <- merge(new_data, col_income, by="row", all.x = TRUE)

new_data <- st_as_sf(new_data)

household_novehicle <- new_data




#merge 

#data: total_household med_income household_poverty household_snap 

names(total_household)[names(total_household) == 'estimate'] <- 'hh_total'

names(med_income)[names(med_income) == 'estimate'] <- 'med_income'

names(household_poverty)[names(household_poverty) == 'estimate'] <- 'hh_poverty'

names(household_snap)[names(household_snap) == 'estimate'] <- 'hh_snap' 

names(household_novehicle)[names(household_novehicle) == 'estimate'] <- 'hh_novehicle' 

total_household$med_income <- med_income$med_income 

total_household$hh_poverty <- household_poverty$hh_poverty  

total_household$hh_snap <- household_snap$hh_snap 

total_household$hh_novehicle <- household_novehicle$hh_novehicle




# grouped 

total_hh <- total_household[total_household$End.Date == " ", ] 

total_hh <- select(total_hh, GEOID, hh_total, med_income, hh_poverty, hh_snap, hh_novehicle) 

total_hh <- st_drop_geometry(total_hh)

# Group by mean of multiple columns
grouped_data <- total_hh %>% group_by(GEOID) %>% 
  summarise(mean_hh_total=mean(hh_total),
            mean_med_income= mean(med_income),
            mean_hh_poverty= mean(hh_poverty),
            mean_hh_snap= mean(hh_snap),
            mean_hh_novehicle = mean(hh_novehicle), 
            count=n()) %>%
  as.data.frame()


grouped_data <- grouped_data[complete.cases(grouped_data), ]

#grouped_data$prop_snap <- 

grouped_data %>% 
  rename(
    hh_number = mean_hh_total,
    hh_medianincome = mean_med_income, 
    hh_poverty = mean_hh_poverty, 
    hh_snap = mean_hh_snap, 
    hh_novehicle = mean_hh_novehicle, 
  ) -> grouped_data






vt1 <- get_acs(geography = "tract", 
               variables = c(hh_number="DP02_0001", hh_medianincome = "B19013_001", 
                             hh_poverty="B17017_002", hh_snap="B22010_002", hh_novehicle="B08201_002"), 
               state=force(state.fips)$abb,
               year = 2019) 

vt1_dt <- as.data.table(vt1)
data_wide_vt1 <- dcast(vt1_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))




new <- merge(x = data_wide_vt1, y = grouped_data, by = "GEOID", all.x = TRUE)

new %>% 
  rename(
    hh_number = hh_number.x,
    hh_medianincome = hh_medianincome.x, 
    hh_poverty = hh_poverty.x, 
    hh_snap = hh_snap.x,
    hh_novehicle = hh_novehicle.x, 
  ) -> new

new$hh_number.y <- NULL 
new$hh_medianincome.y <- NULL 
new$hh_poverty.y <- NULL 
new$hh_snap.y <- NULL 
new$hh_novehicle.y <- NULL 

new$count[is.na(new$count)] <- 0

cor(new$hh_medianincome, new$count)

no_snap <- subset(new, count==0) 
snap <- subset(new, count>0)

summary(no_snap)
summary(snap)


#P002005 = rural population in tract
#P002002 = urban population in tract
#P002001 = tract population

dc1 <- get_decennial(geography = "tract", 
                     variables = c(rural_pop= "P002005", urban_pop = "P002002",
                                   tract_pop="P002001"), 
                     state=force(state.fips)$abb,
                     year = 2010)

dc1_dt <- as.data.table(dc1)
data_wide_dc1 <- dcast(dc1_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("value"))

data_wide_dc1$rural_perc <- (data_wide_dc1$rural_pop/data_wide_dc1$tract_pop)*100

data_wide_dc1 <- select(data_wide_dc1, GEOID, rural_perc)


new <- merge(x = new, y = data_wide_dc1, by = "GEOID", all.x = TRUE)

summary(new)

no_snap <- subset(new, count==0) 
snap <- subset(new, count>0)

summary(no_snap)
summary(snap)

#write.csv(new, file="snap_tract.csv")

new$perc_novehicle <- new$hh_novehicle/new$hh_number
new$perc_poverty <- new$hh_poverty/new$hh_number
new$perc_snap <- new$hh_snap/new$hh_number





#this part is old dont use 

atl <- get_acs(geography = "tract", 
               variables = c(hh_snap="B22002_002"), 
               state="GA", county="Fulton",
               year = 2010, geometry=TRUE)

library(sf)


# Example: Projecting to a UTM zone appropriate for Georgia, USA (e.g., UTM zone 17N)
atl_projected <- st_transform(atl, crs = 32617)  # EPSG:32617 for UTM zone 17N

# Recalculate cell_size if necessary (it might not be needed if sticking with the sqrt(500000) approach)
cell_size <- sqrt(2500000)  # This is fine as we're now in a meter-based CRS

# Create the grid using the projected data
grid <- st_make_grid(atl_projected, cellsize = c(cell_size, cell_size), square = TRUE)
grid_sf <- st_sf(geometry = grid)


# Ensure the grid is in the same CRS as `atl`
grid_sf <- st_transform(grid_sf, st_crs(atl))

# Intersect grid with `atl` to get pieces
block_grid_pieces <- st_intersection(grid_sf, atl)

# Calculate the area of each piece
block_grid_pieces$area <- st_area(block_grid_pieces)

# Calculate the area share of each piece relative to its original census tract
block_grid_pieces <- block_grid_pieces %>%
  group_by(GEOID) %>%
  mutate(total_tract_area = sum(area)) %>%
  ungroup() %>%
  mutate(area_share = area / total_tract_area)

# Calculate weighted population for each piece
block_grid_pieces$weighted_population <- block_grid_pieces$area_share * block_grid_pieces$estimate

# Aggregate these weighted populations back to the grid-cell level
grid_population <- block_grid_pieces %>%
  group_by(geometry) %>%
  summarise(total_population = sum(weighted_population, na.rm = TRUE))

# Note: If you lose the grid ID in the process, consider maintaining it through the operations or merging by geometry.

# Add an ID to each grid cell before intersection if not already done
grid_sf$id <- 1:nrow(grid_sf)

# After calculating `grid_population`, ensure you have a column to join on, such as 'id' for grid cells
# This might require adjusting the aggregation step to maintain the grid cell 'id'

# Convert `grid_population` to a standard data frame if it's an sf object
grid_population_df <- as.data.frame(grid_population)


# Merge the population data back to the grid
final_grid <- left_join(grid_sf, grid_population_df, by = "id")




















#use this part 


atl <- get_acs(geography = "tract", 
               variables = c(hh_snap="B22002_002"), 
               state="TX", county="Lubbock",
               year = 2010, geometry=TRUE)

library(sf)


# Example: Projecting to a UTM zone appropriate for Georgia, USA (e.g., UTM zone 17N)
atl_projected <- st_transform(atl, crs = 32617)  # EPSG:32617 for UTM zone 17N

# Recalculate cell_size if necessary (it might not be needed if sticking with the sqrt(500000) approach)
cell_size <- sqrt(2500000)  # This is fine as we're now in a meter-based CRS

# Create the grid using the projected data
grid <- st_make_grid(atl_projected, cellsize = c(cell_size, cell_size), square = TRUE)
grid_sf <- st_sf(geometry = grid)

# Ensure the grid is in the same CRS as `atl`
grid_sf <- st_transform(grid_sf, st_crs(atl))

# Add an ID to each grid cell before intersection if not already done
grid_sf$id <- 1:nrow(grid_sf)

# Intersect grid with `atl` to get pieces
block_grid_pieces <- st_intersection(grid_sf, atl)

# Calculate the area of each piece
block_grid_pieces$area <- st_area(block_grid_pieces)

# Calculate the area share of each piece relative to its original census tract
block_grid_pieces <- block_grid_pieces %>%
  group_by(GEOID) %>%
  mutate(total_tract_area = sum(area)) %>%
  ungroup() %>%
  mutate(area_share = area / total_tract_area)

# Calculate weighted population for each piece
block_grid_pieces$weighted_population <- block_grid_pieces$area_share * block_grid_pieces$estimate

# Aggregate these weighted populations back to the grid-cell level
grid_population <- block_grid_pieces %>%
  group_by(id) %>%
  summarise(total_population = sum(weighted_population, na.rm = TRUE))

# Note: If you lose the grid ID in the process, consider maintaining it through the operations or merging by geometry.


# After calculating `grid_population`, ensure you have a column to join on, such as 'id' for grid cells
# This might require adjusting the aggregation step to maintain the grid cell 'id'

# Convert `grid_population` to a standard data frame if it's an sf object
grid_population_df <- as.data.frame(grid_population)


# Merge the population data back to the grid
final_grid <- left_join(grid_sf, grid_population_df, by = "id")


# Convert to a data.frame, remove NAs, and convert back to sf
sf_df_object <- st_as_sf(na.omit(st_drop_geometry(final_grid)))








fulton_data <- subset(data, County=="LUBBOCK")

points_df <- select(fulton_data, Latitude, Longitude)




library(sf)

# Assuming points_df has 'latitude' and 'longitude'
points_sf <- st_as_sf(points_df, coords = c("Longitude", "Latitude"), crs = st_crs(grid_sf), agr = "constant")

# Create a 1km buffer around each point
points_sf <- st_buffer(points_sf, dist = 5000)

sf_df_object$centroid <- st_centroid(sf_df_object$geometry.y)

# Check for intersections
intersects <- st_intersects(sf_df_object$centroid, points_sf)

# Create a logical vector indicating if each centroid intersects with any buffer
sf_df_object$within_1km <- lengths(intersects) > 0

sf_df_object$within_1km <- as.integer(sf_df_object$within_1km)



# merge to get the grids by GEOID 

# Select 'id' and 'GEOID', and remove geometry
block_grid_subset <- block_grid_pieces %>%
  select(id, GEOID) %>%
  st_set_geometry(NULL)


# Merge sf_df_object with block_grid_subset based on 'id'
merged_sf <- sf_df_object %>%
  left_join(block_grid_subset, by = "id")



merged_sf <- merged_sf %>%
  distinct(id, .keep_all = TRUE)



summary(merged_sf)
summary(sf_df_object)





















library(sf)

sf_df_object$centroid <- st_centroid(sf_df_object$geometry.y)

# Assuming points_df has 'latitude' and 'longitude'
data_point <- st_as_sf(data_point, coords = c("Longitude", "Latitude"), crs = st_crs(grid_sf), agr = "constant")

# Create a 1km buffer around each point
data_point$buffer <- st_buffer(data_point$geometry, dist = 1000)

# This step is necessary if you want to perform spatial operations efficiently
sf_df_object <- st_set_geometry(sf_df_object, "centroid")

# Perform a spatial join to find which grids have centroids within any buffer
# This adds an index to each grid point indicating which buffer(s) it falls into
# If a grid does not fall within any buffer, it will not appear in the joined output
joined <- st_join(sf_df_object, data_point, join = st_within)

# To mark grids within 1km of any point with 1, and others with 0
sf_df_object$within_1km <- ifelse(!is.na(joined$id), 1, 0)

# Reset original geometry for further spatial operations if needed
sf_df_object <- st_set_geometry(sf_df_object, "geometry")




















# Convert points_df to an sf object
data_point <- st_as_sf(data_point, coords = c("Longitude", "Latitude"), crs = st_crs(grid_sf))

# Create a 1km buffer around the point
data_point$buffer <- st_buffer(data_point$geometry, dist = 10000)  # dist is in meters

# Assuming only one point and buffer for simplicity. If multiple, loop or apply function might be needed.
buffer <- data_point$buffer[1]

# Create a logical vector indicating if each centroid is within the buffer
within_buffer <- st_within(sf_df_object$centroid, buffer)

# Convert 'within_buffer' to a binary numeric vector (1 if within buffer, 0 otherwise)
sf_df_object$within_1km <- as.integer(lengths(within_buffer) > 0)
















library(sf)
library(dplyr)

# Assuming your first dataframe is named 'grid_df' and the second dataframe is named 'point_df'
# Replace 'geometry.y' with the actual name of the geometry column in grid_df
# Replace 'latitude' and 'longitude' with the names of the columns in point_df

# Step 1: Compute centroid of the grid in the first dataframe
grid_df <- sf_df_object %>%
  st_transform(., crs = 4326) %>%
  st_centroid() %>%
  st_set_crs(4326) 

# Step 2: Create a numeric matrix for the point
point_matrix <- matrix(c(data_point$Longitude, data_point$Latitude), ncol = 2)

# Step 3: Calculate the distance between each centroid and the point
distances <- st_distance(grid_df$geometry.y, st_point(point_matrix))

# Step 4: Create a new column indicating whether the centroid is within the 1km radius
grid_df$within_1km <- ifelse(distances <= 1000, 1, 0)

# Print the resulting dataframe
print(grid_df)







# Step 2: Calculate the distance between each centroid and the point in the second dataframe
distances <- st_distance(grid_df$geometry.y, st_point(cbind(data_point$Longitude, data_point$Latitude)))

# Step 3: Create a new column indicating whether the centroid is within the 1km radius
grid_df$within_1km <- ifelse(distances <= 1000, 1, 0)

# Print the resulting dataframe
print(grid_df)











