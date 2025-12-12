
 
# Add Column: Flooding in Hurricane's Path? --------------------------------------------------------

# GOAL: Create a new column where "1" indicated the flooding is within the Hurricane's path, and "0" indicates if flooding is outside of the Hurricane's path.

# To find if the flood data is within the Hurricane's path (the buffer), we used st_intersects which returns '0' (if x and y do not intersect) and '1' (if x and y intersects).


# Intersections of flooding points and hurricane path 
intersect <- st_intersects(flooding_FL_sf, ian_buffer) #sparse = FALSE) # See which rows did and didn't intersect 
len <- data.frame(lengths(intersect) > 0) # Returns TRUE (where 1) and FALSE (where 0)

# Combine dataframes 
flooding_FL_sf <- cbind(flooding_FL_sf, len) %>%  
  rename('hur_path' = `lengths.intersect....0` ) 

# Change to 1 and 0s 
flooding_FL_sf$hur_path <- as.numeric(flooding_FL_sf$hur_path)  

# Remove unnecessary objects  
rm(list = c('intersect', 'len'))
