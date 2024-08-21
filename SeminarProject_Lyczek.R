#import files for analysis
library(concaveman)
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(cluster)
library(corrplot)
library(ggalluvial)
library(ggraph)
library(igraph)
library(reshape2)
library(viridis)
library(knitr)
library(kableExtra)

#set working directory
dfir<-paste("/Users/mialyczek/Downloads/Seminar")
setwd(dfir)


######## Functions ############

# Define the color palette
all_colors <- c("red", "blue", "green", "orange", "purple", "cyan", "yellow", 
                "pink", "grey", "darkgreen", "violet", "chocolate4", "black", 
                "deeppink2")

# Create a custom function to assign random colors to unique values
assign_random_colors <- function(values, colors) {
  unique_values <- unique(values)
  set.seed(123) # For reproducibility, you can remove this line for true randomness
  assigned_colors <- sample(colors, length(unique_values), replace = TRUE)
  color_mapping <- setNames(assigned_colors, unique_values)
  return(color_mapping)
}


#######################################

file<-paste("FF_20211102_1_23.txt")
orgData<-read.table(file,header = TRUE)

file<-paste("Geo_Frankfurt_Juli2021.txt")
object_data<-read.table(file,header= FALSE)


data<-data.frame(orgData)


pedestrian<-unique(data$id)
totalpax<-length(pedestrian)

#Separate passenger 1 & 2 from full data set
pax1<-data[which(data$id==pedestrian[1]),]
pax2<-data[which(data$id==pedestrian[2]),]
merged_data <- rbind(pax1, pax2) #potentially use later to test for loops in code


#Visualize the movements of passenger 1 and 2 to better understand the data points. 
pax1plot <-plot(pax1$x.m, pax1$y.m)
pax2plot <-plot(pax2$x.m, pax2$y.m)


plot_platform <- ggplot(object_data, aes(x = V1, y = V2, color = V3)) +
  geom_point(size = 4) +
  scale_color_manual(values = all_colors) +
  theme_minimal() +
  labs(title = "Train Platform", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()
print(plot_platform)

#Prepare the platform data
coor_unique <- object_data %>% distinct(V1, V2, .keep_all = TRUE)
#Check the structure of the data frame
str(coor_unique)
# Convert location to character if it's not already
coor_unique$V3 <- as.character(coor_unique$V3)

#remove any coordinates for Bahnsteig
coor_unique <-slice(coor_unique, -(0:5)) 
print(coor_unique)

#Adjust the scale of the plots so that the plots are consistent 
x_breaks <- c(-60, -40, -20, 0, 20)
y_breaks <- c(0, 4, 8, 12)

#Plot the whole Platform Data
plot_platform <- ggplot(coor_unique, aes(x = V1, y = V2, color = V3)) +
  geom_point(size = 1) +
  scale_color_manual(values = all_colors) +
  theme_minimal() +
  labs(title = "Train Platform", x = "X Coordinate", y = "Y Coordinate") +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  theme_minimal()
print(plot_platform)

# Extract rows with label 'treppe_baustelle'
treppe_baustelle_data <- coor_unique %>%
  filter(V3 == "Treppe_Baustelle")

# Print the new data frame
print(treppe_baustelle_data)


# Fit a linear regression model to distinguish the two Treppe_Baustelle groups from each other. 
lm_model <- lm(V2 ~ V1, data = treppe_baustelle_data)

# Predict the y-values using the linear regression model
treppe_baustelle_data$predicted_V2 <- predict(lm_model, newdata = treppe_baustelle_data)

# Classify points based on their position relative to the regression line
treppe_baustelle_data$cluster <- ifelse(treppe_baustelle_data$V2 > treppe_baustelle_data$predicted_V2, 1, 2)

# Print the data frame with cluster information
print(treppe_baustelle_data)

# Plot the results of clustering the Treppe_Baustelle data
plot_regression <- ggplot(treppe_baustelle_data, aes(x = V1, y = V2, color = as.factor(cluster))) +
  geom_point(size = 1) +
  geom_abline(slope = coef(lm_model)[2], intercept = coef(lm_model)[1], linetype = "dashed", color = "blue") +
  labs(title = "Classification based on Linear Regression", x = "X Coordinate", y = "Y Coordinate") +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  theme_minimal()
print(plot_regression)

# Update the label column based on the cluster assignment
treppe_baustelle_data$V3 <- ifelse(treppe_baustelle_data$cluster == 1, "Treppe_Baustelle1", "Treppe_Baustelle2")

# Selecting the relevant columns
treppe_baustelle_data<-treppe_baustelle_data[,1:3]

# copy of object_data without "Treppe_Baustelle"
object_data0<-object_data %>%
  filter(!object_data$V3 %in% "Treppe_Baustelle")

# adding to object_data0 "Treppe_Baustelle1" and "Treppe_Baustelle2" 
object_data0<-rbind(object_data0,treppe_baustelle_data)

print(object_data0)

#remove any coordinates for Bahnsteig
object_data0 <-slice(object_data0, -(0:5)) 
print(object_data0)

updated_coor_unique <- object_data0 %>% distinct(V1, V2, .keep_all = TRUE)
#Plot the whole Platform Data after preparation
plot_platform <- ggplot(object_data0, aes(x = V1, y = V2, color = V3)) +
  geom_point(size = 1) +
  scale_color_manual(values = all_colors) +
  theme_minimal() +
  labs(title = "Train Platform", x = "X Coordinate", y = "Y Coordinate") +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  theme_minimal()

print(plot_platform)


##### Define Object Areas on Platform ###### 
create_hull <- function(df, category) {
  subset <- df %>% filter(V3 == category)
  points <- as.matrix(subset[, c("V1", "V2")])
  hull <- concaveman(points)
  hull <- as.data.frame(hull)
  hull$category <- category
  return(hull)
}


# Expansion factor mapping as a named vector
expansion_factor_mapping <- c("Display" = 5, "Fahrplan" = 1.5, "Muelleimer" = 5, "Säule_1" = 1.5,
                              "Säule_2" = 1.5, "Säule_3" = 3, "Säule_4" = 3, "Snack_Automat" = 3,
                              "Treppe_Baustelle1" = 1.7, "Treppe_Baustelle2" = 1.7, "Tuer" = 4)

# Function to create and expand hull
create_and_expand_hull <- function(df, category, expansion_factor) {
  # Filter the data for the given category
  subset <- df %>% filter(V3 == category)
  points <- as.matrix(subset[, c("V1", "V2")])
  
  # Create the convex hull
  hull_indices <- chull(points)
  hull <- points[hull_indices, ]
  hull <- rbind(hull, hull[1, ])  # Close the hull by repeating the first point
  
  # Calculate the centroid of the hull
  centroid <- colMeans(hull)
  
  # Calculate the expanded hull
  expanded_hull <- t(apply(hull, 1, function(point) {
    vector <- point - centroid
    expanded_point <- centroid + vector * expansion_factor
    return(expanded_point)
  }))
  
  expanded_hull_df <- as.data.frame(expanded_hull)
  colnames(expanded_hull_df) <- c("V1", "V2")
  expanded_hull_df$category <- category
  return(expanded_hull_df)
}

# Create and expand hulls for each category using the specific expansion factors
hulls <- do.call(rbind, lapply(unique(object_data0$V3), function(category) {
  expansion_factor <- expansion_factor_mapping[category]
  create_and_expand_hull(object_data0, category, expansion_factor)
}))


# Plotting
ggplot() +
  geom_polygon(data = hulls, aes(x = V1, y = V2, group = category, fill = category), alpha = 0.2, color = "black") +
  geom_point(data = object_data0, aes(x = V1, y = V2, color = V3), size = 1) +
  geom_point(data = pax1, aes(x = x.m, y = y.m, fill = factor(id)), size = 0.4, show.legend = FALSE) +  # Adjust point size and remove from legend
  scale_fill_manual(values = all_colors) +
  scale_color_manual(values = all_colors) +
  theme_minimal() +
  labs(title = "Train Platform with Defined Areas", x = "X Coordinate", y = "Y Coordinate") +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  guides(fill = "none")  # Remove fill legend for polygons

hulls1<-data.frame(hulls) #make a copy of the hulls data frame

hulls_sf <- hulls1 %>%
  st_as_sf(coords = c("V1", "V2"), crs = 4326) %>%
  group_by(category) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

ggplot() +
  geom_sf(data = hulls_sf, aes(fill = category)) +
  theme_minimal() +
  labs(title = "Object Areas by Category", fill = "Category")


########Aggregate the Data: Based on Frames ##########

# Define the aggregation interval in seconds
aggregation_interval <- 8  # e.g., aggregate every 2 seconds

# Assign time intervals based on frames
data <- data %>%
  mutate(time_interval = floor(frame / aggregation_interval))

# Aggregate data
# Here we take the mean of x,y, and h coordinates for each pedestrian in each time interval
aggregated_df <- data %>%
  group_by(id, time_interval) %>%
  summarize(
    x.avg = mean(x.m, na.rm = TRUE),
    y.avg = mean(y.m, na.rm = TRUE),
    h.avg = mean(h.m, na.rm = TRUE),
    .groups = 'drop'
  )
#set up the time difference column for later based on number of seconds used in aggregated data
aggregated_df$time_diff <- 2

#######Calculate Pedestrian Speeds #######

#Calculate the speed the pedestrians are moving and derive the amount of time they spend in different areas of the platform. 

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Function to calculate speed for a pedestrian
calculate_speed <- function(df) {
  # Calculate distance between consecutive coordinates
  df$distance <- c(0, euclidean_distance(df$x.avg[-1], df$y.avg[-1], df$x.avg[-nrow(df)], df$y.avg[-nrow(df)]))
  # Calculate speed (m/s)
  df$speed <- df$distance / 2 #since each interval is now representative of 2 seconds
  return(df)
}

# Calculate speed for each pedestrian using aggregated data
pedestrian_speed <- lapply(unique(aggregated_df$id), function(ped_id) {
  calculate_speed(aggregated_df[aggregated_df$id == ped_id, ])
})

# Combine speed data for all pedestrians to main data frame
aggregated_df <- do.call(rbind, pedestrian_speed)



#######Calculate Pedestrian Locations #######
# Define the function to check if a pedestrian is standing in a hull area
determine_pedestrian_location <- function(pedestrian_df, hulls_df) {
  # Create sf objects from dataframes
  hulls_sf <- hulls1 %>%
    st_as_sf(coords = c("V1", "V2"), crs = 4326) %>%
    group_by(category) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  pedestrian_sf <- st_as_sf(pedestrian_df, coords = c("x.avg", "y.avg"), crs = 4326)
  
  # Check if each pedestrian point is within any of the hull polygons
  inside_hull <- st_intersects(pedestrian_sf, hulls_sf, sparse = FALSE)
  
  # Create a location column based on the result
  pedestrian_df$location <- apply(inside_hull, 1, function(row) {
    if (any(row)) {
      hull_category <- hulls_sf$category[which(row)]
      return(hull_category[1])  # Assuming a pedestrian can't be in more than one hull at a time
    } else {
      return("platform")
    }
  })
  return(pedestrian_df)
}

#drop unnecessary columns for location function calculation
min_df <- aggregated_df %>%
  select(-speed, -h.avg, -distance, -time_diff)


# Determine pedestrian locations
locations_df <- determine_pedestrian_location(min_df, hulls)

# Merge the location results into main data frame
aggregated_df <- aggregated_df %>%
  left_join(locations_df %>% select(id, time_interval, location), by = c("id", "time_interval"))



#######Calculate Pedestrian Cumulative Time Spent in Areas #######
# Define a function to calculate cumulative time spent by a pedestrian in locations
calculate_cumulative_time <- function(dt) {
  # Ensure the data.table is sorted by pedestrian id and time
  setorder(dt, id, time_interval)
  
  # Initialize cumulative time column
  dt[, cumulative_time := 0]
  
  # Calculate cumulative time within each group of id and location
  dt[, cumulative_time := cumsum(time_diff), by = .(id, location)]
  
  return(dt)
}

#Begin implementation of the cumulative_time function by preparing to run the code in chunks
# Convert data.frame to data.table
aggregated_dt <- as.data.table(aggregated_df)

# Get unique pedestrian IDs
unique_ids <- unique(aggregated_dt$id)

# Define the chunk size
chunk_size <- 1000

# Initialize an empty list to store results
results_list <- list()

# Process data in chunks
for (i in seq(1, length(unique_ids), by = chunk_size)) {
  # Get the current chunk of IDs
  id_chunk <- unique_ids[i:min(i + chunk_size - 1, length(unique_ids))]
  
  # Subset the data for the current chunk
  chunk_dt <- aggregated_dt[id %in% id_chunk]
  
  # Apply the cumulative time calculation
  chunk_result <- calculate_cumulative_time(chunk_dt)
  
  # Append the result to the list
  results_list[[length(results_list) + 1]] <- chunk_result
}

# Combine all chunks back into a single data.table
final_result <- rbindlist(results_list)

aggregated_df <- as.data.frame(final_result)


#######Calculate Pedestrian Total time spent in Areas #######

# Function to calculate total time spent in each location
calculate_total_time_spent <- function(df) {
  # Ensure the dataframe is sorted by id and frame
  df <- df %>%
    arrange(id, time_interval)
  
  # Initialize columns for cumulative time and group ID
  df$cumulative_time <- 0
  df$group_id <- 0
  
  # Initialize the first group_id
  group_id <- 1
  
  # Loop through the dataframe row by row
  for (i in 1:nrow(df)) {
    if (i == 1) {
      df$group_id[i] <- group_id
      df$cumulative_time[i] <- df$time_diff[i]
    } else {
      if (df$id[i] == df$id[i - 1] && df$location[i] == df$location[i - 1]) {
        # Same pedestrian and same location, continue the group
        df$group_id[i] <- group_id
        df$cumulative_time[i] <- df$cumulative_time[i - 1] + df$time_diff[i]
      } else {
        # Different pedestrian or different location, start a new group
        group_id <- group_id + 1
        df$group_id[i] <- group_id
        df$cumulative_time[i] <- df$time_diff[i]
      }
    }
  }
  
  # Calculate total time spent in each group
  df <- df %>%
    group_by(id, group_id) %>% # was previously location
    mutate(time_spent = sum(time_diff)) %>%
    ungroup()
  
  # Drop the group_id column as no longer needed
  df <- df %>%
    select(-group_id)
  
  return(df)
}

#Begin implementation of the total_time_spent function by preparing to run the code in chunks
# Convert data.frame to data.table
aggregated_dt <- as.data.table(aggregated_df)

# Define the chunk size
chunk_size <- 1000

# Initialize an empty list to store results
results_list <- list()

# Process data in chunks
for (i in seq(1, length(unique_ids), by = chunk_size)) {
  # Get the current chunk of IDs
  id_chunk <- unique_ids[i:min(i + chunk_size - 1, length(unique_ids))]
  
  # Subset the data for the current chunk
  chunk_dt <- aggregated_dt[id %in% id_chunk]
  
  # Apply the cumulative time calculation
  chunk_result <- as.data.frame(chunk_dt)
  chunk_result <- calculate_total_time_spent(chunk_result)
  
  # Append the result to the list
  results_list[[length(results_list) + 1]] <- chunk_result
}

# Combine all chunks back into a single data frame
aggregated_df <- rbindlist(results_list)
aggregated_df <- as.data.frame(aggregated_df)


####### First and Last Frames ########

# Define hull names
hull_names <- c("Treppe_Baustelle1,Treppe_Baustelle2, Säule_1, Säule_2, Säule_3, Säule_4,Fahrplan, Display, Muelleimer, Snack_Automat, Tuer")

# Function to determine first_frame and last_frame
determine_frames <- function(df) {
  df <- df %>%
    group_by(id) %>%
    arrange(time_interval) %>%
    mutate(
      location_change = location != lag(location, default = first(location)),
      group = cumsum(location_change)
    ) %>%
    group_by(id, location, group) %>%
    mutate(
      first_frame = min(time_interval),
      last_frame = max(time_interval)
    ) %>%
    ungroup() %>%
    select(-location_change, -group)
  
  return(df)
}

# Apply the frame function to the dataframe
aggregated_df <- determine_frames(aggregated_df)


########### Function Trajectory #########

# Function to classify trajectories for a single chunk
classify_trajectories_chunk <- function(df) {
  setDT(df)  # Convert to data.table for efficiency
  
  # Initialize trajectory column with "platform"
  df[, trajectory := "platform"]
  
  # Define object hulls
  object_hulls <- c('Treppe_Baustelle1', 'Treppe_Baustelle2', 'Säule_1', 'Säule_2', 'Säule_3', 'Säule_4', 'Fahrplan', 'Display', 'Muelleimer', 'Snack_Automat', 'Tuer')
  
  # Loop through unique pedestrian IDs
  unique_ids <- unique(df$id)
  for (pid in unique_ids) {
    # Filter data for the current pedestrian ID
    ped_data <- df[id == pid]
    
    # Loop through rows to classify trajectories
    for (i in 1:nrow(ped_data)) {
      if (ped_data$location[i] %in% object_hulls) {
        # If the location is a hull, set trajectory for the hull duration
        df[id == pid & time_interval >= ped_data$first_frame[i] & time_interval <= ped_data$last_frame[i], trajectory := ped_data$location[i]]
      } else if (ped_data$location[i] == "platform") {
        # If the location is platform, look at the next hull
        next_hull <- ped_data[location %in% object_hulls & time_interval > ped_data$time_interval[i], location][1]
        if (!is.na(next_hull)) {
          df[id == pid & time_interval >= ped_data$first_frame[i] & time_interval <= ped_data$last_frame[i], trajectory := paste("on platform moving towards", next_hull)]
        }
      }
    }
  }
  
  return(df)
}

# Function to process the data frame in chunks
classify_trajectories_in_chunks <- function(df, chunk_size = 1000) {
  setDT(df)  # Convert to data.table for efficiency
  unique_ids <- unique(df$id)
  results <- list()
  
  # Process each chunk separately
  for (i in seq(1, length(unique_ids), by = chunk_size)) {
    id_chunk <- unique_ids[i:min(i + chunk_size - 1, length(unique_ids))]
    chunk_data <- df[id %in% id_chunk]
    classified_chunk <- classify_trajectories_chunk(chunk_data)
    results[[length(results) + 1]] <- classified_chunk
  }
  
  # Combine all chunks into a single data frame
  result_df <- rbindlist(results)
  return(result_df)
}

# Applying the function to classify trajectories in chunks
aggregated_df <- classify_trajectories_in_chunks(aggregated_df, chunk_size = 1000)

# Save the final result to csv
write.csv(aggregated_df, "aggregated_df.csv", row.names=FALSE)



###### Data Analysis ##########
aggregated_df <- read.csv("aggregated_df.csv", header = TRUE)

####### Encode location and trajectory columns######

# Ensure 'location' and 'trajectory' are treated as factors
aggregated_df <- aggregated_df %>%
  mutate(location = factor(location)) %>%
  mutate(trajectory = factor(trajectory))


# One-hot encoding for 'location'
location_encoded <- model.matrix(~ location - 1, data = aggregated_df) %>%
  as.data.frame() %>%
  rename_all(~ gsub("location", "loc", .)) 

# One-hot encoding for 'location'
trajectory_encoded <- model.matrix(~ trajectory - 1, data = aggregated_df) %>%
  as.data.frame() %>%
  rename_all(~ gsub("trajectory", "traj", .))

# Combine the encoded columns with the original data, dropping 'location' and 'trajectory'
combined_df <- aggregated_df %>%
  select(-location, -trajectory) %>%
  bind_cols(location_encoded, trajectory_encoded)


####### Clustering Pedestrian Movements: Speed & Coordinates ########
#apply K-means clustering to see if there are patterns to pedestrian movements

# Select predictors for clustering
data_for_clustering <- aggregated_df %>%
  select(x.avg, y.avg, speed)

# Standardize the data (excluding 'id' column)
data_scaled <- data_for_clustering %>%
  #select(-id) %>%
  scale()

# Determine the optimal number of clusters using the Elbow Method
wss <- (nrow(data_scaled) - 1) * sum(apply(data_scaled, 2, var))
for (i in 2:30) {
  wss[i] <- sum(kmeans(data_scaled, centers = i, iter.max = 2000, nstart = 25)$withinss)
}

# Plot the Elbow plot
plot(1:30, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = "Elbow Method for Optimal Clusters: Speed & Coordinates")

# Assuming we choose # clusters from the elbow plot
kmeans_result <- kmeans(data_scaled, centers = 25, nstart = 25)

# Add cluster results to the sampled data
aggregated_df$Cluster1 <- kmeans_result$cluster

# Ensure 'cluster' is a factor for better visualization
aggregated_df$Cluster1 <- as.factor(aggregated_df$Cluster1)

# Function to calculate mode (most frequent value)
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Group by 'id' and determine the most common cluster assignment for each pedestrian
aggregated_df <- aggregated_df %>%
  group_by(id) %>%
  mutate(MostCommonCluster = mode_function(Cluster1)) %>%
  ungroup()

# Update the 'Cluster1' column with the most common cluster assignment for each pedestrian
aggregated_df$Cluster1 <- aggregated_df$MostCommonCluster

# Visualize the clusters using the sampled data
ggplot(aggregated_df, aes(x = x.avg, y = y.avg, color = Cluster1)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks)+
  labs(title = "K-means Clustering of Pedestrian Movements & Speed", color = "Cluster1")

# Cluster characteristics calculations
cluster_summary1 <- aggregated_df %>%
  group_by(Cluster1) %>%
  summarise(
    count = n(),
    avg_x = mean(x.avg),
    avg_y = mean(y.avg),
    avg_speed = mean(speed),
    avg_distance = mean(distance),
    mode_location = mode_function(location)
  )

# Visualize cluster characteristics
ggplot(cluster_summary1, aes(x = Cluster1, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Pedestrians in Clusters")

#Re-calculate WCSS
# Calculate centroids for each cluster and store in a new dataframe
wss_df <- aggregated_df %>%
  group_by(Cluster1) %>%
  summarise(
    centroid_x = mean(x.avg),
    centroid_y = mean(y.avg),
    centroid_speed = mean(speed)
  )

# Merge centroids back with the original data
merged_df <- aggregated_df %>%
  left_join(wss_df, by = "Cluster1")

# Calculate the squared Euclidean distance to the centroid
merged_df <- merged_df %>%
  mutate(
    distance_to_centroid = (x.avg - centroid_x)^2 + (y.avg - centroid_y)^2 + (speed - centroid_speed)^2
  )

# Calculate the total within-cluster sum of squares (WSS)
total_wss <- sum(merged_df$distance_to_centroid)

# Calculate the number of clusters
num_clusters <- length(unique(merged_df$Cluster1))

# Calculate the average WSS by dividing the total WSS by the number of clusters
average_wss1 <- total_wss / num_clusters

# Print the average WSS
average_wss1
#[1] 6014481

####### Clustering pedestrian movements: Trajectory Encoded########
#apply K-means clustering to see if there are patterns to pedestrian movements
colnames(trajectory_encoded)

# Select predictors for clustering
data_for_clustering <- combined_df %>%
  select(trajDisplay,trajFahrplan, trajMuelleimer, "trajon platform moving towards Display" ,"trajon platform moving towards Fahrplan", "trajon platform moving towards Muelleimer", "trajon platform moving towards Säule_1", "trajon platform moving towards Säule_2","trajon platform moving towards Säule_3", "trajon platform moving towards Säule_4","trajon platform moving towards Snack_Automat", "trajon platform moving towards Treppe_Baustelle1","trajon platform moving towards Treppe_Baustelle2","trajon platform moving towards Tuer",trajplatform, trajSäule_1, trajSäule_2, trajSäule_3, trajSäule_4, trajSnack_Automat, trajTreppe_Baustelle1, trajTreppe_Baustelle2, trajTuer)


# Determine the optimal number of clusters using the Elbow Method
wss <- (nrow(data_for_clustering) - 1) * sum(apply(data_for_clustering, 2, var))
for (i in 2:25) {
  wss[i] <- sum(kmeans(data_for_clustering, centers = i, iter.max = 1000, nstart = 25)$withinss)
}

# Plot the Elbow plot
plot(1:25, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = "Elbow Method for Optimal Clusters: Trajectories")

# Assuming we choose # clusters from the elbow plot
kmeans_result <- kmeans(data_for_clustering, centers = 20, nstart = 25)

# Add cluster results to the sampled data
aggregated_df$Cluster2 <- kmeans_result$cluster

# Ensure 'cluster' is a factor for better visualization
aggregated_df$Cluster2 <- as.factor(aggregated_df$Cluster2)

# Group by 'id' and determine the most common cluster assignment for each pedestrian
aggregated_df <- aggregated_df %>%
  group_by(id) %>%
  mutate(MostCommonCluster = mode_function(Cluster2)) %>%
  ungroup()

# Update the 'Cluster2' column with the most common cluster assignment for each pedestrian
aggregated_df$Cluster2 <- aggregated_df$MostCommonCluster


# Visualize the clusters using the sampled data
ggplot(aggregated_df, aes(x = x.avg, y = y.avg, color = Cluster2)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks)+
  labs(title = "K-means Clustering of\nPedestrian Movements, Time Spent, & Trajectory", color = "Cluster2")


# Cluster characteristics calculations
cluster_summary2 <- aggregated_df %>%
  group_by(Cluster2) %>%
  summarise(
    count = n(),
    avg_x = mean(x.avg),
    avg_y = mean(y.avg),
    avg_speed = mean(speed),
    avg_distance = mean(distance),
    mode_location = mode_function(location)
  )


# Visualize cluster characteristics
ggplot(cluster_summary2, aes(x = Cluster2, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Pedestrians in Clusters")

#Re-calculate WCSS
# Calculate centroids for each cluster and store in a new dataframe
merged_df2 <- combined_df
merged_df2 <- merged_df2 %>%
  left_join(aggregated_df %>% select(id, time_interval, Cluster2), by = c("id", "time_interval"))

wss_df2 <- merged_df2 %>%
  group_by(Cluster2) %>%
  summarise(
    centroid_trajDisplay = mean(trajDisplay),
    centroid_trajFahrplan = mean(trajFahrplan), 
    centroid_trajMuelleimer = mean(trajMuelleimer), 
    centroid_trajon_platform_moving_towards_Display = mean(`trajon platform moving towards Display`),
    centroid_trajon_platform_moving_towards_Fahrplan = mean(`trajon platform moving towards Fahrplan`),
    centroid_trajon_platform_moving_towards_Muelleimer = mean(`trajon platform moving towards Muelleimer`),
    centroid_trajon_platform_moving_towards_Säule1 = mean(`trajon platform moving towards Säule_1`),
    centroid_trajon_platform_moving_towards_Säule2 = mean(`trajon platform moving towards Säule_2`),
    centroid_trajon_platform_moving_towards_Säule3 = mean(`trajon platform moving towards Säule_3`),
    centroid_trajon_platform_moving_towards_Säule4 = mean(`trajon platform moving towards Säule_4`),
    centroid_trajon_platform_moving_towards_Snack_Automat = mean(`trajon platform moving towards Snack_Automat`),
    centroid_trajon_platform_moving_towards_Treppe_Baustelle1 = mean(`trajon platform moving towards Treppe_Baustelle1`), 
    centroid_trajon_platform_moving_towards_Treppe_Baustelle2 = mean(`trajon platform moving towards Treppe_Baustelle2`),
    centroid_trajon_platform_moving_towards_Tuer = mean(`trajon platform moving towards Tuer`),
    centroid_trajplatform = mean(trajplatform), 
    centroid_trajSäule1 = mean(trajSäule_1), 
    centroid_trajSäule2 = mean(trajSäule_2),
    centroid_trajSäule3 = mean(trajSäule_3), 
    centroid_trajSäule4 = mean(trajSäule_4), 
    centroid_trajSnack_Automat = mean(trajSnack_Automat),
    centroid_trajTreppe_Baustelle1 = mean(trajTreppe_Baustelle1),
    centroid_trajTreppe_Baustelle2 = mean(trajTreppe_Baustelle2), 
    centroid_trajTuer = mean(trajTuer)
  )

# Merge centroids back with the original data
merged_df2 <- merged_df2 %>%
  left_join(wss_df2, by = "Cluster2")

# Calculate the squared Euclidean distance to the centroid
merged_df2 <- merged_df2 %>%
  mutate(
    distance_to_centroid = 
      (trajDisplay - centroid_trajDisplay)^2 + (trajFahrplan - centroid_trajFahrplan)^2 + 
      (trajMuelleimer - centroid_trajMuelleimer)^2 + (`trajon platform moving towards Display` - centroid_trajon_platform_moving_towards_Display)^2 +
      (`trajon platform moving towards Fahrplan` - centroid_trajon_platform_moving_towards_Fahrplan)^2 +
      (`trajon platform moving towards Muelleimer` - centroid_trajon_platform_moving_towards_Muelleimer)^2 +
      (`trajon platform moving towards Säule_1` - centroid_trajon_platform_moving_towards_Säule1)^2 +
      (`trajon platform moving towards Säule_2` - centroid_trajon_platform_moving_towards_Säule2)^2 +
      (`trajon platform moving towards Säule_3` - centroid_trajon_platform_moving_towards_Säule3)^2 +
      (`trajon platform moving towards Säule_4` - centroid_trajon_platform_moving_towards_Säule4)^2 +
      (`trajon platform moving towards Snack_Automat` - centroid_trajon_platform_moving_towards_Snack_Automat)^2 +
      (`trajon platform moving towards Treppe_Baustelle1` - centroid_trajon_platform_moving_towards_Treppe_Baustelle1)^2 +
      (`trajon platform moving towards Treppe_Baustelle2` - centroid_trajon_platform_moving_towards_Treppe_Baustelle2)^2 +
      (`trajon platform moving towards Tuer` - centroid_trajon_platform_moving_towards_Tuer)^2 +
      (trajplatform - centroid_trajplatform)^2 +
      (trajSäule_1 - centroid_trajSäule1)^2 +
      (trajSäule_2 - centroid_trajSäule2)^2 +
      (trajSäule_3 - centroid_trajSäule3)^2 +
      (trajSäule_4 - centroid_trajSäule4)^2 +
      (trajSnack_Automat - centroid_trajSnack_Automat)^2 +
      (trajTreppe_Baustelle1 - centroid_trajTreppe_Baustelle1)^2 +
      (trajTreppe_Baustelle2 - centroid_trajTreppe_Baustelle2)^2 +
      (trajTuer - centroid_trajTuer)^2
  )

# Calculate the total within-cluster sum of squares (WSS)
total_wss2 <- sum(merged_df2$distance_to_centroid)

# Calculate the number of clusters
num_clusters <- length(unique(merged_df2$Cluster2))

# Calculate the average WSS by dividing the total WSS by the number of clusters
average_wss2 <- total_wss2 / num_clusters

# Print the average WSS
average_wss2
#[1] 30280.97

####### Clustering with encoded location variables & time_spent########

# Apply one-hot encoding
# One-hot encoding of 'location' column
encoded_df1 <- aggregated_df %>%
  select(id, time_interval, location) %>%
  mutate(location = factor(location)) %>%
  cbind(., model.matrix(~ location - 1, data = .))

# Combine the encoded data with the original data, dropping the original 'location' column
combined_df1 <- aggregated_df %>%
  select(-location) %>%
  left_join(encoded_df1, by = c("id", "time_interval"))


# Select predictors for clustering
data_for_clustering <- combined_df1 %>%
  select(time_spent, locationDisplay, locationFahrplan, locationMuelleimer,locationplatform, locationSäule_1, locationSäule_2,locationSäule_3, locationSäule_4, locationSnack_Automat, locationTreppe_Baustelle1,locationTreppe_Baustelle2, locationTuer )

# Standardize the data
data_scaled <- data_for_clustering %>%
  scale()

# Determine the optimal number of clusters using the Elbow Method
wss <- (nrow(data_scaled) - 1) * sum(apply(data_scaled, 2, var))
for (i in 2:30) {
  wss[i] <- sum(kmeans(data_scaled, centers = i, iter.max = 2000, nstart = 25)$withinss)
}

# Plot the Elbow plot
plot(1:30, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = "Elbow Method for Optimal Clusters\nTime Spent & Location")

# Assuming we choose # clusters from the elbow plot
kmeans_result <- kmeans(data_scaled, centers = 23, nstart = 25)

# Add cluster results to the sampled data
aggregated_df$Cluster3 <- kmeans_result$cluster

# Ensure 'cluster' is a factor for better visualization
aggregated_df$Cluster3 <- as.factor(aggregated_df$Cluster3)

# Group by 'id' and determine the most common cluster assignment for each pedestrian
aggregated_df <- aggregated_df %>%
  group_by(id) %>%
  mutate(MostCommonCluster = mode_function(Cluster3)) %>%
  ungroup()

# Update the 'Cluster3' column with the most common cluster assignment for each pedestrian
aggregated_df$Cluster3 <- aggregated_df$MostCommonCluster

# Visualize the clusters using the sampled data
ggplot(aggregated_df, aes(x = x.avg, y = y.avg, color = Cluster3)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  labs(title = "K-means Clustering of Location & Time Spent", color = "Cluster3")

#Cluster characteristics calculations
cluster_summary3 <- aggregated_df %>%
  group_by(Cluster3) %>%
  summarise(
    count = n(),
    avg_x = mean(x.avg),
    avg_y = mean(y.avg),
    avg_speed = mean(speed),
    avg_distance = mean(distance),
    mode_location = mode_function(location)
  )

# Visualize cluster characteristics
ggplot(cluster_summary3, aes(x = Cluster3, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Pedestrians in Clusters")

#Re-calculate WCSS
# Calculate centroids for each cluster and store in a new dataframe
merged_df3 <- combined_df1
merged_df3 <- merged_df3 %>%
  left_join(aggregated_df %>% select(id, time_interval, Cluster3), by = c("id", "time_interval"))

wss_df3 <- merged_df3 %>%
  group_by(Cluster3) %>%
  summarise(
    centroid_time_spent = mean(time_spent),
    centroid_locationDisplay = mean(locationDisplay),
    centroid_locationFahrplan = mean(locationFahrplan), 
    centroid_locationMuelleimer = mean(locationMuelleimer), 
    centroid_locationplatform = mean(locationplatform), 
    centroid_locationSäule1 = mean(locationSäule_1), 
    centroid_locationSäule2 = mean(locationSäule_2),
    centroid_locationSäule3 = mean(locationSäule_3), 
    centroid_locationSäule4 = mean(locationSäule_4), 
    centroid_locationSnack_Automat = mean(locationSnack_Automat),
    centroid_locationTreppe_Baustelle1 = mean(locationTreppe_Baustelle1),
    centroid_locationTreppe_Baustelle2 = mean(locationTreppe_Baustelle2), 
    centroid_locationTuer = mean(locationTuer)
  )

# Merge centroids back with the original data
merged_df3 <- merged_df3 %>%
  left_join(wss_df3, by = "Cluster3")

# Calculate the squared Euclidean distance to the centroid
merged_df3 <- merged_df3 %>%
  mutate(
    distance_to_centroid =(time_spent - centroid_time_spent)^2 +
      (locationDisplay - centroid_locationDisplay)^2 + 
      (locationFahrplan - centroid_locationFahrplan)^2 + 
      (locationMuelleimer - centroid_locationMuelleimer)^2 +
      (locationplatform - centroid_locationplatform)^2 +
      (locationSäule_1 - centroid_locationSäule1)^2 +
      (locationSäule_2 - centroid_locationSäule2)^2 +
      (locationSäule_3 - centroid_locationSäule3)^2 +
      (locationSäule_4 - centroid_locationSäule4)^2 +
      (locationSnack_Automat - centroid_locationSnack_Automat)^2 +
      (locationTreppe_Baustelle1 - centroid_locationTreppe_Baustelle1)^2 +
      (locationTreppe_Baustelle2 - centroid_locationTreppe_Baustelle2)^2 +
      (locationTuer - centroid_locationTuer)^2
  )

# Calculate the total within-cluster sum of squares (WSS)
total_wss3 <- sum(merged_df3$distance_to_centroid)

# Calculate the number of clusters
num_clusters <- length(unique(merged_df3$Cluster3))

# Calculate the average WSS by dividing the total WSS by the number of clusters
average_wss3 <- total_wss3 / num_clusters

# Print the average WSS
average_wss3
#[1] 1924997224

####### Clustering with encoded location variables, time_spent, and speed ########

# Combine the encoded data with the original data, dropping the original 'location' column
combined_df1 <- aggregated_df %>%
  select(-location) %>%
  left_join(encoded_df1, by = c("id", "time_interval"))

# Select predictors for clustering
data_for_clustering <- combined_df1 %>%
  select(time_spent, speed, locationDisplay, locationFahrplan, locationMuelleimer,locationplatform, locationSäule_1, locationSäule_2,locationSäule_3, locationSäule_4, locationSnack_Automat, locationTreppe_Baustelle1,locationTreppe_Baustelle2, locationTuer)


# Standardize the data
data_scaled <- data_for_clustering %>%
  scale()

# Determine the optimal number of clusters using the Elbow Method
wss <- (nrow(data_scaled) - 1) * sum(apply(data_scaled, 2, var))
for (i in 2:20) {
  wss[i] <- sum(kmeans(data_scaled, centers = i, iter.max = 1000, nstart = 25)$withinss)
}

# Plot the Elbow plot
plot(1:20, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = "Elbow Method for Optimal Clusters Encoded 
Locations, Speed, & Time_spent")

# Assuming we choose # clusters from the elbow plot
kmeans_result <- kmeans(data_scaled, centers = 20, nstart = 25)

# Add cluster results to the sampled data
aggregated_df$Cluster4 <- kmeans_result$cluster

# Ensure 'cluster' is a factor for better visualization
aggregated_df$Cluster4 <- as.factor(aggregated_df$Cluster4)

# Group by 'id' and determine the most common cluster assignment for each pedestrian
aggregated_df <- aggregated_df %>%
  group_by(id) %>%
  mutate(MostCommonCluster = mode_function(Cluster4)) %>%
  ungroup()

# Update the 'Cluster4' column with the most common cluster assignment for each pedestrian
aggregated_df$Cluster4 <- aggregated_df$MostCommonCluster

# Visualize the clusters using the sampled data
ggplot(aggregated_df, aes(x = x.avg, y = y.avg, color = Cluster4)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  labs(title = "K-means Clustering of\nLocation, Time Spent & Speed", color = "Cluster4")

# Cluster characteristics calculation
cluster_summary4 <- aggregated_df %>%
  group_by(Cluster4) %>%
  summarise(
    count = n(),
    avg_x = mean(x.avg),
    avg_y = mean(y.avg),
    avg_speed = mean(speed),
    avg_distance = mean(distance),
    mode_location = mode_function(location)
  )

# Visualize cluster characteristics
ggplot(cluster_summary4, aes(x = Cluster4, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Pedestrians in Clusters")

#Re-calculate WCSS
# Step 1: Calculate centroids for each cluster and store in a new dataframe
merged_df4 <- combined_df1
merged_df4 <- merged_df4 %>%
  left_join(aggregated_df %>% select(id, time_interval, Cluster4), by = c("id", "time_interval"))

wss_df4 <- merged_df4 %>%
  group_by(Cluster4) %>%
  summarise(
    centroid_time_spent = mean(time_spent),
    centroid_speed = mean(speed),
    centroid_locationDisplay = mean(locationDisplay),
    centroid_locationFahrplan = mean(locationFahrplan), 
    centroid_locationMuelleimer = mean(locationMuelleimer), 
    centroid_locationplatform = mean(locationplatform), 
    centroid_locationSäule1 = mean(locationSäule_1), 
    centroid_locationSäule2 = mean(locationSäule_2),
    centroid_locationSäule3 = mean(locationSäule_3), 
    centroid_locationSäule4 = mean(locationSäule_4), 
    centroid_locationSnack_Automat = mean(locationSnack_Automat),
    centroid_locationTreppe_Baustelle1 = mean(locationTreppe_Baustelle1),
    centroid_locationTreppe_Baustelle2 = mean(locationTreppe_Baustelle2), 
    centroid_locationTuer = mean(locationTuer)
  )

# Merge centroids back with the original data
merged_df4 <- merged_df4 %>%
  left_join(wss_df4, by = "Cluster4")

# Calculate the squared Euclidean distance to the centroid
merged_df4 <- merged_df4 %>%
  mutate(
    distance_to_centroid =(time_spent - centroid_time_spent)^2 +
      (speed - centroid_speed)^2 +
      (locationDisplay - centroid_locationDisplay)^2 + 
      (locationFahrplan - centroid_locationFahrplan)^2 + 
      (locationMuelleimer - centroid_locationMuelleimer)^2 +
      (locationplatform - centroid_locationplatform)^2 +
      (locationSäule_1 - centroid_locationSäule1)^2 +
      (locationSäule_2 - centroid_locationSäule2)^2 +
      (locationSäule_3 - centroid_locationSäule3)^2 +
      (locationSäule_4 - centroid_locationSäule4)^2 +
      (locationSnack_Automat - centroid_locationSnack_Automat)^2 +
      (locationTreppe_Baustelle1 - centroid_locationTreppe_Baustelle1)^2 +
      (locationTreppe_Baustelle2 - centroid_locationTreppe_Baustelle2)^2 +
      (locationTuer - centroid_locationTuer)^2
  )

# Calculate the total within-cluster sum of squares (WSS)
total_wss4 <- sum(merged_df4$distance_to_centroid)

# Calculate the number of clusters
num_clusters <- length(unique(merged_df4$Cluster4))

# Calculate the average WSS by dividing the total WSS by the number of clusters
average_wss4 <- total_wss4 / num_clusters

# Print the average WSS
average_wss4
#[1] 4019900015

######## Clustering Using Location and Trajectory########

# Select predictors for clustering
data_for_clustering <- combined_df %>%
  select(locDisplay, locFahrplan, locMuelleimer,locplatform, locSäule_1, locSäule_2,locSäule_3, locSäule_4, locSnack_Automat, locTreppe_Baustelle1,locTreppe_Baustelle2, locTuer, trajDisplay, trajFahrplan, trajMuelleimer, `trajon platform moving towards Display`, `trajon platform moving towards Fahrplan`, `trajon platform moving towards Muelleimer`, `trajon platform moving towards Säule_1`, `trajon platform moving towards Säule_2`, `trajon platform moving towards Säule_3`, `trajon platform moving towards Säule_4`, `trajon platform moving towards Snack_Automat`, `trajon platform moving towards Treppe_Baustelle1`, `trajon platform moving towards Treppe_Baustelle2`, `trajon platform moving towards Tuer`)


# Standardize the data
data_scaled <- data_for_clustering %>%
  scale()

# Determine the optimal number of clusters using the Elbow Method
wss <- (nrow(data_scaled) - 1) * sum(apply(data_scaled, 2, var))
for (i in 2:23) {
  wss[i] <- sum(kmeans(data_scaled, centers = i, iter.max = 1000, nstart = 25)$withinss)
}

# Plot the Elbow plot
plot(1:23, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = "Elbow Method for Optimal Clusters Encoded 
Locations & Trajectories")

# Assuming we choose # clusters from the elbow plot
kmeans_result <- kmeans(data_scaled, centers = 23, nstart = 25) #errors when k>23

# Add cluster results to the data
aggregated_df$Cluster5 <- kmeans_result$cluster

# Ensure 'cluster' is a factor for better visualization
aggregated_df$Cluster5 <- as.factor(aggregated_df$Cluster5)

# Group by 'id' and determine the most common cluster assignment for each pedestrian
aggregated_df <- aggregated_df %>%
  group_by(id) %>%
  mutate(MostCommonCluster = mode_function(Cluster5)) %>%
  ungroup()

# Update the 'Cluster5' column with the most common cluster assignment for each pedestrian
aggregated_df$Cluster5 <- aggregated_df$MostCommonCluster

# Visualize the clusters using the data
ggplot(aggregated_df, aes(x = x.avg, y = y.avg, color = Cluster5)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  coord_equal(xlim = c(-65, 20)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  labs(title = "K-means Clustering of Location & Trajectory", color = "Cluster5")

# Cluster characteristics calculations
cluster_summary5 <- aggregated_df %>%
  group_by(Cluster5) %>%
  summarise(
    count = n(),
    avg_x = mean(x.avg),
    avg_y = mean(y.avg),
    avg_speed = mean(speed),
    avg_distance = mean(distance),
    mode_location = mode_function(location)
  )

# Visualize cluster characteristics
ggplot(cluster_summary5, aes(x = Cluster5, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Pedestrians in Clusters")

#Re-calculate WCSS
# Calculate centroids for each cluster and store in a new dataframe
merged_df5 <- combined_df
merged_df5 <- merged_df5 %>%
  left_join(aggregated_df %>% select(id, time_interval, Cluster5), by = c("id", "time_interval"))

wss_df5 <- merged_df5 %>%
  group_by(Cluster5) %>%
  summarise(
    centroid_locationDisplay = mean(locDisplay),
    centroid_locationFahrplan = mean(locFahrplan), 
    centroid_locationMuelleimer = mean(locMuelleimer), 
    centroid_locationplatform = mean(locplatform), 
    centroid_locationSäule1 = mean(locSäule_1), 
    centroid_locationSäule2 = mean(locSäule_2),
    centroid_locationSäule3 = mean(locSäule_3), 
    centroid_locationSäule4 = mean(locSäule_4), 
    centroid_locationSnack_Automat = mean(locSnack_Automat),
    centroid_locationTreppe_Baustelle1 = mean(locTreppe_Baustelle1),
    centroid_locationTreppe_Baustelle2 = mean(locTreppe_Baustelle2), 
    centroid_locationTuer = mean(locTuer),
    centroid_trajDisplay = mean(trajDisplay),
    centroid_trajFahrplan = mean(trajFahrplan), 
    centroid_trajMuelleimer = mean(trajMuelleimer), 
    centroid_trajon_platform_moving_towards_Display = mean(`trajon platform moving towards Display`),
    centroid_trajon_platform_moving_towards_Fahrplan = mean(`trajon platform moving towards Fahrplan`),
    centroid_trajon_platform_moving_towards_Muelleimer = mean(`trajon platform moving towards Muelleimer`),
    centroid_trajon_platform_moving_towards_Säule1 = mean(`trajon platform moving towards Säule_1`),
    centroid_trajon_platform_moving_towards_Säule2 = mean(`trajon platform moving towards Säule_2`),
    centroid_trajon_platform_moving_towards_Säule3 = mean(`trajon platform moving towards Säule_3`),
    centroid_trajon_platform_moving_towards_Säule4 = mean(`trajon platform moving towards Säule_4`),
    centroid_trajon_platform_moving_towards_Snack_Automat = mean(`trajon platform moving towards Snack_Automat`),
    centroid_trajon_platform_moving_towards_Treppe_Baustelle1 = mean(`trajon platform moving towards Treppe_Baustelle1`), 
    centroid_trajon_platform_moving_towards_Treppe_Baustelle2 = mean(`trajon platform moving towards Treppe_Baustelle2`),
    centroid_trajon_platform_moving_towards_Tuer = mean(`trajon platform moving towards Tuer`),
    centroid_trajplatform = mean(trajplatform), 
    centroid_trajSäule1 = mean(trajSäule_1), 
    centroid_trajSäule2 = mean(trajSäule_2),
    centroid_trajSäule3 = mean(trajSäule_3), 
    centroid_trajSäule4 = mean(trajSäule_4), 
    centroid_trajSnack_Automat = mean(trajSnack_Automat),
    centroid_trajTreppe_Baustelle1 = mean(trajTreppe_Baustelle1),
    centroid_trajTreppe_Baustelle2 = mean(trajTreppe_Baustelle2), 
    centroid_trajTuer = mean(trajTuer)
  )

# Merge centroids back with the original data
merged_df5 <- merged_df5 %>%
  left_join(wss_df5, by = "Cluster5")

# Calculate the squared Euclidean distance to the centroid
merged_df5 <- merged_df5 %>%
  mutate(
    distance_to_centroid = (locDisplay - centroid_locationDisplay)^2 + 
      (locFahrplan - centroid_locationFahrplan)^2 + 
      (locMuelleimer - centroid_locationMuelleimer)^2 +
      (locplatform - centroid_locationplatform)^2 +
      (locSäule_1 - centroid_locationSäule1)^2 +
      (locSäule_2 - centroid_locationSäule2)^2 +
      (locSäule_3 - centroid_locationSäule3)^2 +
      (locSäule_4 - centroid_locationSäule4)^2 +
      (locSnack_Automat - centroid_locationSnack_Automat)^2 +
      (locTreppe_Baustelle1 - centroid_locationTreppe_Baustelle1)^2 +
      (locTreppe_Baustelle2 - centroid_locationTreppe_Baustelle2)^2 +
      (locTuer - centroid_locationTuer)^2+
      (trajDisplay - centroid_trajDisplay)^2 + (trajFahrplan - centroid_trajFahrplan)^2 + 
      (trajMuelleimer - centroid_trajMuelleimer)^2 + (`trajon platform moving towards Display` - centroid_trajon_platform_moving_towards_Display)^2 +
      (`trajon platform moving towards Fahrplan` - centroid_trajon_platform_moving_towards_Fahrplan)^2 +
      (`trajon platform moving towards Muelleimer` - centroid_trajon_platform_moving_towards_Muelleimer)^2 +
      (`trajon platform moving towards Säule_1` - centroid_trajon_platform_moving_towards_Säule1)^2 +
      (`trajon platform moving towards Säule_2` - centroid_trajon_platform_moving_towards_Säule2)^2 +
      (`trajon platform moving towards Säule_3` - centroid_trajon_platform_moving_towards_Säule3)^2 +
      (`trajon platform moving towards Säule_4` - centroid_trajon_platform_moving_towards_Säule4)^2 +
      (`trajon platform moving towards Snack_Automat` - centroid_trajon_platform_moving_towards_Snack_Automat)^2 +
      (`trajon platform moving towards Treppe_Baustelle1` - centroid_trajon_platform_moving_towards_Treppe_Baustelle1)^2 +
      (`trajon platform moving towards Treppe_Baustelle2` - centroid_trajon_platform_moving_towards_Treppe_Baustelle2)^2 +
      (`trajon platform moving towards Tuer` - centroid_trajon_platform_moving_towards_Tuer)^2 +
      (trajplatform - centroid_trajplatform)^2 +
      (trajSäule_1 - centroid_trajSäule1)^2 +
      (trajSäule_2 - centroid_trajSäule2)^2 +
      (trajSäule_3 - centroid_trajSäule3)^2 +
      (trajSäule_4 - centroid_trajSäule4)^2 +
      (trajSnack_Automat - centroid_trajSnack_Automat)^2 +
      (trajTreppe_Baustelle1 - centroid_trajTreppe_Baustelle1)^2 +
      (trajTreppe_Baustelle2 - centroid_trajTreppe_Baustelle2)^2 +
      (trajTuer - centroid_trajTuer)^2
  )

# Calculate the total within-cluster sum of squares (WSS)
total_wss5 <- sum(merged_df5$distance_to_centroid)

# Calculate the number of clusters
num_clusters <- length(unique(merged_df5$Cluster5))

# Calculate the average WSS by dividing the total WSS by the number of clusters
average_wss5 <- total_wss5 / num_clusters

# Print the average WSS
average_wss5
#[1] 44241.68

########Clustering for Location Visit - Frequencies ########

#prepare the data frame to include each id and the visitation frequency for each location
# Create a dataframe with unique visits to each location by each pedestrian
unique_visits1 <- aggregated_df %>%
  arrange(id, time_interval) %>%
  group_by(id, location) %>%
  summarize(unique_visits = n_distinct(first_frame), .groups = 'drop')

# Pivot the data to have one row per 'id' and columns for each location
visits_df <- unique_visits1 %>%
  pivot_wider(names_from = location, values_from = unique_visits, values_fill = 0)

# Prepare the data
# Remove the 'id' column before clustering
clustering_data <- visits_df %>%
  select(c(-id))

# Function to calculate mode
calculate_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# For example, if 'visits_df' contains an ID column, exclude it
visits_numeric_df <- clustering_data %>% select_if(is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(clustering_data, use = "complete.obs", method = "pearson")

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle")

# Determine the number of clusters (K)
# Use the Elbow method 
set.seed(2)  # For reproducibility
wss <- (nrow(clustering_data)-1)*sum(apply(clustering_data,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(clustering_data, centers=i)$tot.withinss)
plot(1: 30, wss, type="b", xlab="Number of Clusters (K)", ylab="Within groups sum of squares", main = "Elbow Method for Optimal Clusters:\nLocation Visit Frequencies")

set.seed(2)
# Perform K-means clustering
kmeans_result <- kmeans(clustering_data, centers=30)

# Access the Within-Cluster Sum of Squares (WSS)
wss <- kmeans_result$tot.withinss
print(wss)
#[1] 46497.91

# Access within-cluster sum of squares for each cluster
cluster_wss <- kmeans_result$withinss
print("Within-cluster WSS for each cluster:")
print(cluster_wss)
#[1] "Within-cluster WSS for each cluster:"
#[1]   581.4667  2614.6031   524.5714   367.7279   654.5714  1787.2133   753.5556  1552.4375 17141.5083
#[10]   308.2273  4746.3096   286.4876   608.6452   838.7460  1162.4505   784.1000   195.1667  1640.9277
#[19]  1034.8499   432.0437   899.0283  1216.5848  1424.4034   328.7500   364.1818   665.2500   891.9357
#[28]   677.2133   600.8309  1414.1255

# Analyze and visualize the results
# Add the cluster assignments back to the original data
visits_df$cluster <- kmeans_result$cluster

# Assuming visits_df has columns 'id' and 'cluster'
# Perform a left join to add cluster assignments to the original data frame
aggregated_df <- aggregated_df %>%
  left_join(visits_df %>% select(id, cluster), by = "id") %>%
  rename(Cluster = cluster)  # Rename if you prefer the column to be named 'Cluster'

visits_df_summary <- visits_df %>%
  count(cluster) %>%
  rename(count = n)  # Rename 'n' to 'count' for clarity

ggplot(visits_df_summary, aes(x = factor(cluster), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Pedestrians in Clusters",
       x = "Cluster",
       y = "Number of Pedestrians") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the mean, standard deviation, and median visit counts for each location across clusters
cluster_summary <- visits_df %>%
  group_by(cluster) %>%
  summarise(
    across(!id, list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = TRUE),
      median = \(x) median(x, na.rm = TRUE)
    ))
  )

# Selecting columns for mean
mean_data <- cluster_summary %>%
  select(cluster, ends_with("_mean"))

# Renaming columns to remove "_mean"
colnames(mean_data) <- gsub("_mean", "", colnames(mean_data))

# Gathering data into long format for ggplot
mean_data_long <- mean_data %>%
  pivot_longer(cols = -cluster, names_to = "location", values_to = "mean")

# Plotting
ggplot(mean_data_long, aes(x = location, y = mean, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Visit Frequencies by Location and Cluster",
       x = "Location", y = "Mean Visit Frequency", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Selecting columns for standard deviation
sd_data <- cluster_summary %>%
  select(cluster, ends_with("_sd"))

# Renaming columns to remove "_sd"
colnames(sd_data) <- gsub("_sd", "", colnames(sd_data))

# Gathering data into long format for ggplot
sd_data_long <- sd_data %>%
  pivot_longer(cols = -cluster, names_to = "location", values_to = "sd")

# Plotting
ggplot(sd_data_long, aes(x = location, y = sd, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standard Deviation of Visit Frequencies by Location and Cluster",
       x = "Location", y = "Standard Deviation of Visit Frequency", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Selecting columns for median
median_data <- cluster_summary %>%
  select(cluster, ends_with("_median"))

# Renaming columns to remove "_median"
colnames(median_data) <- gsub("_median", "", colnames(median_data))

# Gathering data into long format for ggplot
median_data_long <- median_data %>%
  pivot_longer(cols = -cluster, names_to = "location", values_to = "median")

# Plotting
ggplot(median_data_long, aes(x = location, y = median, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Median Visit Frequencies by Location and Cluster",
       x = "Location", y = "Median Visit Frequency", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify location columns based on their names
location_columns <- names(visits_df)[!names(visits_df) %in% c("id", "cluster")]

# Calculate summary statistics for each location
summary_stats_clusters <- visits_df %>%
  group_by(cluster) %>%
  summarize(across(all_of(location_columns), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE)
  )))

# Calculate summary statistics
summary_stats <- clustering_data %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
    mode = ~calculate_mode(.x)
  )))


######## Clustering Location Visits Binary #########
# Transform the data
# Convert location visit counts to binary (1 if visited, 0 if not)
binary_visits_df <- visits_df %>%
  mutate(across(-id, ~ ifelse(. > 0, 1, 0)))  # Convert all columns except 'id' to binary

# Remove the 'id' column before clustering
clustering_data <- binary_visits_df %>%
  select(-id, -cluster)

# Calculate summary statistics
summary_stats_binary <- clustering_data %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    mode = ~calculate_mode(.x)
  )))

# For example, if 'visits_df' contains an ID column, exclude it
visits_numeric_df <- clustering_data %>% select_if(is.numeric)

# Step 2: Calculate the correlation matrix
correlation_matrix <- cor(clustering_data, use = "complete.obs", method = "pearson")

# Step 3: Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle")

# Step 2: Determine the number of clusters (K)
# Again, use the Elbow method or another method to choose K
set.seed(2)
wss <- (nrow(clustering_data)-1)*sum(apply(clustering_data[,-1],2,var))
for (i in 2:20) wss[i] <- sum(kmeans(clustering_data[,-1], centers=i)$tot.withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters (K)", ylab="Within groups sum of squares", main = "Elbow Method for Optimal Clusters:\nLocation Visits - Binary")

# Step 3: Perform K-means clustering
set.seed(2)
kmeans_result <- kmeans(clustering_data[,-1], centers=17)

# Access the Within-Cluster Sum of Squares (WSS)
wss <- kmeans_result$tot.withinss
print(wss)
#[1] 15715.38

# Access within-cluster sum of squares for each cluster
cluster_wss <- kmeans_result$withinss
print("Within-cluster WSS for each cluster:")
print(cluster_wss)
#[1]    79.98802   413.50167   150.72107  1504.69431   381.56554   102.28346   176.86815    71.12621   929.55128
#[10]    17.51923    77.38679    61.62903 11275.21917    82.39035   200.06568   138.29808    52.57534

# Add the cluster assignments back to the original data
binary_visits_df$cluster <- kmeans_result$cluster

# Extract the relevant columns from binary_visits_df
cluster_assignments <- binary_visits_df %>%
  select(id, cluster)

# Merge the cluster assignments into the aggregated_df by 'pedestrian_id'
aggregated_df <- aggregated_df %>%
  left_join(cluster_assignments, by = "id")


binary_visits_df_summary <- binary_visits_df %>%
  count(cluster) %>%
  rename(count = n)  # Rename 'n' to 'count' for clarity

ggplot(binary_visits_df_summary, aes(x = factor(cluster), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Pedestrians in Clusters",
       x = "Cluster",
       y = "Number of Pedestrians") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Define the columns you want to summarize
location_columns <- c("platform", "Säule_1", "Säule_2", "Säule_3", "Säule_4", "Fahrplan", "Tuer","Display", "Muelleimer", "Snack_Automat", "Treppe_Baustelle1", "Treppe_Baustelle2")

# Calculate the average number of visits by cluster for each location
average_visits_by_cluster <- binary_visits_df %>%
  group_by(cluster) %>%
  summarise(across(all_of(location_columns), \(x) mean(x, na.rm = TRUE)))

# Convert data to long format for easier plotting
average_visits_long <- average_visits_by_cluster %>%
  pivot_longer(cols = -cluster, names_to = "location", values_to = "average_visits")

# Create a bar plot
ggplot(average_visits_long, aes(x = factor(cluster), y = average_visits, fill = location)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Visits by Cluster and Location",
       x = "Cluster",
       y = "Average Visits") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the mode value by cluster for each location
mode_visits_by_cluster <- binary_visits_df %>%
  group_by(cluster) %>%
  summarise(across(all_of(location_columns), calculate_mode))

# Convert mode data to long format for easier plotting
mode_visits_long <- mode_visits_by_cluster %>%
  pivot_longer(cols = -cluster, names_to = "location", values_to = "mode_value")

# Create a heatmap
ggplot(mode_visits_long, aes(x = location, y = factor(cluster), fill = factor(mode_value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkblue"), name = "Mode Value") +
  labs(title = "Mode of Visits by Cluster and Location",
       x = "Location",
       y = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Convert to long format 
binary_visits_long <- binary_visits_df %>%
  select(-id) %>%
  pivot_longer(cols = -cluster, names_to = "location", values_to = "visit")


# Fit logistic regression models for each location
set.seed(1)
logistic_models <- binary_visits_long %>%
  group_by(location) %>%
  do(model = glm(visit ~ factor(cluster), data = ., family = binomial))

# Check the summary of one model (for example, the first one)
summary(logistic_models$model[[2]])


#export the model into a file to include in seminar paper
install.packages("knitr")
install.packages("kableExtra")

# Create a data frame of your model summary
model_df <- as.data.frame(summary(logistic_models$model[[1]])$coefficients)

# Create the LaTeX table
kable(model_df, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "striped", full_width = FALSE) %>%
  save_kable("logistic_model.tex")

#visualize the results using log odds 
# Create a list to store odds ratios for each location
odds_ratios_list <- list()

# Loop through each location's logistic model
for (i in 1:nrow(logistic_models)) {
  model <- logistic_models$model[[i]]
  
  # Get the tidy coefficients from the model
  coefficients <- broom::tidy(model)
  
  # Calculate odds ratios
  coefficients$odds_ratio <- exp(coefficients$estimate)
  
  # Add location information
  coefficients$location <- logistic_models$location[i]
  
  # Store the result
  odds_ratios_list[[i]] <- coefficients
}

# Combine the results into a single data frame
odds_ratios_df <- do.call(rbind, odds_ratios_list)

# Filter for significant results
significant_odds_ratios <- odds_ratios_df %>%
  filter(p.value < 0.05)

# Forest plot of odds ratios with confidence intervals
ggplot(significant_odds_ratios, aes(x = location, y = odds_ratio, color = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = exp(estimate - 1.96 * std.error), ymax = exp(estimate + 1.96 * std.error)), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Forest Plot of Odds Ratios by Location", x = "Location", y = "Odds Ratio") +
  theme_minimal()

# The Odds ratio for cluster 11 and Tuer are really high. To verify results investigate the data to verify such results
# Extract data for verification
high_log_odds_cluster <- odds_ratios_df %>%
  filter(term == "factor(cluster)11", location == "Tuer")

print(high_log_odds_cluster)

# View data points for cluster 11 and location "Tuer"
high_log_odds_data <- binary_visits_df %>%
  filter(cluster == 11, get("Tuer") == 1)

summary(high_log_odds_data)


######### Visualizations: Trajectory Plotting By Cluster#########
install.packages("viridis")

#### The best performing clustering combination of predictors was that for Iteration 5 using Location 
#and trajectory.
#The visualizations obtained below use the data and clustering assignment for this clustering ##### 

#plot the individual trajectories of each cluster's pedestrian movements
plot_trajectories <- function(cluster_num, df) {
  cluster_data <- df %>% filter(cluster == cluster_num)
  
  plot <- ggplot() +
    geom_sf(data = hulls_sf, aes(fill = category), color = "black") +  # Plot sf object (polygons)
    geom_path(data = cluster_data, aes(x = x.avg, y = y.avg, group = id, color = as.factor(id)), alpha = 0.7) +
    scale_fill_viridis(discrete = TRUE) +  # Use a viridis color palette for fill
    scale_color_viridis(discrete = TRUE) +  # Use a viridis color palette for lines
    labs(title = paste("Trajectories for Cluster", cluster_num),
         x = "X Coordinate",
         y = "Y Coordinate",
         color = "Pedestrian ID") +
    theme_minimal() +
    theme(legend.position = "none") +  # Remove legend for clarity
    coord_sf(lims_method = "geometry_bbox")  # Adjust coordinate limits
  print(plot)
}

# Get unique clusters
clusters <- unique(aggregated_df$cluster)

# Plot trajectories for each cluster
for (cluster_num in clusters) {
  plot <- plot_trajectories(cluster_num, aggregated_df)
  print(plot)
}

#The trajectories for each cluster are difficult to summarize when we plot all of their movements cluster by cluster. 
#Calculate the average movements for each cluster and plot the most common trajectories by cluster. 
# Load required libraries

merged_df5 <- merged_df5 %>%
  left_join(aggregated_df %>% select(id, time_interval, location, trajectory), by = c("id", "time_interval"))


# Step 1: Calculate transition frequencies excluding self-transitions
transitions <- merged_df5 %>%
  arrange(id, time_interval) %>%
  group_by(id) %>%
  mutate(next_location = lead(location)) %>%
  filter(!is.na(next_location) & next_location != location) %>%
  group_by(Cluster5, location, next_location) %>%
  summarize(transition_count = n(), .groups = 'drop')

# Step 2: Identify top movements (optional: adjust `n` as needed)
top_movements <- transitions %>%
  group_by(Cluster5) %>%
  top_n(n = 5, wt = transition_count)  # Adjust `n` to select top movements

# Step 3: Aggregate coordinates for top movements from `aggregated_df`
#Arrange data by pedestrian_id and time_interval
merged_df5 <- merged_df5 %>%
  arrange(id, time_interval)

#Calculate next_location using lead function
merged_df5 <- merged_df5 %>%
  group_by(id) %>%
  mutate(next_location = lead(location))

#Filter aggregated_df to include only top movements
top_movements_data <- top_movements %>%
  left_join(merged_df5, by = c("Cluster5", "location", "next_location")) %>%
  select(Cluster5, location, next_location, x.avg, y.avg)

#Group and summarize to calculate average coordinates
top_movements_coords <- top_movements_data %>%
  group_by(Cluster5, location, next_location) %>%
  summarise(
    avg_x = mean(x.avg, na.rm = TRUE),
    avg_y = mean(y.avg, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 4: Plot aggregated trajectories for each cluster
plot <- ggplot() +
  geom_sf(data = hulls_sf, aes(fill = "Hulls"),fill = "grey", color = "black", show.legend = FALSE) +  # Plot sf object (polygons) with a consistent fill color
  geom_path(data = top_movements_coords, aes(x = avg_x, y = avg_y, color = as.factor(Cluster5)), alpha = 0.9) +
  geom_segment(data = top_movements_coords, aes(x = avg_x, y = avg_y, xend = lead(avg_x), yend = lead(avg_y), color = as.factor(Cluster5)), 
               arrow = arrow(length = unit(0.1, "inches")), alpha = 0.6) +  # Add arrows for transitions
  scale_color_viridis(discrete = TRUE, name = "Cluster") +  # Use a viridis color palette for lines
  labs(title = "Trajectories for Individual Clusters",
       x = "X Coordinate",
       y = "Y Coordinate") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Adjust legend position
  coord_sf(lims_method = "geometry_bbox")  # Adjust coordinate limits

print(plot)

########Heat Mapping Pedestrian Movements#######

install.packages("reshape2")
    
# Define number of bins (adjust as needed)
num_bins <- 100

# Bin x.avg and y.avg into grid cells
merged_df5$xbins <- cut(merged_df5$x.avg, breaks = num_bins, labels = FALSE)
merged_df5$ybins <- cut(merged_df5$y.avg, breaks = num_bins, labels = FALSE)

# Aggregate data by bins
cluster_data <- merged_df5 %>%
  group_by(Cluster5, xbins, ybins) %>%
  summarize(total_time_spent = sum(time_spent), .groups = 'drop')

# Ensure total_time_spent is numeric
cluster_data$total_time_spent <- as.numeric(cluster_data$total_time_spent)

# Adjust color scale based on the range of total_time_spent
max_time_spent <- max(cluster_data$total_time_spent, na.rm = TRUE)

# Create the heat map
heatmap <- ggplot(cluster_data, aes(x = xbins, y = ybins, fill = total_time_spent)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, max_time_spent)) +
  labs(title = "Heatmap of time spent in Areas on Platform", x = "X Bins", y = "Y Bins", fill = "Time Spent") +
  theme_minimal() +
  coord_fixed()+  # Need to see if better way to visualize the platform shape!!!!

# Print the heatmap
print(heatmap)


#########Density Plots By Cluster############

#Plot the time spent density by location for clusters
# Compute average coordinates and adjusted total time spent
merged_dense_df <- aggregated_df %>%
  group_by(id, location, cluster) %>%
  arrange(time_interval) %>%
  distinct(first_frame, .keep_all = TRUE) %>%  # Keep only the first occurrence for each distinct first_frame within each group
  group_by(location, cluster) %>%  # Re-group to summarize over all ids in each location 
  summarize(
    x.avg = mean(x.avg, na.rm = TRUE), 
    y.avg = mean(y.avg, na.rm = TRUE), 
    mean_time_spent = mean(time_spent, na.rm = TRUE),  # average time spent based on all first occurrences
    .groups = 'drop'
  )

# Unique clusters
Clusters <- unique(merged_dense_df$cluster)

#rename cluster column for applying for-loop
# Rename a column
merged_dense_df <- merged_dense_df %>%
  rename(Cluster = cluster)

for (cluster in Clusters) {
  # Filter data for the current cluster
  cluster_data <- merged_dense_df %>%
    filter(Cluster == cluster)
  
  if (nrow(cluster_data) > 0) {
    # Create the density plot for the current cluster
    density_plot <- ggplot() +
      geom_polygon(data = hulls, aes(x = V1, y = V2, group = category, fill = category), alpha = 0.2, color = "black") +
      geom_point(data = cluster_data, aes(x = x.avg, y = y.avg, size = mean_time_spent, fill = "Average Time Spent"), alpha = 0.6, shape = 21) +
      scale_fill_manual(values = c("Average Time Spent" = "red"), guide = "none") +  # Adjust fill scale for categorical data
      labs(title = paste("Density Plot by Location for Cluster", cluster), 
           x = "X Coordinate", y = "Y Coordinate", fill = "Density", size = "Time Spent (sec)") +
      theme_minimal() +
      coord_equal(xlim = c(-65, 20)) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(breaks = y_breaks) 
    
    print(density_plot)
    ggsave(paste("density_plot_loc_cluster_", cluster, "meansec.png", sep = ""), plot = density_plot, bg = "white")
  }
}

# Adjust the data to calculate time spent in minutes
merged_dense_df <- merged_dense_df %>%
  mutate(mean_time_spent_min = mean_time_spent / 60)  # Convert seconds to minutes

# Unique clusters
clusters <- unique(aggregated_df$Cluster)

for (cluster in clusters) {
  # Filter data for the current cluster
  cluster_data <- merged_dense_df %>%
    filter(Cluster == cluster)
  
  if (nrow(cluster_data) > 0) {
    # Create the density plot for the current cluster
    density_plot <- ggplot() +
      geom_polygon(data = hulls, aes(x = V1, y = V2, group = category, fill = category), alpha = 0.2, color = "black") +
      geom_point(data = cluster_data, aes(x = x.avg, y = y.avg, size = mean_time_spent_min, fill = "Average Time Spent"), alpha = 0.6, shape = 21) +
      scale_fill_manual(values = c("Average Time Spent" = "red"), guide = "none") +  # Adjust fill scale for categorical data
      labs(title = paste("Density Plot by Location for Cluster", cluster), 
           x = "X Coordinate", y = "Y Coordinate", fill = "Density",
           size = "Time Spent (min)") +  # Adding the unit "(min)" to the legend
      theme_minimal() +
      coord_equal(xlim = c(-65, 20)) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(breaks = y_breaks) 
    
    print(density_plot)
    ggsave(paste("density_plot_loc_traj_cluster_", cluster, "_meanmin.png", sep = ""), plot = density_plot, bg = "white")
  }
}

#the density plot for cluster 5 is skewed and includes a lot of time spent in a location 
#not visited by the majority of the cluster
# Filter the data for cluster = 5 and location = 'Säule_3', then get unique IDs and count them
unique_ids_ <- aggregated_df %>%
  filter(cluster == 5, location == "Säule_3") %>% 
  distinct(id) %>% 
  nrow()

# Print the number of unique IDs
print(unique_ids_)

#did these 5 ids visit the other mode locations of cluster 5?
# Step 1: Filter the data for cluster = 5 and location = 'Säule_3' to get relevant IDs
filtered_ids <- aggregated_df %>%
  filter(cluster == 5, location == "Säule_3") %>%
  distinct(id) %>% # Replace 'id' with your actual ID column name
  pull(id) # Extract the IDs as a vector

# Step 2: Filter the full data set for rows with these IDs
filtered_data <- aggregated_df %>%
  filter(id %in% filtered_ids)

# Step 3: Check if these IDs are present in multiple locations
ids_multiple_locations <- filtered_data %>%
  group_by(id) %>%
  summarize(num_locations = n_distinct(location)) %>%
  filter(num_locations > 1) # IDs with more than 1 distinct location

# Step 4: Print or view the results
print(ids_multiple_locations)


#the density plot for cluster 10 is skewed and includes a lot of time spent in a location 
#not visited by the majority of the cluster
# Filter the data for cluster = 10 and location = 'Säule_3', 'Säule_4', & 'Display' then get unique IDs and count them
unique_ids_ <- aggregated_df %>%
  filter(cluster == 10, location == "Display") %>% 
  distinct(id) %>% 
  nrow()

# Print the number of unique IDs
print(unique_ids_)

#did these 5 ids visit the other mode locations of cluster 5?
# Step 1: Filter the data for cluster = 5 and location = 'Säule_3' to get relevant IDs
filtered_ids <- aggregated_df %>%
  filter(cluster == 5, location == "Säule_3") %>%
  distinct(id) %>% # Replace 'id' with your actual ID column name
  pull(id) # Extract the IDs as a vector

# Step 2: Filter the full data set for rows with these IDs
filtered_data <- aggregated_df %>%
  filter(id %in% filtered_ids)

# Step 3: Check if these IDs are present in multiple locations
ids_multiple_locations <- filtered_data %>%
  group_by(id) %>%
  summarize(num_locations = n_distinct(location)) %>%
  filter(num_locations > 1) # IDs with more than 1 distinct location

# Step 4: Print or view the results
print(ids_multiple_locations)
########Data Metrics ################
# Load necessary libraries

# Let's create some summary metrics to use in visualizations 

# 1. Common Waiting Positions:
# For each cluster, identify the most common waiting positions based on x and y coordinates
common_waiting_positions <- aggregated_df %>%
  group_by(cluster, location) %>%
  summarize(mean_x = mean(x.avg, na.rm = TRUE), mean_y = mean(y.avg, na.rm = TRUE), 
            median_x = median(x.avg, na.rm = TRUE), median_y = median(y.avg, na.rm = TRUE), 
            .groups = 'drop')

# Create a plot for common waiting positions
plot_common_waiting_positions <- function(data) {
  ggplot(data, aes(x = mean_x, y = mean_y)) +
    geom_polygon(data = hulls, aes(x = V1, y = V2, group = category, fill = category), alpha = 0.2, color = "black") +
    geom_point(aes(color = "Mean Position"), size = 1) +  # Mean positions
    geom_point(aes(x = median_x, y = median_y, color = "Median Position"), size = 1, shape = 17) +  # Median positions
    facet_wrap(~cluster) +  # One plot per cluster
    labs(title = "Common Waiting Positions by Cluster",
         x = "X Coordinate",
         y = "Y Coordinate",
         color = "Position Type") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    coord_equal(xlim = c(-65, 20)) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    scale_color_manual(values = c("Mean Position" = "blue", "Median Position" = "red"))
}

# Apply the function to the common waiting positions data
common_waiting_positions_plot <- plot_common_waiting_positions(common_waiting_positions)

# Display the plot
print(common_waiting_positions_plot)


# 2. Movement Patterns:
# Describe typical movement patterns including frequent transitions between locations
# Calculate transition frequencies excluding self-transitions
transitions <- aggregated_df %>%
  arrange(id, time_interval) %>%
  group_by(id) %>%
  mutate(next_location = lead(location)) %>%
  filter(!is.na(next_location) & next_location != location) %>%
  group_by(cluster, location, next_location) %>%
  summarize(transition_count = n(), .groups = 'drop')

install.packages("ggraph")
# Create a network graph for each cluster
create_network_plot <- function(cluster_data, cluster_id) {
  # Create an edge list for the transitions
  edges <- cluster_data %>%
    select(location, next_location, transition_count)
  
  # Create a graph object from the edge list
  graph <- graph_from_data_frame(edges, directed = TRUE)
  
  # Plot the network
  plot <- ggraph(graph, layout = 'fr') +
    geom_edge_link(aes(width = transition_count), alpha = 0.8, color = 'blue') +
    geom_node_point(size = 5, color = 'red') +
    geom_node_text(aes(label = name), vjust = 1.5, hjust = 0.5) +
    labs(title = paste("Location Transitions for Cluster", cluster_id),
         x = "X Coordinate", y = "Y Coordinate", fill = "Transition Count") +
    theme_minimal()
  
  return(plot)
}

# Apply the function and save plots
clusters <- unique(transitions$cluster)

for (Cluster in clusters) {
  cluster_data <- transitions %>% filter(cluster == Cluster)
  if (nrow(cluster_data) > 0) {
    network_plot <- create_network_plot(cluster_data, Cluster)
    print(network_plot)
    ggsave(paste("network_plot_cluster_", cluster, ".png", sep = ""), plot = network_plot, bg = "white")
  }
}

# Create a heatmap plot for each cluster
create_heatmap_plot <- function(cluster_data, cluster_id) {
  # Plot the heatmap
  plot <- ggplot(cluster_data, aes(x = location, y = next_location, fill = transition_count)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = paste("Location Transitions Heatmap for Cluster", cluster_id),
         x = "From Location", y = "To Location", fill = "Transition Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot)
}

# Apply the function and save plots
for (Cluster in clusters) {
  cluster_data <- transitions %>% filter(cluster == Cluster)
  if (nrow(cluster_data) > 0) {
    heatmap_plot <- create_heatmap_plot(cluster_data, Cluster)
    print(heatmap_plot)
    ggsave(paste("heatmap_plot_cluster_", cluster, ".png", sep = ""), plot = heatmap_plot, bg = "white")
  }
}

# Calculate average speed, distance, and time spent in each cluster
# Part 1: Calculate correct time spent based on first frame in each location
time_spent_summary <- aggregated_df %>%
  group_by(cluster, id, location) %>%
  filter(time_interval == min(time_interval)) %>%  # Keep only the first frame for each location per pedestrian
  ungroup() %>%
  group_by(cluster, location) %>%
  summarize(
    avg_time_spent = mean(time_spent, na.rm = TRUE),
    total_time_spent = sum(time_spent, na.rm = TRUE),
    .groups = 'drop'
  )

# Part 2: Calculate average speed and total distance using all data
speed_distance_summary <- aggregated_df %>%
  group_by(cluster) %>%
  summarize(
    avg_speed = mean(speed, na.rm = TRUE),
    total_distance = sum(distance, na.rm = TRUE),
    avg_distance = mean(distance, na.rm = TRUE),
    .groups = 'drop'
  )


# If cumulative_time_spent is not already in your dataframe, calculate it
if (!"cumulative_time_spent" %in% colnames(aggregated_df)) {
  aggregated_df <- aggregated_df %>%
    group_by(id, location) %>%
    mutate(cumulative_time_spent = cumsum(time_interval)) %>%
    ungroup()
}

# Create a dataframe with unique visits to each location by each pedestrian
unique_visits <- aggregated_df %>%
  group_by(id, location, cluster) %>%
  summarize(
    avg_speed = mean(speed, na.rm = TRUE),
    total_distance = sum(distance, na.rm = TRUE),
    time_spent = first(time_spent), # Assuming time_spent is total for the visit
    .groups = 'drop'
  )

# 3. Cluster Summary:
# Summarizes average speed, total distance, average time spent, and total time spent for each cluster
cluster_summary <- unique_visits %>%
  group_by(cluster) %>%
  summarize(
    avg_speed = mean(avg_speed, na.rm = TRUE),
    total_distance = sum(total_distance, na.rm = TRUE),
    .groups = 'drop'
  )

# 4. Total Time Spent in Each Location by Cluster
# Calculates the total and average cumulative time spent in each location by cluster
total_time_spent_by_location <- unique_visits %>%
  group_by(cluster, location) %>%
  summarize(
    total_time_spent = sum(time_spent, na.rm = TRUE),
    avg_time_spent = mean(time_spent, na.rm = TRUE),
    .groups = 'drop'
  )


# 5. Trajectories Summary
# Summarize the qualitative trajectories by cluster
trajectories_summary <- aggregated_df %>%
  group_by(cluster, trajectory) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  ) %>%
  spread(key = trajectory, value = count, fill = 0)


########### Visualize Common Movements of Pedestrians ########
# Load necessary libraries
install.packages("ggalluvial")

#visualize the amount of time spent in different locations by each cluster
# Get the unique locations
locations <- unique(total_time_spent_by_location$location)

# Loop over each location and create a bar plot
for(loc in locations) {
  # Filter data for the current location
  data_for_location <- total_time_spent_by_location %>%
    filter(location == loc)
  
  # Create the bar plot
  p <- ggplot(data_for_location, aes(x = as.factor(cluster), y = avg_time_spent, fill = as.factor(cluster))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Average Time Spent at", loc, "by Cluster"),
         x = "Cluster",
         y = "Average Time Spent (sec)",
         fill = "Cluster") +
    theme_minimal()
  
  # Print the plot
  print(p)
}


#heat map for frequency each cluster's observations observe each trajectory
trajectories_long <- trajectories_summary %>%
  gather(key = "trajectory", value = "count", -cluster)

# Step 3: Create a heatmap
ggplot(trajectories_long, aes(x = cluster, y = trajectory, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Heatmap of Trajectory Counts by Cluster",
    x = "Cluster",
    y = "Trajectory",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#re-create heat map without platform trajectory to get more comparable results
# Step 1: Summarize the trajectories by cluster, excluding a specific trajectory
trajectories_summary_filter <- aggregated_df %>%
  filter(trajectory != "platform") %>%  # Exclude the specific trajectory
  group_by(cluster, trajectory) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  ) %>%
  spread(key = trajectory, value = count, fill = 0)

# Step 2: Convert the summarized data to long format for plotting
trajectories_long <- trajectories_summary_filter %>%
  gather(key = "trajectory", value = "count", -cluster)

# Step 3: Create a heatmap
ggplot(trajectories_long, aes(x = cluster, y = trajectory, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Heatmap of Trajectory Counts by Cluster",
    x = "Cluster",
    y = "Trajectory",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Step 1: Summarize the trajectories by cluster, excluding a specific trajectory if needed
trajectories_summary <- aggregated_df %>%
  group_by(cluster, trajectory) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  )

# Step 2: Create a bar graph for each trajectory
ggplot(trajectories_summary, aes(x = cluster, y = count, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ trajectory, scales = "free_y") + #free_y or fixed for the axis comparability
  labs(
    title = "Comparison of Trajectory Counts by Cluster",
    x = "Cluster",
    y = "Count",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Adjust sizes for readability

#Non-consecutive trajectory occurences for each cluster
# Step 1: Identify non-consecutive changes in trajectory for each pedestrian within each cluster
non_consecutive_trajectories <- aggregated_df %>%
  arrange(id, time_interval) %>%
  group_by(id, cluster) %>%
  mutate(
    prev_trajectory = lag(trajectory, default = first(trajectory)), # Previous trajectory
    trajectory_change = trajectory != prev_trajectory  # Identify trajectory change
  ) %>%
  filter(trajectory_change) %>%
  ungroup()

# Step 2: Count the number of first occurrences of each trajectory per pedestrian per cluster
trajectory_counts <- non_consecutive_trajectories %>%
  group_by(cluster, trajectory, id) %>%
  summarize(count = 1, .groups = 'drop') %>% # Count each first occurrence as 1
  group_by(cluster, trajectory) %>%
  summarize(total_count = sum(count), .groups = 'drop') # Sum up counts by cluster and trajectory

# Step 3: Visualize the result using ggplot2
ggplot(trajectory_counts, aes(x = trajectory, y = total_count, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Non-Consecutive Trajectory Counts by Cluster",
    x = "Trajectory",
    y = "Count",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(trajectory_counts, aes(x = cluster, y = total_count, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ trajectory, scales = "free_y") + #free_y or fixed for the axis comparability
  labs(
    title = "Comparison of Trajectory Counts by Cluster",
    x = "Cluster",
    y = "Count",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Adjust sizes for readability


#Transition comparisons
# Load the package
# Identify the main transitions for each cluster
main_transitions <- transitions %>%
  group_by(cluster) %>%
  top_n(n = 5, wt = transition_count) %>% # Adjust n to focus on top transitions
  ungroup()

# Create a flow diagram using ggalluvial
ggplot(main_transitions, aes(axis1 = location, axis2 = next_location, y = transition_count, fill = cluster)) +
  geom_alluvium(aes(fill = cluster)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 1.5) +
  scale_x_discrete(limits = c("From", "To")) +
  theme_minimal() +
  labs(title = "Top 5 Pedestrian Movements by Cluster",
       x = "Locations",
       y = "Transition Count",
       fill = "Cluster")

# Optionally, save the plot
ggsave("pedestrian_flow_diagram.png")


# Function to create and save an alluvial plot for a given cluster
create_alluvial_plot <- function(cluster_data, cluster_id) {
  # Create a unique transition label for each type of transition
  cluster_data <- cluster_data %>%
    mutate(transition_type = paste(location, "to", next_location, sep = " -> "))
  
  plot <- ggplot(cluster_data,
                 aes(axis1 = location, axis2 = next_location, y = transition_count, fill = transition_type)) +
    geom_alluvium() +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("From", "To")) +
    theme_bw() +
    labs(title = paste("Pedestrian Transitions for Cluster", cluster_id),
         x = "Locations",
         y = "Transition Count",
         fill = "Transition Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot) 
}

# Create alluvial plots for each cluster
Clusters <- unique(transitions$cluster)

for (cluster in Clusters) {
  cluster_data <- transitions %>%
    filter(cluster == Clusters)
  plot <- create_alluvial_plot(cluster_data, cluster_id)
  ggsave(filename = paste0("alluvial_plot_cluster_", cluster_id, ".png"), plot = plot, width = 10, height = 7)
}



########### Data Metrics for Overall Pedestrian Summary - Not cluster Based##############

# Check if the necessary columns are present
if (!all(c("id", "location", "time_spent", "time_interval") %in% colnames(aggregated_df))) {
  stop("Required columns are missing from the data.table")
}

# Function to calculate the average time spent in each location across all pedestrians
calculate_average_time_spent_location <- function(data) {
  data %>%
    arrange(id, time_interval) %>%  # Ensure data is ordered by id and time_interval
    group_by(id, location, visit_id) %>%  # Group by id, location, and visit_id
    filter(time_interval == min(time_interval)) %>%  # Keep only the first observation per visit
    summarise(total_time_spent = sum(time_spent, na.rm = TRUE), .groups = 'drop') %>%
    # Convert total time spent from seconds to minutes 
    mutate(total_time_spent_minutes = total_time_spent / 60) %>% 
    group_by(location) %>%
    summarise(average_time_spent_minutes = mean(total_time_spent_minutes, na.rm = TRUE), .groups = 'drop')
}

# Create a unique identifier for each visit
aggregated_df <- aggregated_df %>%
  arrange(id, time_interval) %>%
  group_by(id, location) %>%
  mutate(visit_id = cumsum(time_interval != lag(time_interval, default = first(time_interval))))

# Apply the function
average_time_spent_location <- calculate_average_time_spent_location(aggregated_df)

#Visualize the results horizontal with color
ggplot(average_time_spent_location, aes(x = reorder(location, average_time_spent_minutes), y = average_time_spent_minutes, fill = location)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for better differentiation
  labs(title = "Average Time Spent in Each Location",
       x = "Location",
       y = "Average Time Spent (Minutes)",
       fill = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

#Visualize the results vertical only blue
ggplot(average_time_spent_location, aes(x = location, y = average_time_spent_minutes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Time Spent by Location",
       x = "Location",
       y = "Average Time Spent (Minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

#calculate the average time spent in locations divided by the total number of pedestrians 
pedestrian <- unique(aggregated_df$id)
totalpax<-length(pedestrian)

# Aggregate the results to get the overall totals
final_avg_result1 <- total_time_spent %>%
  group_by(location) %>%
  summarize(total_time_spent = sum(total_time_spent),
            num_pedestrians = totalpax,
            average_time_spent = total_time_spent / num_pedestrians,
            .groups = 'drop')

#Visualize the results
ggplot(final_avg_result1, aes(x = location, y = average_time_spent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Time Spent by Location over all Pedestrians",
       x = "Location",
       y = "Time Spent over All Pedestrians") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

#find some insights for speed of movement around the platform using speed values
# Replace NA values in speed column with 0
aggregated_df$speed[is.na(aggregated_df$speed)] <- 0

speed_summary <- summary(aggregated_df$speed, digits=4)

# Create a data frame with summary statistics
speed_summary_table <- data.frame(
  Measures = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  Value = as.numeric(speed_summary)  # Convert summary to numeric (if needed)
)

#How much time do passengers spend standing  in place?
# Calculate total time where speed is 0
total_time_speed_zero <- sum(aggregated_df$time_diff[aggregated_df$speed == 0])

#convert to minutes (seconds/minutes)
total_time_speed_zero/60

#What is the average amount of time a passenger is standing?
# Calculate total time where speed is 0 for each pedestrian ID
standing_time_per_pedestrian <- aggregate(time_diff ~ id, data = aggregated_df[aggregated_df$speed == 0, ], FUN = sum)

# Calculate average standing time across all pedestrians
average_standing_time <- sum(standing_time_per_pedestrian$time_diff) / totalpax

# Calculate average number of locations a pedestrian is tracked
# Calculate the number of unique locations visited by each pedestrian
location_counts <- aggregated_df %>%
  group_by(id) %>%
  summarize(num_locations = n_distinct(location))

# Calculate the average number of locations visited across all pedestrians
avg_num_loc <- location_counts %>%
  summarize(avg_num_loc = mean(num_locations)) %>%
  pull(avg_num_loc)



