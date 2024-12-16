######### Stage 1: Loading libraries and reading the CSV files #########

# Change to working directory
setwd("/Users/aliciang/Downloads/ETW2500/A2")

# Load the necessary libraries
library(pacman)
p_load(tidyverse, factoextra, cluster, dendextend)

# Load the csv file
student_data <- read.csv("Student_Data.csv")
student_data <- as.data.frame(student_data)
str(student_data)

########### Stage 2: Variable Exploration ###########

# Extract variables of interest to create Master Dataset
master_data <- data.frame(
  # Social Perspective
  student_data$goout,
  student_data$activities,
  
  # Well-being
  student_data$Walc,
  student_data$health,
  
  # Family Background
  student_data$famsup, 
  student_data$famrel)

# Check for NA value
any(is.na(master_data))

# Randomization process for sample selection
set.seed(31861148)
master_sample <- master_data[sample(1:nrow(student_data), 200), ] 

# Change "yes" "no" to Logical format
master_sample$student_data.activities <- ifelse(master_sample$student_data.activities == "yes",TRUE, FALSE)
master_sample$student_data.famsup <- ifelse(master_sample$student_data.famsup == "yes",TRUE, FALSE)

########### Stage 3: Variable Construction through Hierarchical Clustering ###########

########### [a] Social Perspective ###########

# Select variables for Social Perspective
set.seed(31861148)
social_perspective <- data.frame(
  master_sample$student_data.goout,
  master_sample$student_data.activities
)

# Scaled and ensure it is in numerical format for "goout"
social_perspective$master_sample.student_data.goout <- as.numeric(scale(social_perspective$master_sample.student_data.goout))

# Ensure it's in a dataframe format
social_perspective <- as.data.frame(social_perspective)

# Elbow Method : Social Perspective
set.seed(31861148)
social_perspective_elbow <- fviz_nbclust(social_perspective, FUNcluster = hcut, method = "wss",
                                         k.max = 6) + 
  labs(title="Elbow Method [Social Perspective]")
social_perspective_elbow

# Silhouette Method : Social Perspective
set.seed(31861148)
social_perspective_silhouette <- fviz_nbclust(social_perspective, FUNcluster = hcut, method = "silhouette",
                                              k.max = 6) + 
  labs(title="Silhouette Method [Social Perspective]")
social_perspective_silhouette

# Set binary variables as factors
social_perspective$master_sample.student_data.activities <- as.factor(social_perspective$master_sample.student_data.activities)

# Compute Gower distance
gower_dist <- daisy(social_perspective, metric = "gower")

# Perform hierarchical clustering - Complete
social_perspective_complete <- hclust(gower_dist, method = "complete")
plot(color_branches(as.dendrogram(social_perspective_complete), k=5), main = "Complete Method with K=5") 

# Perform hierarchical clustering - Average
social_perspective_average <- hclust(gower_dist, method = "average")
plot(color_branches(as.dendrogram(social_perspective_average), k=5), main = "Average Method with K=5") 

# Perform hierarchical clustering - Ward
social_perspective_ward <- hclust(gower_dist, method = "ward.D2")
plot(color_branches(as.dendrogram(social_perspective_ward), k=5), main = "Ward Method with K=5") 

# Check Count - Complete
set.seed(31861148)
social_perspective_cluster_complete_k5 <- cutree(social_perspective_complete, k=5)
social_perspective_complete_k5 <- master_sample %>% mutate(cluster1_ck5 = social_perspective_cluster_complete_k5)

# Check Count - Average
set.seed(31861148)
social_perspective_cluster_average_k5 <- cutree(social_perspective_average, k=5)
social_perspective_average_k5 <- master_sample %>% mutate(cluster1_ak5 = social_perspective_cluster_average_k5)

# Check Count - Ward
set.seed(31861148)
social_perspective_cluster_ward_k5 <- cutree(social_perspective_ward, k=5)
social_perspective_ward_k5 <- master_sample %>% mutate(cluster1_wk5 = social_perspective_cluster_ward_k5)

# List out the counts in each cluster for each hierachical clustering method
social_perspective_complete_k5 %>%  count(cluster1_ck5)
social_perspective_average_k5 %>%  count(cluster1_ak5)
social_perspective_ward_k5 %>%  count(cluster1_wk5)

# Count the mean of each variable in each cluster
social_perspective_ward_k5 %>%
  group_by(cluster1_wk5) %>%
  summarise(across(c( "student_data.goout", "student_data.activities"), 
                   ~round(mean(as.numeric(.x), na.rm = TRUE),0)))

########### [b] Well Being ###########

# Select variables for Well Being
set.seed(31861148)
well_being <- data.frame(
  master_sample$student_data.health, 
  master_sample$student_data.Walc 
)

# Scaling
well_being <- scale(well_being)
well_being <- as.data.frame(well_being)

# Elbow Method : Well Being
set.seed(31861148)
well_being_elbow <- fviz_nbclust(well_being, FUNcluster = hcut, method = "wss",
                                 k.max = 6) + 
  labs(title="Elbow Method [Well Being]")
well_being_elbow

# Silhouette Method : Well Being
set.seed(31861148)
well_being_silhouette <- fviz_nbclust(well_being, FUNcluster = hcut, method = "silhouette",
                                      k.max = 6) + 
  labs(title="Silhouette Method [Well Being]")
well_being_silhouette

# Perform hierarchical clustering - Complete
set.seed(31861148)
well_being_complete <- hclust(dist(well_being), method = "complete")
plot(color_branches(as.dendrogram(social_perspective_complete), k=3), main = "Complete Method with K=3") 

# Perform hierarchical clustering - Average
set.seed(31861148)
well_being_average <- hclust(dist(well_being), method = "average")
plot(color_branches(as.dendrogram(social_perspective_average), k=3), main = "Average Method with K=3") 

# Perform hierarchical clustering - Ward
set.seed(31861148)
well_being_ward <- hclust(dist(well_being), method = "ward.D")
plot(color_branches(as.dendrogram(social_perspective_ward), k=3), main = "Ward Method with K=3")

# Check count - Complete
set.seed(31861148)
well_being_cluster_complete_k3 <- cutree(well_being_complete, k=3)
well_being_complete_k3 <- master_sample %>% mutate(cluster2_ck3 = well_being_cluster_complete_k3)

# Check count - Average
set.seed(31861148)
well_being_cluster_average_k3 <- cutree(well_being_average, k=3)
well_being_average_k3 <- master_sample %>% mutate(cluster2_ak3 = well_being_cluster_average_k3)

# Check count - Ward
set.seed(31861148)
well_being_cluster_ward_k3 <- cutree(well_being_ward, k=3)
well_being_ward_k3 <- master_sample %>% mutate(cluster2_wk3 = well_being_cluster_ward_k3)

# List out the counts in each cluster for each hierachical clustering method
well_being_complete_k3 %>%  count(cluster2_ck3)
well_being_average_k3 %>%  count(cluster2_ak3)
well_being_ward_k3 %>%  count(cluster2_wk3)

# Count the mean of each variable in each cluster
well_being_ward_k3 %>%
  group_by(cluster2_wk3) %>%
  summarise(across(c("student_data.health","student_data.Walc"), 
                   ~round(mean(as.numeric(.x), na.rm = TRUE), 0)))

########### [c] Family Background ###########

# Select variables for Family Background
set.seed(31861148)
family_background <- data.frame(
  master_sample$student_data.famrel,     
  master_sample$student_data.famsup
)

# Scale only for Family Relationships
family_background$master_sample.student_data.famrel <- as.numeric(scale(family_background$master_sample.student_data.famrel))

family_background <- as.data.frame(family_background)

# Elbow Method : Family Background
set.seed(31861148)
family_background_elbow <- fviz_nbclust(family_background, FUNcluster = hcut, method = "wss",
                                        k.max = 6) + 
  labs(title="Elbow Method [Family Background]")
family_background_elbow

# Silhouette Method : Family Background
set.seed(31861148)
family_background_silhouette <- fviz_nbclust(family_background, FUNcluster = hcut, method = "silhouette",
                                             k.max = 6) + 
  labs(title="Silhouette Method [Family Background]")
family_background_silhouette

# Set binary variables as factors
family_background$master_sample.student_data.famsup <- as.factor(family_background$master_sample.student_data.famsup)

# Compute Gower distance
gower_dist <- daisy(family_background, metric = "gower")

# Perform hierarchical clustering - Complete
family_background_complete <- hclust(gower_dist, method = "complete")
plot(color_branches(as.dendrogram(family_background_complete), k=5), main = "Complete Method with K=5")

# Perform hierarchical clustering - Average
family_background_average <- hclust(gower_dist, method = "average")
plot(color_branches(as.dendrogram(family_background_average), k=5), main = "Average Method with K=5")

# Perform hierarchical clustering - Ward
family_background_ward <- hclust(gower_dist, method = "ward.D2")
plot(color_branches(as.dendrogram(family_background_ward), k=5), main = "Ward Method with K=5")

# Check count - Complete
set.seed(31861148)
family_background_cluster_complete_k5 <- cutree(family_background_complete, k=5)
family_background_complete_k5 <- master_sample %>% mutate(cluster3_ck5 = family_background_cluster_complete_k5)

# Check count - Average
set.seed(31861148)
family_background_cluster_average_k5 <- cutree(family_background_average, k=5)
family_background_average_k5 <- master_sample %>% mutate(cluster3_ak5 = family_background_cluster_average_k5)

# Check count - Ward
set.seed(31861148)
family_background_cluster_ward_k5 <- cutree(family_background_ward, k=5)
family_background_ward_k5 <- master_sample %>% mutate(cluster3_wk5 = family_background_cluster_ward_k5)

# Count the items in cluster
family_background_complete_k5 %>%  count(cluster3_ck5)
family_background_average_k5 %>%  count(cluster3_ak5)
family_background_ward_k5 %>%  count(cluster3_wk5)

family_background_ward_k5 %>%
  group_by(cluster3_wk5) %>%
  summarise(across(c("student_data.famrel","student_data.famsup"), 
                   ~round(mean(as.numeric(.x), na.rm = TRUE), 0)))

########### Stage 4: Final Data set ###########

final <- data.frame(social_perspective_ward_k5$cluster1_wk5, well_being_ward_k3$cluster2_wk3, family_background_ward_k5$cluster3_wk5)
final

########### Stage 5: Hierarchical Clustering for Stress Level ###########

set.seed(31861148)
final_elbow <- fviz_nbclust(final, FUNcluster = hcut, method = "wss", 
                            k.max = 6) +
  labs(title="Elbow method for Stress Level")
final_elbow

set.seed(31861148)
final_silhouette <- fviz_nbclust(final, FUNcluster = hcut, method = "silhouette", 
                                 k.max = 6) +
  labs(title="Silhouette method for Stress Level")
final_silhouette

# Perform hierarchical clustering - Complete
set.seed(31861148)
final_hclus_complete <- hclust(daisy(final,metric="gower"), method = "complete")
plot(color_branches(as.dendrogram(final_hclus_complete), k=3), main = "Complete Method with K=3") 

# Perform hierarchical clustering - Average
set.seed(31861148)
final_hclus_average <- hclust(daisy(final,metric="gower"), method = "average")
plot(color_branches(as.dendrogram(final_hclus_average), k=3), main = "Average Method with K=3") 

# Perform hierarchical clustering - Ward
set.seed(31861148)
final_hclus_ward <- hclust(daisy(final,metric="gower"), method = "ward.D")
plot(color_branches(as.dendrogram(final_hclus_ward), k=3), main = "Ward Method with K=3") 

# Check count - Complete
set.seed(31861148)
final_cluster_comp_k3 <- cutree(final_hclus_complete, k=3)
final_data_comp_k3 <- final %>% mutate(clusterf_ck3 = final_cluster_comp_k3)

# Check count - Average
final_cluster_average_k3 <- cutree(final_hclus_average, k=3)
final_data_average_k3 <- final %>% mutate(clusterf_ak3 = final_cluster_ward_k3)

# Check count - Ward
final_cluster_ward_k3 <- cutree(final_hclus_ward, k=3)
final_data_ward_k3 <- final %>% mutate(clusterf_wk3 = final_cluster_ward_k3)

# Count the items in cluster
final_data_comp_k3 %>%  count(clusterf_ck3)
final_data_average_k3 %>%  count(clusterf_ak3)
final_data_ward_k3 %>%  count(clusterf_wk3)

get_mode <- function(x) {
  uniq_x <- unique(x)
  freq_table <- table(x)
  modes <- names(freq_table[freq_table == max(freq_table)])
  return(as.numeric(modes))
}

# Reframe to get mode for each variable
summary_data <- final_data_comp_k3 %>%
  group_by(clusterf_wk3) %>%
  reframe(across(everything(), ~ get_mode(.x), .names = "mode_{.col}"))

# Rename columns 
colnames(summary_data) <- c(
  "Cluster",                    
  "Social_Perspective",    
  "Well_Being",          
  "Family_Background")
summary_data

















