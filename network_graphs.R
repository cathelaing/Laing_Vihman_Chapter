library(igraph)

################################## SET UP #######################################

# Load data

globaldistance <- read_csv("globaldistance_final.csv")
distance_df <- read_csv("distance_df.csv") %>% dplyr::select(
  Subject, Age, Gloss, GlossID, geminate_A) %>%
  rename(gloss1 = GlossID)
thresholds <- read_csv("globalthresholds.csv") %>% left_join(distance_df)

# Set colour properties

fine = 500 # this will adjust the resolving power for colour of nodes
pal = colorRampPalette(c('red','green'))   # set up colour palette for nodes
cut.off <- .25  # set threshold cut off

# colour palette legend:

color_num = 0:max(thresholds$degree) #create a color palette of the same size as the number of vertices.
color_spectrum <- pal(length(unique(color_num)))
ordered <- order(color_num) # map the pallete to the order of values on vertices
color <- vector(length = length(ordered), mode="double")

for(i in 1:length(ordered)){
  color[ordered[i]] <- color_spectrum [i]
}

############################### CREATE DATAFRAMES ##################################

infants <- thresholds %>% dplyr::select(Speaker, gloss1)

infants_list <- infants %>%
  split(., f = .$Speaker)

graph_base <- globaldistance %>%              # create edges
  distinct(Speaker, gloss1, distance, .keep_all = TRUE) %>%
  rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globalnodes <- graph_base %>%      # create nodes
  select(-to) %>% 
  distinct(from, Speaker, .keep_all = TRUE)

# test <- graph_from_data_frame(d=graph_base, vertices=globalnodes, directed=F)
# net_plot_threshold <- delete_edges(test, which(E(test)$weight > cut.off))
# plot(net_plot_threshold)

########################### DATA LOOP ##################################

# Create graph data for each child
# in the global network, the data is considered at each month + all previous months, as specified in age <= element$age

globalgraphdata <- lapply(infants_list, FUN = function(element) {
  edges_net <- graph_base %>% filter(Speaker %in% element$Speaker) %>% distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes %>% filter(Speaker %in% element$Speaker)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete_edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
})

plot(globalgraphdata$Adeline)

######################## Calculating small-world properties - age data #####################

# Average path length for each subject in each session

globalpathlength_base <- lapply(globalgraphdata, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength <- reshape2::melt(globalpathlength_base) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y",
         "Speaker" = "L1")

# Clustering coefficient for each subject in each session

globalclusteringcoef_global_base <- lapply(globalgraphdata, FUN = function(element) {
  global_clustering_coef <- transitivity(element)
})

globalclusteringcoef_global <- reshape2::melt(globalclusteringcoef_global_base) %>%
  rename("clust_coef_global" = "value", 
         "Speaker" = "L1") 

globalclusteringcoef_avg_base <- lapply(globalgraphdata, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "average")
})

globalclusteringcoef_avg <- reshape2::melt(globalclusteringcoef_avg_base) %>%
  rename("clust_coef_avg" = "value",
         "Speaker" = "L1")


globalsmallworlddata <- globalpathlength %>% 
  left_join(globalclusteringcoef_global, by = c("Speaker")) %>%
  left_join(globalclusteringcoef_avg, by = c("Speaker")) 

write_csv(globalsmallworlddata, "globalsmallworlddata.csv")
