library(igraph)

################################## SET UP #######################################

# Load data

globaldistance <- read_csv("globaldistance_final.csv")
distance_df <- read_csv("distance_df.csv") %>% dplyr::select(
  Subject, Language, Age, Gloss, GlossID, geminate_A, geminate_T, data_type) %>%
  rename(gloss1 = GlossID)
thresholds <- read_csv("globalthresholds.csv") %>%
  left_join(distance_df)
subj_lang <- distance_df %>% group_by(Subject, Language) %>% tally() %>%
  rename("Speaker" = Subject) %>%
  dplyr::select(-n)

ggplot(globaldistance, aes(x = distance_norm, fill = data_type)) +
         geom_density(alpha = 0.5) + theme_bw() 

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

## infant graphs

## Target

infants_subj_T <- thresholds %>% filter(data_type == "target") %>% dplyr::select(Subject, gloss1)

infants_list_subj_T <- infants_subj_T %>%
  split(., f = .$Subject)

graph_base_subj_T <- globaldistance %>%              # create edges
  filter(data_type == "target") %>%
  distinct(Speaker, gloss1, distance, .keep_all = TRUE) %>%
  rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globalnodes_subj_T <- graph_base_subj_T %>%      # create nodes
  select(-to) %>% 
  distinct(from, Speaker, .keep_all = TRUE)

# test <- graph_from_data_frame(d=graph_base_subj_T, vertices=globalnodes_subj_T, directed=F)
# net_plot_threshold <- delete_edges(test, which(E(test)$weight > cut.off))
# plot(net_plot_threshold)

########################### DATA LOOP ##################################

# Create graph data for each child
# in the global network, the data is considered at each month + all previous months, as specified in age <= element$age

globalgraphdata_subj_T <- lapply(infants_list_subj_T, FUN = function(element) {
  edges_net <- graph_base_subj_T %>% filter(Speaker %in% element$Subject) %>% 
    distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes_subj_T %>% filter(Speaker %in% element$Subject)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete_edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
})

plot(globalgraphdata_subj_T$Carys)

######################## Calculating small-world properties - age data #####################

# Average path length for each subject in each session

globalpathlength_base_subj_T <- lapply(globalgraphdata_subj_T, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength_subj_T <- reshape2::melt(globalpathlength_base_subj_T) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y",
         "Speaker" = "L1")

# Clustering coefficient for each subject in each session

globalclusteringcoef_global_base_subj_T <- lapply(globalgraphdata_subj_T, FUN = function(element) {
  global_clustering_coef <- transitivity(element, type = "global")
})

globalclusteringcoef_global_subj_T <- reshape2::melt(globalclusteringcoef_global_base_subj_T) %>%
  rename("clust_coef_global" = "value", 
         "Speaker" = "L1") 

globalclusteringcoef_avg_base_subj_T <- lapply(globalgraphdata_subj_T, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "local")
})

globalclusteringcoef_avg_subj_T <- reshape2::melt(globalclusteringcoef_avg_base_subj_T) %>%
  rename("clust_coef_avg" = "value",
         "Speaker" = "L1")


globalsmallworlddata_subj_T <- globalpathlength_subj_T %>% 
  left_join(globalclusteringcoef_global_subj_T, by = c("Speaker")) %>%
 # left_join(globalclusteringcoef_avg_subj_T, by = c("Speaker")) %>%
  left_join(subj_lang) %>%
  mutate(data_type = "target",
         Speaker_ID = Speaker,
         ID = NA)


## Actual

infants_subj_A <- thresholds %>% filter(data_type == "actual") %>% dplyr::select(Speaker_ID, gloss1)

infants_list_subj_A <- infants_subj_A %>%
  split(., f = .$Speaker_ID)

graph_base_subj_A <- globaldistance %>%              # create edges
  filter(data_type == "actual") %>%
  distinct(Speaker_ID, gloss1, distance, .keep_all = TRUE) %>%
  rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globalnodes_subj_A <- graph_base_subj_A %>%      # create nodes
  select(-to) %>% 
  distinct(from, Speaker_ID, .keep_all = TRUE)

# test <- graph_from_data_frame(d=graph_base, vertices=globalnodes, directed=F)
# net_plot_threshold <- delete_edges(test, which(E(test)$weight > cut.off))
# plot(net_plot_threshold)

########################### DATA LOOP ##################################

# Create graph data for each child
# in the global network, the data is considered at each month + all previous months, as specified in age <= element$age

globalgraphdata_subj_A <- lapply(infants_list_subj_A, FUN = function(element) {
  edges_net <- graph_base_subj_A %>% filter(Speaker_ID %in% element$Speaker_ID) %>% distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes_subj_A %>% filter(Speaker_ID %in% element$Speaker_ID)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete_edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
})

plot(globalgraphdata_subj_A$Afan_1)

######################## Calculating small-world properties - age data #####################

# Average path length for each subject in each session

globalpathlength_base_subj_A <- lapply(globalgraphdata_subj_A, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength_subj_A <- reshape2::melt(globalpathlength_base_subj_A) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y",
         "Speaker_ID" = "L1")

# Clustering coefficient for each subject in each session

globalclusteringcoef_global_base_subj_A <- lapply(globalgraphdata_subj_A, FUN = function(element) {
  global_clustering_coef <- transitivity(element, type = "global")
})

globalclusteringcoef_global_subj_A <- reshape2::melt(globalclusteringcoef_global_base_subj_A) %>%
  rename("clust_coef_global" = "value", 
         "Speaker_ID" = "L1") 

globalclusteringcoef_avg_base_subj_A <- lapply(globalgraphdata_subj_A, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type="local")
})

globalclusteringcoef_avg_subj_A <- reshape2::melt(globalclusteringcoef_avg_base_subj_A) %>%
  rename("clust_coef_avg" = "value",
         "Speaker_ID" = "L1")


globalsmallworlddata_subj_A <- globalpathlength_subj_A %>% 
  left_join(globalclusteringcoef_global_subj_A, by = c("Speaker_ID")) %>%
  #left_join(globalclusteringcoef_avg_subj_A, by = c("Speaker_ID"))  %>%
  separate(Speaker_ID, c("Speaker", "ID"), sep = "_", remove = F) %>%
  left_join(subj_lang) %>%
  mutate(data_type = "actual")


globalsmallworlddata_subj <- rbind(globalsmallworlddata_subj_T, globalsmallworlddata_subj_A) %>%
  #mutate(LangID = "") %>%
  dplyr::select(-ID, -Speaker_ID)

## language graphs

## Target

infants_lang_T <- thresholds %>% 
  filter(data_type == "target") %>% 
  dplyr::select(Subject, Language, gloss1) %>%
  rename("Speaker" = Subject) %>%
  left_join(subj_lang)

infants_list_lang_T <- infants_lang_T %>%
  split(., f = .$Language)

graph_base_lang_T <- globaldistance %>%              # create edges
  left_join(subj_lang) %>%
  filter(data_type == "target") %>%
  distinct(Speaker, Language, gloss1, distance, .keep_all = TRUE) %>%
  rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globalnodes_lang_T <- graph_base_lang_T %>%      # create nodes
  select(-to) %>% 
  distinct(from, Speaker, Language, .keep_all = TRUE)

# test <- graph_from_data_frame(d=graph_base, vertices=globalnodes, directed=F)
# net_plot_threshold <- delete_edges(test, which(E(test)$weight > cut.off))
# plot(net_plot_threshold)

########################### DATA LOOP ##################################

# Create graph data for each child
# in the global network, the data is considered at each month + all previous months, as specified in age <= element$age

globalgraphdata_lang_T <- lapply(infants_list_lang_T, FUN = function(element) {
  edges_net <- graph_base_lang_T %>% filter(Language %in% element$Language) %>% distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes_lang_T %>% filter(Language %in% element$Language)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete_edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
})

plot(globalgraphdata_lang_T$UK)

######################## Calculating small-world properties - age data #####################

# Average path length for each subject in each session

globalpathlength_base_lang_T <- lapply(globalgraphdata_lang_T, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength_lang_T <- reshape2::melt(globalpathlength_base_lang_T) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y",
         "Language" = "L1")

# Clustering coefficient for each subject in each session

globalclusteringcoef_global_base_lang_T <- lapply(globalgraphdata_lang_T, FUN = function(element) {
  global_clustering_coef <- transitivity(element, type ="global")
})

globalclusteringcoef_global_lang_T <- reshape2::melt(globalclusteringcoef_global_base_lang_T) %>%
  rename("clust_coef_global" = "value", 
         "Language" = "L1") 

globalclusteringcoef_avg_base_lang_T <- lapply(globalgraphdata_lang_T, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "local")
})

globalclusteringcoef_avg_lang_T <- reshape2::melt(globalclusteringcoef_avg_base_lang_T) %>%
  rename("clust_coef_avg" = "value",
         "Language" = "L1")


globalsmallworlddata_lang_T <- globalpathlength_lang_T %>% 
  left_join(globalclusteringcoef_global_lang_T, by = c("Language")) %>%
  mutate(data_type = "target",
         Speaker = NA)


## Actual

infants_lang_A <- thresholds %>% 
  filter(data_type == "actual_lang") %>% 
  dplyr::select(Speaker_ID, gloss1) %>%
  rename("Speaker" = Speaker_ID) %>%
  left_join(subj_lang)

infants_list_lang_A <- infants_lang_A %>%
  split(., f = .$Language)

graph_base_lang_A <- globaldistance %>%              # create edges
  left_join(subj_lang) %>%
  filter(data_type == "actual_lang") %>%
  distinct(Speaker, Language, gloss1, distance, .keep_all = TRUE) %>%
  rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globalnodes_lang_A <- graph_base_lang_A %>%      # create nodes
  select(-to) %>% 
  distinct(from, Speaker, Language, .keep_all = TRUE)

# test <- graph_from_data_frame(d=graph_base, vertices=globalnodes, directed=F)
# net_plot_threshold <- delete_edges(test, which(E(test)$weight > cut.off))
# plot(net_plot_threshold)

########################### DATA LOOP ##################################

# Create graph data for each child
# in the global network, the data is considered at each month + all previous months, as specified in age <= element$age

globalgraphdata_lang_A <- lapply(infants_list_lang_A, FUN = function(element) {
  edges_net <- graph_base_lang_A %>% filter(Language %in% element$Language) %>% distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes_lang_A %>% filter(Language %in% element$Language)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete_edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
})

plot(globalgraphdata_lang_A$Urdu)

######################## Calculating small-world properties - age data #####################

# Average path length for each subject in each session

globalpathlength_base_lang_A <- lapply(globalgraphdata_lang_A, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength_lang_A <- reshape2::melt(globalpathlength_base_lang_A) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y",
         "Language" = "L1")

# Clustering coefficient for each subject in each session

globalclusteringcoef_global_base_lang_A <- lapply(globalgraphdata_lang_A, FUN = function(element) {
  global_clustering_coef <- transitivity(element, type = "global")
})

globalclusteringcoef_global_lang_A <- reshape2::melt(globalclusteringcoef_global_base_lang_A) %>%
  rename("clust_coef_global" = "value", 
         "Language" = "L1") 

globalclusteringcoef_avg_base_lang_A <- lapply(globalgraphdata_lang_A, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "local")
})

globalclusteringcoef_avg_lang_A <- reshape2::melt(globalclusteringcoef_avg_base_lang_A) %>%
  rename("clust_coef_avg" = "value",
         "Language" = "L1")


globalsmallworlddata_lang_A <- globalpathlength_lang_A %>% 
  left_join(globalclusteringcoef_global_lang_A, by = c("Language")) %>%
 # left_join(globalclusteringcoef_avg_lang_A, by = c("Language"))  %>%
  mutate(data_type = "actual",
         Speaker = NA)

globalsmallworlddata_lang <- rbind(globalsmallworlddata_lang_T, globalsmallworlddata_lang_A)# %>%
  # mutate(Speaker_ID = NA,
  #        LangID = Language) 

globalsmallworlddata <- rbind(globalsmallworlddata_lang, globalsmallworlddata_subj)

write_csv(globalsmallworlddata, "globalsmallworlddata.csv")
