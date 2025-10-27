# This script prepares the data for analysis from each of the global distance matrices
# It calculates the degree for each word pair across a set of thresholds from E=0.1-1 in relation to age, vocab size, and age of production (global network)
# It then runs correlations between degree and AOP

globaldistance <- read_csv("globaldistance_final.csv")

# Create a new dataframe to show degree of connectivity for a range of thresholds between 0 and 1, as per Amatuni & Bergelson, 2017

#thresholds <- seq(from = 0, to = 1, by = 0.01)  # create empty list
thresholds <- 0.25
names(thresholds) <- paste("threshold", thresholds, sep ="_") # name list object

globaldistance_list <- lapply(thresholds, function(t) {
  filter(globaldistance, distance_norm < t) %>%
    group_by(Speaker, gloss1) %>%                     # for gloss1 side of the data, otherwise 50% of data is missed (oops)
    tally()
})

globaldistance_list_melted <- reshape2::melt(globaldistance_list)

globaldistance_list_degree <- globaldistance_list_melted %>%
  rename("degree" = "value") %>%
  separate(L1, into = c("remove", "threshold"), sep = "_") %>%
  filter(threshold == 0.25) %>%
  dplyr::select(-remove, -threshold, -variable)

globalthresholds <- globaldistance_list_melted %>%
  rename("degree" = "value") %>%
  separate(L1, c("l1", "threshold"), sep = "_") %>%
  dplyr::select(-l1, -variable)

write_csv(globalthresholds, "globalthresholds.csv")
