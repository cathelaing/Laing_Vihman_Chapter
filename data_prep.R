library(arsenal)
library(tidyverse)
options("encoding" = "UTF-8")


full_data <- read_csv("data_ready.csv") %>%
  filter(Language %in% c("French", "US", "UK", "Finnish", "Japanese", "Urdu"))

full_data %>% group_by(Subject, Language) %>% tally()

# remove vowels

# substitute all target vowels for generic V because I don't care about vowels
full_data$Vremoved_target <- gsub("([
e 
a
ɑ
u
ə
ɔ
ɛ
o
i
ø
y
ɥ
œ
æ
ɤ
ɯ])", "V", full_data$IPAtarget) 

## assumptions about the data:

# palatalized consonants = Cj
full_data$Vremoved_target <- gsub("ʲ", "j", full_data$Vremoved_target)  

## questions about the data:

# what to do about transcriptions marked with "?", "Q3", "[lVttV?]", "&"

full_data$Vremoved_target <- gsub("VVV", "V", full_data$Vremoved_target)  # remove triphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
full_data$Vremoved_target <- gsub("VV", "V", full_data$Vremoved_target)  # remove diphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
full_data <- full_data %>% mutate(nsyl_target = stringr::str_count(Vremoved_target, "V"),
                                                          nsyl_target = ifelse(nsyl_target == 0, 1, nsyl_target))

full_data$Vremoved_target <- gsub("ʁ", "R", full_data$Vremoved_target) # code won't run properly with /ʁ/ so change to /R/
full_data$Vremoved_target <- gsub("ʁ", "R", full_data$Vremoved_target) # code won't run properly with /ʁ/ so change to /R/
full_data$Vremoved_target <- gsub("Vː", "V", full_data$Vremoved_target)  

# substitute all actual vowels for generic V because I don't care about vowels here either

full_data$Vremoved_actual <- gsub("([ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə æ ɜ ʉ ɨ œ ɒ ɤ ɵ ʊ ε ɚ ɶ ɯ])", "V", 
                                  full_data$IPAactual)    # vowels taken from runnng Phone Inventory script in Phon

full_data$Vremoved_actual <- gsub("ʁ", "R", full_data$Vremoved_actual) # code won't run properly with /ʁ/ so change to /R/
full_data$Vremoved_actual <- gsub("V::", "V", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("V:::", "V", full_data$Vremoved_actual) 
full_data$Vremoved_actual <- gsub("Vːː", "V", full_data$Vremoved_actual)  
full_data$Vremoved_actual <- gsub("Vː", "V", full_data$Vremoved_actual)  
full_data$Vremoved_actual <- gsub("V:", "V", full_data$Vremoved_actual)  
full_data$Vremoved_actual <- gsub("VVV", "V", full_data$Vremoved_actual)  
full_data$Vremoved_actual <- gsub("VV", "V", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("Ṽ", "V", full_data$Vremoved_actual) ## check this works
full_data$Vremoved_actual <- gsub("ʷ", "w", full_data$Vremoved_actual) ## check all the below
full_data$Vremoved_actual <- gsub("ⁿ", "n", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʲ", "j", full_data$Vremoved_actual) ## check this with Marilyn
full_data$Vremoved_actual <- gsub("ʰ", "", full_data$Vremoved_actual)  ## just remove aspiration?
full_data$Vremoved_actual <- gsub("t¸", "t", full_data$Vremoved_actual)


######

## To check with Marilyn:
# ...Vp:VʃV
# V?VʷVʊ::
# ?
#[V2VdVffVrVntVtVrgVts?]
#[:]
#[VtV]
#bVbV...dV
# bVʔV[B]V[=VtVpVclVsVrVfVlVps:V117]
# bVˈbV[:]
# [?]
# hVp'*2)
# kVhːhttV[Q3]
# jV)
# kVk*2)
# kVʔV...
# mV[Q2]
# mVlːʲV[Q2]
# nV-nV
# pl̥VʰpV[CH]
# pʰVtʰVpʰVpʰkʰV..
# tVː'gVː
# tVːkŋ̩̻
# ts̩tʰV[x2,Vwh]
# ts̩ɬV̥V[wh]
# wVʔwV*2)
# wʊʰʷVʔ
# , - what is this supposed to be?
# devoicing - need to go through csv file and manually change these :(  ˳

#Meeting with Marilyn
# remove imitations
# remove segments marked as ?
# remove palatalized consonants

# ' - deleted in main doc
#ʰ - deleted in main doc
# ̩ - deleted in main doc
# m̻ - might need to go through and find all of these....
# | - deleted in main doc

# ŋ̩  - re-run code and check this
 

#####

## replace long consonants to maintain geminate equivalents across all datasets
full_data$Vremoved_actual <- gsub("t:", "t-t", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("b:", "b-b", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("z:", "z-z", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("p:", "p-p", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("s:", "s-s", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("v:", "v-v", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("j:", "j-j", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʃ:", "ʃ-ʃ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("l:", "l-l", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("d:", "d-d", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("n:", "n-n", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("k:", "k-k", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ɲ:", "ɲ-ɲ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ŋ:", "ŋ-ŋ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("m:", "m-m", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("r:", "r-r", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʂ:", "ʂ-ʂ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("g:", "g-g", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʦ:", "ʦ-ʦ", full_data$Vremoved_actual)  ### CHECK THIS ONE
full_data$Vremoved_actual <- gsub("h:", "h-h", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ɬ:", "ɬ-ɬ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ç:", "ç-ç", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("x:", "x-x", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("f:", "f-f", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʔ:", "ʔ-ʔ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("tː", "t-t", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("bː", "b-b", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("zː", "z-z", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("pː", "p-p", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("sː", "s-s", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("vː", "v-v", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("jː", "j-j", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʃː", "ʃ-ʃ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("lː", "l-l", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("dː", "d-d", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("nː", "n-n", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("kː", "k-k", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ɲː", "ɲ-ɲ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ŋː", "ŋ-ŋ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("mː", "m-m", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("rː", "r-r", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʂː", "ʂ-ʂ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("gː", "g-g", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʦː", "ʦ-ʦ", full_data$Vremoved_actual)  ### CHECK THIS ONE
full_data$Vremoved_actual <- gsub("hː", "h-h", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ɬː", "ɬ-ɬ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("xː", "x-x", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("xːː", "x-x", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("fː", "f-f", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʔː", "ʔ-ʔ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("çː", "ç-ç", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("θːː", "θ-θ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("βːː", "β-β", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("θː", "θ-θ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ɸː", "ɸ-ɸ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ɸː", "ɸ-ɸ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("mːː", "m-m", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("βː", "β-β", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("wː", "w-w", full_data$Vremoved_actual)

full_data$Vremoved_target <- gsub("t:", "t-t", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("b:", "b-b", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("z:", "z-z", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("p:", "p-p", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("s:", "s-s", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("v:", "v-v", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("j:", "j-j", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʃ:", "ʃ-ʃ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("l:", "l-l", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("d:", "d-d", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("n:", "n-n", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("k:", "k-k", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ɲ:", "ɲ-ɲ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ŋ:", "ŋ-ŋ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("m:", "m-m", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("r:", "r-r", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʂ:", "ʂ-ʂ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("g:", "g-g", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʦ:", "ʦ-ʦ", full_data$Vremoved_target)  ### CHECK THIS ONE
full_data$Vremoved_target <- gsub("h:", "h-h", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ɬ:", "ɬ-ɬ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ç:", "ç-ç", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("x:", "x-x", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("f:", "f-f", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("tː", "t-t", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("bː", "b-b", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("zː", "z-z", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("pː", "p-p", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("sː", "s-s", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("vː", "v-v", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("jː", "j-j", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʃː", "ʃ-ʃ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("lː", "l-l", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("dː", "d-d", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("nː", "n-n", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("kː", "k-k", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ɲː", "ɲ-ɲ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ŋː", "ŋ-ŋ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("mː", "m-m", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("rː", "r-r", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʂː", "ʂ-ʂ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("gː", "g-g", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʦː", "ʦ-ʦ", full_data$Vremoved_target)  ### CHECK THIS ONE
full_data$Vremoved_target <- gsub("hː", "h-h", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ɬː", "ɬ-ɬ", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("xː", "x-x", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("fː", "f-f", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("çː", "ç-ç", full_data$Vremoved_target)


full_data <- full_data %>% mutate(nsyl_actual = stringr::str_count(Vremoved_actual, "V"),
                                                          nsyl_actual = ifelse(nsyl_actual == 0, 1, nsyl_actual))

#checks <- as.data.frame(unique(French$Vremoved_actual))

full_data <- full_data %>% mutate(TargetCV = str_replace_all(str_replace_all(IPAtarget, 
                                                                       "[e a ɑ u ə ɔ ɛ o i ø y ɥ œ æ ɤ]", "V"), "[^V]", "C"),
                                TargetCV = as.factor(TargetCV))

# target_structures_sample <- as.data.frame(levels(full_data$TargetCV)) # list all structures in the data
# 
# target_structures_sample <- target_structures_sample %>%
#   rename("TargetCV" = `levels(full_data$TargetCV)`)
# 
# target_structures_sample$TargetCV_edited <- gsub("ː", "", target_structures_sample$TargetCV)
# target_structures_sample$TargetCV_edited <- gsub("VVVV", "V", target_structures_sample$TargetCV_edited)  
# target_structures_sample$TargetCV_edited <- gsub("VVV", "V", target_structures_sample$TargetCV_edited)  
# target_structures_sample$TargetCV_edited <- gsub("VV", "V", target_structures_sample$TargetCV_edited)
# target_structures_sample$TargetCV_edited <- gsub("[(G g)]", "C", target_structures_sample$TargetCV_edited)  # counting glides as consonants, consistent with above
# target_structures_sample$TargetCV_edited <- gsub("CCCC", "C", target_structures_sample$TargetCV_edited)  
# target_structures_sample$TargetCV_edited <- gsub("CCC", "C", target_structures_sample$TargetCV_edited)  
# target_structures_sample$TargetCV_edited <- gsub("CC", "C", target_structures_sample$TargetCV_edited)  
# target_structures_sample$TargetCV_edited <- gsub("^", "", target_structures_sample$TargetCV_edited)
# 
# 
# target_structures_sample <- target_structures_sample %>%
#   mutate(TargetCV_edited = as.factor(TargetCV_edited))

full_data <- full_data %>% mutate(ActualCV = str_replace_all(str_replace_all(IPAactual, 
                                                                       "[ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə æ ɜ ʉ ɨ œ ɒ ɤ ɵ ʊ ε ɚ ɶ]",
                                                                       "V"), "[^V]", "C"),
                            ActualCV = as.factor(ActualCV))

actual_structures_sample <- as.data.frame(levels(full_data$ActualCV)) # list all structures in the data

actual_structures_sample <- actual_structures_sample %>%
  rename("ActualCV" = `levels(full_data$ActualCV)`)

actual_structures_sample$ActualCV_edited <- gsub("ː", "", actual_structures_sample$ActualCV)
actual_structures_sample$ActualCV_edited <- gsub("VVVV", "V", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("VVV", "V", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("VV", "V", actual_structures_sample$ActualCV_edited)
actual_structures_sample$ActualCV_edited <- gsub("[(G g)]", "C", actual_structures_sample$ActualCV_edited)  # counting glides as consonants, consistent with above
actual_structures_sample$ActualCV_edited <- gsub("CCCC", "C", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("CCC", "C", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("CC", "C", actual_structures_sample$ActualCV_edited)  
actual_structures_sample$ActualCV_edited <- gsub("^", "", actual_structures_sample$ActualCV_edited)


actual_structures_sample <- actual_structures_sample %>%
  mutate(ActualCV_edited = as.factor(ActualCV_edited))

full_data <- full_data %>% #left_join(target_structures_sample) %>%
  left_join(actual_structures_sample)  # join with main dataframe

full_data %>% group_by(nsyl_actual) %>% tally()  # some 10-syl words which are vocal play - remove anything above 5 syls (apareil photo)

#full_data <- full_data %>% filter(nsyl_actual <6)  ## might want to change this later to include all words

### split the syllables for alignment

## separating the geminates from the non-geminates - creating new variable to identify 
## geminate and non-geminate rows

full_data$geminate_T <- as.numeric(grepl("-", full_data$Vremoved_target))
full_data$geminate_A <- as.numeric(grepl("-", full_data$Vremoved_actual))

####################################################################################

### worry about this bit later (and run script through target forms, which will need writing) 
### if we decide to use target forms 

#full_data <- full_data %>% mutate(nsyl_actual = ifelse(geminate_A == 1, nsyl_actual+1, nsyl_actual))

## start with target non-geminates and non-complex clusters (which need dealing with separately)

## complex_clusters <- full_data %>% dplyr::filter(grepl("=", IPAtarget))

#######################################################################################

Vinitial <- full_data %>% filter(stringr::str_detect(ActualCV, "^V")) # DF for looking at V-intial structures only
Cinitial <- full_data %>% filter(stringr::str_detect(ActualCV, "^C")|stringr::str_detect(ActualCV, "^G"))    # DF for looking at C-intial structures only

nsyl_actual_list_nogem <- full_data %>%
  filter(geminate_A == 0) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_nogem <- lapply(nsyl_actual_list_nogem, FUN = function(element) {
  split_syl_Cinit <- element %>% filter(ActualCV %in% Cinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual",
                                "S6C1_actual", "S7C1_actual"), "V")
  split_sylCinit2 <- split_syl_Cinit %>%
    mutate(SFC1_actual = ifelse(nsyl_actual == 1 & !is.na(S2C1_actual), S2C1_actual, 0),     # create a category that is just codas
           S2C1_actual = ifelse(nsyl_actual == 1 & !is.na(SFC1_actual), 0, S2C1_actual),     # codas will always be aligned with codas
           SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(S3C1_actual), S3C1_actual, SFC1_actual),
           S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFC1_actual), 0, S3C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 3 & !is.na(S4C1_actual), S4C1_actual, SFC1_actual),
           S4C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFC1_actual), 0, S4C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 4 & !is.na(S5C1_actual), S5C1_actual, SFC1_actual),
           S5C1_actual = ifelse(nsyl_actual == 4 & !is.na(SFC1_actual), 0, S5C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 5 & !is.na(S6C1_actual), S6C1_actual, SFC1_actual),
           S6C1_actual = ifelse(nsyl_actual == 5 & !is.na(SFC1_actual), 0, S6C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 6 & !is.na(S7C1_actual), S7C1_actual, SFC1_actual),
           S7C1_actual = ifelse(nsyl_actual == 6 & !is.na(SFC1_actual), 0, S7C1_actual))
  split_clust_Cinit_final <- split_sylCinit2 %>% separate(S1C1_actual, 
                                                          c("S1C1", "S1C2", "S1C3", "S1C4"), 
                                                          sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("S4C1", "S4C2", "S4C3", "S4C4"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("S5C1", "S5C2", "S5C3", "S5C4"), sep = "(?<=.)") %>%
    separate(S6C1_actual, c("S6C1", "S6C2", "S6C3", "S6C4"), sep = "(?<=.)") %>%
    separate(S7C1_actual, c("S7C1", "S7C2", "S7C3", "S7C4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)")
  split_syl_Vinit <- element %>% filter(ActualCV %in% Vinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual", "S5C1_actual",
                                "S6C1_actual", "S7C1_actual"), "V")
  split_sylVinit2 <- split_syl_Vinit %>%
    mutate(SFC1_actual = ifelse(nsyl_actual == 1 & !is.na(S2C1_actual), S2C1_actual, 0),     # create a category that is just codas
           S2C1_actual = ifelse(nsyl_actual == 1 & !is.na(SFC1_actual), 0, S2C1_actual),     # codas will always be aligned with codas
           SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(S3C1_actual), S3C1_actual, SFC1_actual),
           S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFC1_actual), 0, S3C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 3 & !is.na(S4C1_actual), S4C1_actual, SFC1_actual),
           S4C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFC1_actual), 0, S4C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 4 & !is.na(S5C1_actual), S5C1_actual, SFC1_actual),
           S5C1_actual = ifelse(nsyl_actual == 4 & !is.na(SFC1_actual), 0, S5C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 5 & !is.na(S6C1_actual), S6C1_actual, SFC1_actual),
           S6C1_actual = ifelse(nsyl_actual == 5 & !is.na(SFC1_actual), 0, S6C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 6 & !is.na(S7C1_actual), S7C1_actual, SFC1_actual),
           S7C1_actual = ifelse(nsyl_actual == 6 & !is.na(SFC1_actual), 0, S7C1_actual))
  split_clust_Vinit_final <- split_sylVinit2 %>% separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("S4C1", "S4C2", "S4C3", "S4C4"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("S5C1", "S5C2", "S5C3", "S5C4"), sep = "(?<=.)") %>%
    separate(S6C1_actual, c("S6C1", "S6C2", "S6C3", "S6C4"), sep = "(?<=.)") %>%
    separate(S7C1_actual, c("S7C1", "S7C2", "S7C3", "S7C4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)")
  sample_IPA_CVinit <- rbind(split_clust_Vinit_final, split_clust_Cinit_final)
})

actual_list_base_nogem <- do.call(rbind.data.frame, loop_actual_base_nogem) %>%
  dplyr::select(-TargetCV, -ActualCV, -ActualCV_edited, -IPAtarget, -Vremoved_target) %>%
  mutate(
    S1CF1 = "",
    S1CF2 = "",
    S1CF3 = "",
    S1CF4 = "",
    S2CF1 = "",
    S2CF2 = "",
    S2CF3 = "",
    S2CF4 = "",
    S3CF1 = "",
    S3CF2 = "",
    S3CF3 = "",
    S3CF4 = "",
    SFCF1 = "",
    SFCF2 = "",
    SFCF3 = "",
    SFCF4 = "",
    geminate_A = F
  )


nsyl_actual_list_gem <- full_data %>%
  filter(geminate_A == 1) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_gem <- lapply(nsyl_actual_list_gem, FUN = function(element) {
  split_syl_Cinit <- element %>% filter(ActualCV %in% Cinitial$ActualCV) %>%
    separate(Vremoved_actual, c("seg1", "seg2", "seg3", "seg4", "seg5"), "V")
   split_sylCinit2 <- split_syl_Cinit %>%
     rename("SF_actual" = "seg5") %>%
     mutate(SF_actual = ifelse((is.na(SF_actual)|SF_actual == "") & !is.na(seg4),
                                 seg4, SF_actual),
            S4C1_actual = ifelse(nsyl_actual == 5, seg4, NA)) %>%
     dplyr::select(-seg4) %>%
     separate(seg3, c("seg3a", "seg3b"), "-") %>%
     mutate(S3C1_actual = ifelse(nsyl_actual == 4 & !is.na(SF_actual), seg3b, NA),
            SF_actual = ifelse(nsyl_actual > 1 & is.na(SF_actual)|SF_actual == "", seg3b, SF_actual)) %>%
     dplyr::select(-seg3b) %>%
     mutate(SXCF_actual = ifelse(nsyl_actual < 5 & !is.na(SF_actual), seg3a, NA),
            S4C1_actual = ifelse(nsyl_actual == 5 & is.na(S3C1_actual), seg3a, S4C1_actual),
            SF_actual = ifelse(!is.na(seg3a) & is.na(SF_actual), seg3a, SF_actual)) %>%
     dplyr::select(-seg3a) %>%
     separate(seg2, c("seg2a", "seg2b"), "-") %>%
     mutate(SF_actual = ifelse(nsyl_actual == 1 &
                                   (is.na(SF_actual)|SF_actual == ""), seg2b, SF_actual),
            SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(seg2b), seg2b, NA),
            S2C1_actual = ifelse(nsyl_actual == 3, seg2b, NA)) %>%
     dplyr::select(-seg2b) %>%
     mutate(S1CF_actual = ifelse(nsyl_actual == 1 &
                                   (!is.na(SF_actual)) & (!grepl("-", seg1)), seg2a, NA),
            SF_actual = ifelse(nsyl_actual == 1 & is.na(SF_actual), seg2a, SF_actual),
            ## individual case that prob needs removing:
            S2C1_actual = ifelse(nsyl_actual == 1 & grepl("βʔβʔβ", seg2a), seg2a, S2C1_actual),
            SFC1_actual = ifelse(nsyl_actual == 2 & is.na(SFC1_actual) &
                                   (is.na(SF_actual)|SF_actual == ""), seg2a, SFC1_actual),
            S2C1_actual = ifelse(nsyl_actual == 2 & !is.na(SXCF_actual), seg2a, S2C1_actual),
            S1CF_actual = ifelse(nsyl_actual == 2 & (!grepl("-", seg1)) &
                                   is.na(SXCF_actual), seg2a, S1CF_actual),
            # now need to sort some specific tokens that don't pattern with others
            SFC1_actual = ifelse(nsyl_actual == 2 & seg1 == "m-m" & seg2a == "ʔ", "ʔ", SFC1_actual),
            SXCF_actual = ifelse(nsyl_actual == 2 & seg1 == "m-m" & seg2a == "m", "m", SXCF_actual),
            S3C1_actual = ifelse(nsyl_actual == 3 & seg1 == "n-n", seg2a, S3C1_actual),
            S1CF_actual = ifelse(nsyl_actual == 3 & !is.na(S2C1_actual), seg2a, S1CF_actual),
            S2C1_actual = ifelse(nsyl_actual == 3 & !is.na(SXCF_actual) &
                                   is.na(S2C1_actual), seg2a, S2C1_actual),
            S2C1_actual = ifelse(nsyl_actual == 4 & is.na(S2C1_actual), seg2a, S2C1_actual)) %>%
     dplyr::select(-seg2a) %>%
     separate(seg1, c("S1C1_actual", "seg1b"), "-") %>%
     mutate(SFC1_actual = ifelse(nsyl_actual == 1 & !is.na(SF_actual), seg1b, SFC1_actual),
            SF_actual = ifelse(nsyl_actual == 1 & is.na(SF_actual), seg1b, SF_actual),
            S2C1_actual = ifelse((nsyl_actual == 2|nsyl_actual == 3) & is.na(S2C1_actual), seg1b, S2C1_actual),
            S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(S2C1_actual), seg1b, S3C1_actual)) %>%
     dplyr::select(-seg1b) %>%
     #fixing some errors:
     mutate(S3C1_actual = ifelse(Gloss == "money" & IPAactual == "m:ʌm:ɪ", NA, S3C1_actual),
            S3CF_actual = ifelse(SXCF_actual == "β" & nsyl_actual == 2, "β", NA),
            SXCF_actual = ifelse(SXCF_actual == "β" & nsyl_actual == 2, NA, SXCF_actual),
            S2CF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 2, SXCF_actual, NA),
            SXCF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 2, NA, SXCF_actual),
            S3CF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 3, SXCF_actual, S3CF_actual),
            SXCF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 3, NA, SXCF_actual),
            S2CF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 4, SXCF_actual, S2CF_actual),
            SXCF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 4, NA, SXCF_actual)) %>%
     dplyr::select(-SXCF_actual) #%>%
     ## now sort nsyls so it's accurate ## update: ignoring, it's complicated and may not be needed
     # mutate(nsyl_actual = ifelse(nsyl_actual == 1, 2, nsyl_actual),
     #        nsyl_actual = ifelse(nsyl_actual == 2 & !is.na(S2C1_actual) & is.na(S3C1_actual), 3, nsyl_actual),
     #        nsyl_actual = ifelse(nsyl_actual == 3 & !is.na(S3C1_actual) & is.na(S4C1_actual), 4, nsyl_actual),
     #        nsyl_actual = ifelse(nsyl_actual == 4 & !is.na(S4C1_actual), 5, nsyl_actual))
   split_clust_Cinit_final <- split_sylCinit2 %>% 
     separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
     separate(S1CF_actual, c("S1CF1", "S1CF2", "S1CF3", "S1CF4"), sep = "(?<=.)") %>%
     separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
     separate(S2CF_actual, c("S2CF1", "S2CF2", "S2CF3", "S2CF4"), sep = "(?<=.)") %>%
     separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
     separate(S3CF_actual, c("S3CF1", "S3CF2", "S3CF3", "S3CF4"), sep = "(?<=.)") %>%
     separate(S4C1_actual, c("S4C1", "S4C2", "S4C3", "S4C4"), sep = "(?<=.)") %>%
     separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
     separate(SF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)")
   
   #### Now work on vowel-initial forms
   
  split_syl_Vinit <- element %>% filter(ActualCV %in% Vinitial$ActualCV) %>%
    separate(Vremoved_actual, c("seg1", "seg2", "seg3", "seg4", "seg5"), "V") %>%
    separate(seg2, c("seg2a", "seg2b", "seg2c"), "-", remove = F) %>%
    mutate(S1CF_actual = ifelse(grepl("-", seg2), seg2a, NA),
           S2C1_actual = ifelse(!grepl("-", seg2), seg2a, NA),
           S2C1_actual = ifelse(grepl("-", seg2), seg2b, S2C1_actual),
           SFC1_actual = ifelse(!is.na(seg2c), seg2c, NA)) %>%
    dplyr::select(-seg1, -seg2, -seg2a, -seg2b, -seg2c) %>%
    separate(seg3, c("seg3a", "seg3b"), "-", remove = F) %>%
    mutate(
            S3C1_actual = ifelse(nsyl_actual ==4 & is.na(seg3b), seg3a, NA),
            S3C1_actual = ifelse(nsyl_actual ==4 & !is.na(seg3b), seg3b, S3C1_actual),
            SFC1_actual = ifelse(nsyl_actual ==3 & !is.na(seg3b), seg3b, SFC1_actual),
            S3C1_actual = ifelse(nsyl_actual ==3 & grepl("-", seg4), seg3a, S3C1_actual),
            S2CF_actual = ifelse(grepl("-", seg3), seg3a, NA),
            SFC1_actual = ifelse(nsyl_actual == 3 & (!is.na(seg4)|seg4 !=""), seg3a, SFC1_actual)) %>%
    dplyr::select(-seg3, -seg3a, -seg3b) %>%
    separate(seg4, c("seg4a", "seg4b"), "-", remove = F) %>%
    mutate(SFC1_actual = ifelse(seg5 == "" & is.na(seg4b) & is.na(SFC1_actual), seg4a, SFC1_actual),
         SFCF_actual = ifelse(!is.na(seg4b), seg4b, NA),
         SFCF_actual = ifelse(is.na(seg5) & is.na(seg4b) & is.na(SFCF_actual), seg4a, SFCF_actual)) %>%
   dplyr::select(-seg4, -seg4a, -seg4b, -seg5)
  split_sylVinit2 <- split_syl_Vinit %>%
      mutate(
             nsyl_actual = ifelse(nsyl_actual == 1, 2, nsyl_actual),
             ## adding data manually due to idiosyncratic token
             S3CF_actual = ifelse(ActualCV == "VCVCCVCCC", "s", NA),
             SFC1_actual = ifelse(ActualCV == "VCVCCVCCC", NA, SFC1_actual),
             nsyl_actual = ifelse(ActualCV == "VCVCCVCCC", 4, nsyl_actual))
  split_clust_Vinit_final <- split_sylVinit2 %>%
    separate(S1CF_actual, c("S1CF1", "S1CF2", "S1CF3", "S1CF4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(S2CF_actual, c("S2CF1", "S2CF2", "S2CF3", "S2CF4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
    separate(S3CF_actual, c("S3CF1", "S3CF2", "S3CF3", "S3CF4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
    separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)") %>%
    mutate(S1C1 = "",
           S1C2 = "",
           S1C3 = "",
           S1C4 = "",
           S4C1 = "",
           S4C2 = "",
           S4C3 = "",
           S4C4 = "")
    sample_IPA_CVinit <- rbind(split_clust_Vinit_final, split_clust_Cinit_final)
})

#colnames.c<- colnames(actual_list_base_gem)

actual_list_base_gem <- do.call(rbind.data.frame, loop_actual_base_gem) %>% 
  dplyr::select(-TargetCV, -ActualCV, -ActualCV_edited, -IPAtarget, -Vremoved_target) %>%
  mutate(
    S5C1 = "",
    S5C2 = "",
    S5C3 = "",
    S5C4 = "",
    S6C1 = "",
    S6C2 = "",
    S6C3 = "",
    S6C4 = "",
    S7C1 = "",
    S7C2 = "",
    S7C3 = "",
    S7C4 = "",
    geminate_A = T
  )

all_data_sample <- rbind(actual_list_base_nogem, actual_list_base_gem) %>%
  mutate(across(everything(), ~replace(., . %in% c(" ", "", 0), NA))) %>%
  tibble::rowid_to_column("ID")

all_data_sample_final <- all_data_sample %>% dplyr::select(ID, 
                                                           Subject,
                                                           Age,
                                                           Gloss,
                                                           geminate_A,
                                                           IPAactual,
                                                           nsyl_actual
                                                           )

write_csv(all_data_sample_final, "all_data_sample_final.csv")


## adding anterior and long features here to accommodate retroflex and geminate segments in this dataset
distinctive.feature.matrix <- tribble(~Symbol, ~Sonorant, ~Consonantal, ~Voice, ~Nasal, ~Degree, ~Labial, 
                                      ~Palatal, ~Pharyngeal, ~Round, ~Tongue, ~Radical, ~Anterior,
                                      "p", -1, 1, -1, -1, 1, 1, 0, -1, 1, 0, 0, -1,
                                      "b", -1, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0, -1,
                                      "t", -1, 1, -1, -1, 1, -1, 1, -1, -1, 1, 0, -1,
                                      "d", -1, 1, 0, -1, 1, -1, 1, -1, -1, 1, 0, -1,
                                      "k", -1, 1, -1, -1, 1, -1, -1, -1, -1, -1, 0, -1,
                                      "ɡ", -1, 1, 0, -1, 1, -1, -1, -1, -1, -1, 0, -1,
                                      "f", -0.5, 1, -1, -1, 0, -1, 1, -1, 1, 0, 0, -1,
                                      "v", -0.5, 1, 0, -1, 0, -1, 1, -1, 1, 0, 0, -1,
                                      "θ", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 0, 0, -1,
                                      "ð", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 0, 0, -1,
                                      "s", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 1, 0, -1,
                                      "c", -1, 1, 0, -1, 0, -1, 1, -1, -1, -1, 0, -1, 
                                      "z", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 1, 0, -1,
                                      "h", -0.5, 1, -1, -1, 0, -1, -1, 1, -1, -1, -1, -1,
                                      "ʃ", -0.5, 1, -1, -1, 0, -1, 0, -1, -1, 0, 0, -1,
                                      "ʒ", -0.5, 1, 0, -1, 0, -1, 0, -1, -1, 0, 0, -1,
                                      "ʧ", -0.8, 1, -1, -1, 1, -1, 0, -1, -1, 0, 0, -1,
                                      "ʤ", -0.8, 1, 0, -1, 1, -1, 0, -1, -1, 0, 0, -1,
                                      "m", 0, 0, 1, 1, 1, 1, 0, -1, 1, 0, 0, -1,
                                      "n", 0, 0, 1, 1, 1, -1, 1, -1, -1, 1, 0, -1,
                                      "ŋ", 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, 0, -1,
                                      "l", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0, -1,
                                      "w", 0.8, 0, 1, 0, 0, 1, -1, -1, 1, -1, 0, -1,
                                      "j", 0.8, 0, 1, 0, 0, -1, 0, -1, -1, 0, 1, -1,
                                      "ɾ", 0.5, 1, 1, 0, -1, -1, -1, 1, -1, 1, 0, -1,
                                      "ʙ", -0.5, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0, -1,
                                      "ʀ", -0.5, 1, 1, -1, -1, -1, -1, -1, -1, 0, -1, -1,
                                      "ɲ", 0, 1, 1, 1, 1, -1, 1, -1, -1, 1, 0, -1,
                                      "r",  0.5, 1, 1, -1, -1, -1, -1, -1, 0, 0, -1, -1,
                                      "ɣ", -0.5 , 1, 1, 0,  0, -1, -1, -1, 0, 0, -1, -1,
                                      "ɱ", 0, 1, 1, 1,  1, 1, -1, -1, -1, 0, 0, -1,
                                      "x", -0.5, 1, -1, 0,  0, -1, -1, -1, 0, 0, -1, -1,
                                      "ʁ", -0.5, 1, -1, 0,  0, -1, -1, 1, 0, 0, -1, -1,
                                      "ç", -0.5, 1, -1, 0,  0, -1, 1, -1, 0, 0, -1, -1,
                                      # added manually as not defined in original. 
                                      #Drew from Cambridge Handbook of Phonology and
                                      # similarities with /h/
                                      "ʔ", -1, 0, 0, -1, 0, -1, -1, 1, -1, 1, 0, -1,
                                      # added manually as not defined in original. 
                                      #Drew from Phoible and
                                      # similarities with /j/
                                      "ɥ", 0.8, 0, 1, 0, 0, 1, 0, -1, -1, 0, 1, -1,
                                      ## new ones for chapter
                                      "β", -0.5, 1, 0, -1, 0, 1, 1, -1, 1, 0, 0, -1,
                                      "ɣ",  -0.5, 1, 1, 0,  0, -1, -1, -1, 0, 0, -1, -1,
                                      "ʨ", -0.8, 1, -1, -1, 1, -1, 1, -1, -1, 0, 0, -1,
                                      "ʥ", -0.8, 1, 1, -1, 1, -1, 1, -1, -1, 0, 0, -1,
                                      "ɦ", -0.5, 1, 0, -1, 0, -1, -1, 1, -1, -1, -1, -1,
                                      "ʎ", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 0, 1, -1,
                                      "ɟ",  -1, 1, 1, -1, 0, -1, 1, -1, -1, -1, 0, -1,
                                      "ɭ", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0, 1,
                                      "χ", -0.5, 1, 1, 0,  0, -1, -1, 1, 0, 0, -1, -1,
                                      "ɖ", -1, 1, 0, -1, 1, -1, 1, -1, -1, 1, 0, 1,
                                      "ɸ", -0.5, 1, -1, -1, 0, 1, 1, -1, 1, 0, 0, -1,
                                      "ɽ", 0.5, 1, 1, 0, -1, -1, -1, 1, -1, 1, 0, 1,
                                      ## /l/ above is also palatalised so keeping this the same
                                      "ɫ", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0, -1,
                                      "ɓ", -1, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0, -1,
                                      "ɕ", -0.5, 1, -1, 0,  0, -1, 1, -1, 0, 1, -1, -1,
                                      "ɢ", -1, 1, 0, -1, 0, -1, 1, 1, -1, -1, 0, -1,
                                      "ɬ", -0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0, -1,
                                      "ɳ", 0, 0, 1, 1, 1, -1, 1, -1, -1, 1, 0, 1,
                                      "ʐ", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 1, 0, 1,
                                      "ɴ", 0, 0, 1, 1, 1, -1, 1, 1, -1, 1, 0, -1,
                                      "ʑ", -0.5, 1, 0, 0,  0, -1, 1, -1, 0, 1, -1, -1,
                                      "ʂ", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 1, 0, 1,
                                      #"ƫ", ??????? also see strange greek symbol
                                      "ɭ", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0, 1,
                                      "ȶ", -1, 1, -1, 0, 1, -1, 1, -1, 0, 1, -1, -1
                                      )    


####### Probably don't need ########################################

# colnames_target <- actual_target_French %>% filter(data_type == "target") %>% dplyr::select(ID, starts_with("S"), -Subject)
# #colnames(colnames_target) <- sub("T","",colnames(colnames_target))
# target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1], 
#                                                                              colnames_target[x])), 
#                         names(colnames_target)[-1])
# 
# output_target <- lapply(target_list, FUN = function(element) {
#   target_segment <- data.frame(element,
#                                distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:14], 
#                                stringsAsFactors=FALSE) %>%
#     replace(is.na(.), 0)
# })
# 
# output_target_df <- as.data.frame(output_target)
# colnames(output_target_df)[1] <- "unique"
# 
# output_target_df <- output_target_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
#    rename("ID" = "unique") %>%
#   left_join(French_comparison_final)

##################################################

colnames_sample <- all_data_sample %>% dplyr::select(ID, starts_with("S"), -Subject)

sample_list <- setNames(lapply(names(colnames_sample)[-1], function(x) cbind(colnames_sample[1], 
                                                                             colnames_sample[x])), 
                        names(colnames_sample)[-1])

output_sample <- lapply(sample_list, FUN = function(element) {
  sample_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], 
                                                                distinctive.feature.matrix$Symbol), 2:13], 
                               stringsAsFactors=FALSE)  %>%
    replace(is.na(.), 0)
})

output_df <- as.data.frame(output_sample)

colnames(output_df)[1] <- "unique"
output_df <- output_df %>% #dplyr::select(unique, -ends_with(".ID")) %>%
  rename("ID" = "unique") %>%
  left_join(all_data_sample_final)

write_csv(output_df, "output_df.csv")
