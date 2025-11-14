library(arsenal)
library(tidyverse)
options("encoding" = "UTF-8")


full_data <- read_csv("data_ready.csv") 


# remove vowels

# substitute all target vowels for generic V because I don't care about vowels
full_data$Vremoved_target <- gsub("([ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə æ ɜ ʉ ɨ œ ɒ ɤ ɵ ʊ ε ɚ ɶ ɯ ʏ ı  ])", "V", full_data$IPAtarget) 

full_data$Vremoved_target <- gsub("ʁ", "R", full_data$Vremoved_target) # code won't run properly with /ʁ/ so change to /R/
full_data$Vremoved_target <- gsub("V::", "V", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("V:::", "V", full_data$Vremoved_target) 
full_data$Vremoved_target <- gsub("Vːː", "V", full_data$Vremoved_target)  
full_data$Vremoved_target <- gsub("Vː", "V", full_data$Vremoved_target)  
full_data$Vremoved_target <- gsub("V:", "V", full_data$Vremoved_target)  
full_data$Vremoved_target <- gsub("VVV", "V", full_data$Vremoved_target)  
full_data$Vremoved_target <- gsub("VV", "V", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("Ṽ", "V", full_data$Vremoved_target) ## check this works
full_data$Vremoved_target <- gsub("ʷ", "w", full_data$Vremoved_target) ## check all the below
full_data$Vremoved_target <- gsub("ⁿ", "n", full_data$Vremoved_target)
full_data$Vremoved_target <- gsub("ʲ", "j", full_data$Vremoved_target) ## check this with Marilyn
full_data$Vremoved_target <- gsub("ʰ", "", full_data$Vremoved_target)  ## just remove aspiration?
full_data$Vremoved_target <- gsub("t¸", "t", full_data$Vremoved_target)

full_data$Vremoved_target <- gsub("ʁ", "R", full_data$Vremoved_target) # code won't run properly with /ʁ/ so change to /R/
full_data$Vremoved_target <- gsub("ʁ", "R", full_data$Vremoved_target) # code won't run properly with /ʁ/ so change to /R/
full_data$Vremoved_target <- gsub("Vː", "V", full_data$Vremoved_target)  

# substitute all actual vowels for generic V because I don't care about vowels here either

full_data$Vremoved_actual <- gsub("([ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə æ ɜ ʉ ɨ œ ɒ ɤ ɵ ʊ ε ɚ ɶ ʏ ı ɯ])", "V", 
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
full_data$Vremoved_actual <- gsub("ʷ", "w", full_data$Vremoved_actual) ## check all the below

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
full_data$Vremoved_actual <- gsub("ʒ:", "ʒ-ʒ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("θ:", "θ-θ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʎ:", "ʎ-ʎ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("w:", "w-w", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʨ:", "ʨ-ɕ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("ʤ:", "ʤ-ʒ", full_data$Vremoved_actual)
full_data$Vremoved_actual <- gsub("c:", "c-c", full_data$Vremoved_actual)


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
full_data$Vremoved_target <- gsub("ɕː", "ɕ-ɕ", full_data$Vremoved_target)

full_data <- full_data %>% mutate(nsyl_actual = stringr::str_count(Vremoved_actual, "V"),
                                                          nsyl_actual = ifelse(nsyl_actual == 0, 1, nsyl_actual))

#checks <- as.data.frame(unique(French$Vremoved_actual))

full_data <- full_data %>% mutate(TargetCV = str_replace_all(str_replace_all(IPAtarget,
                                                                             "[ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə æ ɜ ʉ ɨ œ ɒ ɤ ɵ ʊ ε ɚ  ɯ ɶ]",
                                                                             "V"), "[^V]", "C"),
                                TargetCV = as.factor(TargetCV))

target_structures_sample <- as.data.frame(levels(full_data$TargetCV)) # list all structures in the data

target_structures_sample <- target_structures_sample %>%
  rename("TargetCV" = `levels(full_data$TargetCV)`)

target_structures_sample$TargetCV_edited <- gsub("ː", "", target_structures_sample$TargetCV)
target_structures_sample$TargetCV_edited <- gsub("VVVV", "V", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("VVV", "V", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("VV", "V", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("[(G g)]", "C", target_structures_sample$TargetCV_edited)  # counting glides as consonants, consistent with above
target_structures_sample$TargetCV_edited <- gsub("CCCC", "C", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("CCC", "C", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("CC", "C", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("^", "", target_structures_sample$TargetCV_edited)


target_structures_sample <- target_structures_sample %>%
  mutate(TargetCV_edited = as.factor(TargetCV_edited))

full_data <- full_data %>% mutate(ActualCV = str_replace_all(str_replace_all(IPAactual, 
                                                                       "[ø e a y ʌ ɛ o ɥ u i ɔ ɑ ɪ ə æ ɜ ʉ ɨ œ ɒ ɤ ɵ ʊ ε ɚ  ɯ ɶ]",
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

full_data <- full_data %>% left_join(target_structures_sample) %>%
  left_join(actual_structures_sample)  # join with main dataframe

full_data %>% group_by(nsyl_actual) %>% tally()
### split the syllables for alignment

## separating the geminates from the non-geminates - creating new variable to identify 
## geminate and non-geminate rows

full_data$geminate_T <- as.numeric(grepl("-", full_data$Vremoved_target))
full_data$geminate_A <- as.numeric(grepl("-", full_data$Vremoved_actual))

full_data_disyls <- full_data %>% filter(nsyl_target == 2)
full_data_disyls %>% group_by(nsyl_actual) %>% tally() ## 2 instances of 5syl productions, most 1-3

####################################################################################

## Target forms: split between simple and complex structures; no geminates in complex structures

full_data_disyls <- full_data_disyls %>% mutate(complex = grepl("=", IPAtarget))

nsyl_target_list_complex_Cinit <- full_data_disyls %>%
  filter(complex == T) %>%
  filter(str_detect(TargetCV_edited, '^C')) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_complex_Cinit <- lapply(nsyl_target_list_complex_Cinit, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1_target", "S2C1_target"), "=") %>%
    separate(S1C1_target, c("S1C1_target", "S1CF_target"), "V") %>%
    separate(S2C1_target, c("SFC1_target", "SFCF_target"), "V")
  split_clust <- split_syl %>% tidyr::separate(S1C1_target, c("S1C1", "S1C2", "S1C3"), sep = "(?<=.)") %>%
    tidyr::separate(S1CF_target, c("S1CF1", "S1CF2", "S1CF3"), sep = "(?<=.)") %>%
    tidyr::separate(SFC1_target, c("SFC1", "SFC2", "SFC3"), sep = "(?<=.)") %>%
    tidyr::separate(SFCF_target, c("SFCF1", "SFCF2", "SFCF3"), sep = "(?<=.)")
})

target_list_complex_Cinit <- do.call(rbind.data.frame, sample_IPAtarget_loop_complex_Cinit) 


nsyl_target_list_complex_Vinit <- full_data_disyls %>%
  filter(complex == T) %>%
  filter(str_detect(TargetCV_edited, '^V')) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_complex_Vinit <- lapply(nsyl_target_list_complex_Vinit, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1_target", "S2C1_target"), "=") %>%
     separate(S1C1_target, c("S1C1_target", "S1CF_target"), "V") %>%
     separate(S2C1_target, c("SFC1_target", "SFCF_target"), "V")
  split_clust <- split_syl %>% tidyr::separate(S1C1_target, c("S1C1", "S1C2", "S1C3"), sep = "(?<=.)") %>%
      tidyr::separate(S1CF_target, c("S1CF1", "S1CF2", "S1CF3"), sep = "(?<=.)") %>%
      tidyr::separate(SFC1_target, c("SFC1", "SFC2", "SFC3"), sep = "(?<=.)") %>%
      tidyr::separate(SFCF_target, c("SFCF1", "SFCF2", "SFCF3"), sep = "(?<=.)")
  })

target_list_complex_Vinit <- do.call(rbind.data.frame, sample_IPAtarget_loop_complex_Vinit) 

target_list_complex <- rbind(target_list_complex_Cinit, target_list_complex_Vinit)

### non-complex clusters (with and without geminates):

## without geminates

nsyl_target_list_Cinit <- full_data_disyls %>%
  filter(complex == F & geminate_T == 0) %>%
  filter(str_detect(TargetCV_edited, '^C')) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_Cinit <- lapply(nsyl_target_list_Cinit, FUN = function(element) {
  split_syl <- element %>% tidyr::separate(Vremoved_target, c("S1C1_target", "SFC1_target", "SFCF_target"), "V")
  split_clust <- split_syl %>% tidyr::separate(S1C1_target, c("S1C1", "S1C2", "S1C3"), sep = "(?<=.)") %>%
     tidyr::separate(SFC1_target, c("SFC1", "SFC2", "SFC3"), sep = "(?<=.)") %>%
     tidyr::separate(SFCF_target, c("SFCF1", "SFCF2", "SFCF3"), sep = "(?<=.)")
})

target_list_Cinit <- do.call(rbind.data.frame, sample_IPAtarget_loop_Cinit) 

nsyl_target_list_Vinit <- full_data_disyls %>%
  filter(complex == F & geminate_T == 0) %>%
  filter(str_detect(TargetCV_edited, '^V')) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_Vinit <- lapply(nsyl_target_list_Vinit, FUN = function(element) {
  split_syl <- element %>% tidyr::separate(Vremoved_target, c("S1C1_target", "SFC1_target", "SFCF_target"), "V")
  split_clust <- split_syl %>% tidyr::separate(S1C1_target, c("S1C1", "S1C2", "S1C3"), sep = "(?<=.)") %>%
    tidyr::separate(SFC1_target, c("SFC1", "SFC2", "SFC3"), sep = "(?<=.)") %>%
    tidyr::separate(SFCF_target, c("SFCF1", "SFCF2", "SFCF3"), sep = "(?<=.)")
})

target_list_Vinit <- do.call(rbind.data.frame, sample_IPAtarget_loop_Vinit) 

target_list <- rbind(target_list_Cinit, target_list_Vinit) %>% mutate(S1CF1 = "",
                                                                      S1CF2 = "", 
                                                                      S1CF3 = "")

## with geminates

nsyl_target_list_gem_Cinit <- full_data_disyls %>%
  filter(complex == F & geminate_T == 1) %>%
  filter(str_detect(TargetCV_edited, '^C')) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_gem_Cinit <- lapply(nsyl_target_list_gem_Cinit, FUN = function(element) {
  split_syl <- element %>% tidyr::separate(Vremoved_target, c("S1C1_target", "S1CF_target", "SFCF_target"), "V") %>%
     tidyr::separate(S1CF_target, c("S1CF_target", "SFC1_target"), "-")
  split_clust <- split_syl %>% tidyr::separate(S1C1_target, c("S1C1", "S1C2", "S1C3"), sep = "(?<=.)") %>%
      tidyr::separate(S1CF_target, c("S1CF1", "S1CF2", "S1CF3"), sep = "(?<=.)") %>%
      tidyr::separate(SFC1_target, c("SFC1", "SFC2", "SFC3"), sep = "(?<=.)") %>%
      tidyr::separate(SFCF_target, c("SFCF1", "SFCF2", "SFCF3"), sep = "(?<=.)")
})

target_list_gem_Cinit <- do.call(rbind.data.frame, sample_IPAtarget_loop_gem_Cinit)

nsyl_target_list_gem_Vinit <- full_data_disyls %>%
  filter(complex == F & geminate_T == 1) %>%
  filter(str_detect(TargetCV_edited, '^V')) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_gem_Vinit <- lapply(nsyl_target_list_gem_Vinit, FUN = function(element) {
  split_syl <- element %>% tidyr::separate(Vremoved_target, c("S1C1_target", "S1CF_target", "SFCF_target"), "V") %>%
       tidyr::separate(S1CF_target, c("S1CF_target", "SFC1_target"), "-")
  split_clust <- split_syl %>% tidyr::separate(S1C1_target, c("S1C1", "S1C2", "S1C3"), sep = "(?<=.)") %>%
    tidyr::separate(S1CF_target, c("S1CF1", "S1CF2", "S1CF3"), sep = "(?<=.)") %>%
    tidyr::separate(SFC1_target, c("SFC1", "SFC2", "SFC3"), sep = "(?<=.)") %>%
    tidyr::separate(SFCF_target, c("SFCF1", "SFCF2", "SFCF3"), sep = "(?<=.)")
})

target_list_gem_Vinit <- do.call(rbind.data.frame, sample_IPAtarget_loop_gem_Vinit)

target_list_gem <- rbind(target_list_gem_Cinit, target_list_gem_Vinit)

target_list_all <- rbind(target_list, target_list_complex, target_list_gem) %>% 
  mutate(data_type = "target",
         S1C4 = "",
         S1CF4 = "",
         S2C1 = "",
         S2C2 = "",
         S2C3 = "",
         S2C4 = "",
         S2CF1 = "",
         S2CF2 = "",
         S2CF3 = "",
         S2CF4 = "",
         S3C1 = "",
         S3C2 = "",
         S3C3 = "",
         S3C4 = "",
         S3CF1 = "",
         S3CF2 = "",
         S3CF3 = "",
         S3CF4 = "",
         S4C1 = "", 
         S4C2 = "", 
         S4C3 = "", 
         S4C4 = "", 
         S5C1 = "", 
         S5C2 = "", 
         S5C3 = "", 
         S5C4 = "", 
         S6C1 = "", 
         S6C2 = "", 
         S6C3 = "", 
         S6C4 = "",
         SFC4 = "",
         SFCF4 = "") %>% dplyr::select(-Vremoved_actual)

#######################################################################################

### Actual forms ###

nsyl_actual_list_nogem_Cinit <- full_data_disyls %>%
  filter(geminate_A == 0) %>%
  filter(str_detect(ActualCV_edited, '^C')) %>%
  mutate(final_seg = ifelse(grepl("V$", Vremoved_actual), "V", "C")) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_nogem_Cinit <- lapply(nsyl_actual_list_nogem_Cinit, FUN = function(element) {
  split_syl_Cinit <- element %>% separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", 
                                                             "S3C1_actual", "S4C1_actual", 
                                                             "S5C1_actual", "S6C1_actual"), "V")
  split_sylCinit2 <- split_syl_Cinit %>%
    mutate(S1CF_actual = ifelse(nsyl_actual == 1 & (!is.na(S2C1_actual)) &
                                  final_seg == "C", S2C1_actual, NA),
           SFCF_actual = ifelse(nsyl_actual == 2 & !is.na(S3C1_actual) &
                                  final_seg == "C", S3C1_actual, NA),
           SFC1_actual = ifelse(nsyl_actual == 2, S2C1_actual, NA),
           S2C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFC1_actual), NA, S2C1_actual),
           S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFCF_actual), NA, S3C1_actual),
           SFCF_actual = ifelse(nsyl_actual == 3 & !is.na(S4C1_actual) & final_seg == "C",
                                S4C1_actual, SFCF_actual),
           SFC1_actual = ifelse(nsyl_actual == 3, S3C1_actual, SFC1_actual),
           S4C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFCF_actual), NA, S4C1_actual),
           S3C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFC1_actual), NA, S3C1_actual),
           SFCF_actual = ifelse(nsyl_actual == 4 & !is.na(S5C1_actual) & final_seg == "C",
                                S5C1_actual, SFCF_actual),
           SFC1_actual = ifelse(nsyl_actual == 4, S4C1_actual, SFC1_actual),
           S5C1_actual = ifelse(nsyl_actual == 4 & !is.na(SFCF_actual), NA, S5C1_actual),
           S4C1_actual = ifelse(nsyl_actual == 4 & !is.na(SFC1_actual), NA, S4C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 5 & final_seg == "V", S5C1_actual, SFC1_actual),
           S5C1_actual = ifelse(nsyl_actual == 5 & !is.na(SFC1_actual), NA, S5C1_actual))
  split_clust_Cinit_final <- split_sylCinit2 %>%
    separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
    separate(S1CF_actual, c("S1CF1", "S1CF2", "S1CF3", "S1CF4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("S4C1", "S4C2", "S4C3", "S4C4"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("S5C1", "S5C2", "S5C3", "S5C4"), sep = "(?<=.)") %>%
    separate(S6C1_actual, c("S6C1", "S6C2", "S6C3", "S6C4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
    separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)") %>%
    mutate(S1C3 = ifelse(!is.na(S1C4) & S1C3 == "", S1C4, S1C3),
           S1C4 = NA,
           SFCF3 = ifelse(!is.na(SFCF4) & SFCF3 == "", SFCF4, SFCF3),
           SFCF4 = NA,
           SFC3 = ifelse(!is.na(SFC4) & SFC3 == "", SFC4, SFC3),
           SFC4 = NA)
})

actual_nogem_Cinit <- do.call(rbind.data.frame, loop_actual_base_nogem_Cinit) 

### V-initial

nsyl_actual_list_nogem_Vinit <- full_data_disyls %>%
  filter(geminate_A == 0) %>%
  filter(str_detect(ActualCV_edited, '^V')) %>%
  mutate(final_seg = ifelse(grepl("V$", Vremoved_actual), "V", "C")) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_nogem_Vinit <- lapply(nsyl_actual_list_nogem_Vinit, FUN = function(element) {
  split_syl_Vinit <- element %>% separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", 
                                                             "S3C1_actual", "S4C1_actual", 
                                                             "S5C1_actual", "S6C1_actual"), "V")
  split_sylVinit2 <- split_syl_Vinit %>%
    mutate(SFCF_actual = ifelse(nsyl_actual == 1 & (!is.na(S2C1_actual)) &
                                  final_seg == "C", S2C1_actual, NA),
           SFC1_actual = ifelse(nsyl_actual == 1 & (!is.na(S2C1_actual)) &
                                  final_seg == "V", S2C1_actual, NA),
           SFCF_actual = ifelse(nsyl_actual == 2 & !is.na(S3C1_actual) &
                                  final_seg == "C", S3C1_actual, SFCF_actual),
           SFC1_actual = ifelse(nsyl_actual == 2 & (is.na(SFC1_actual)|SFC1_actual == "") &
                                  (is.na(S3C1_actual)|S3C1_actual == "") &
                         final_seg == "V", S2C1_actual, SFC1_actual),
           SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(SFCF_actual) & is.na(SFC1_actual),
                                S2C1_actual, SFC1_actual),
  S2C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFC1_actual), NA, S2C1_actual),
  S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFCF_actual), NA, S3C1_actual),
  SFCF_actual = ifelse(nsyl_actual == 3 & !is.na(S4C1_actual) & final_seg == "C",
                       S4C1_actual, SFCF_actual),
  SFC1_actual = ifelse(nsyl_actual == 3, S3C1_actual, SFC1_actual),
  S4C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFCF_actual), NA, S4C1_actual),
  S3C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFC1_actual), NA, S3C1_actual),
  SFCF_actual = ifelse(nsyl_actual == 4 & !is.na(S5C1_actual) & final_seg == "C",
                       S5C1_actual, SFCF_actual),
  SFC1_actual = ifelse(nsyl_actual == 4, S4C1_actual, SFC1_actual),
  S5C1_actual = ifelse(nsyl_actual == 4 & !is.na(SFCF_actual), NA, S5C1_actual),
  S4C1_actual = ifelse(nsyl_actual == 4 & !is.na(SFC1_actual), NA, S4C1_actual),
  SFC1_actual = ifelse(nsyl_actual == 5 & final_seg == "V", S5C1_actual, SFC1_actual),
  S5C1_actual = ifelse(nsyl_actual == 5 & !is.na(SFC1_actual), NA, S5C1_actual))
  split_clust_Vinit_final <- split_sylVinit2 %>% 
    separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
    separate(S4C1_actual, c("S4C1", "S4C2", "S4C3", "S4C4"), sep = "(?<=.)") %>%
    separate(S5C1_actual, c("S5C1", "S5C2", "S5C3", "S5C4"), sep = "(?<=.)") %>%
    separate(S6C1_actual, c("S6C1", "S6C2", "S6C3", "S6C4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
    separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)") %>%
    mutate(SFC3 = ifelse(!is.na(SFC4) & SFC3 == "", SFC4, SFC3),
           SFC4 = NA)
})

actual_nogem_Vinit <- do.call(rbind.data.frame, loop_actual_base_nogem_Vinit) %>%
  mutate(S1CF1 = "",
         S1CF2 = "",
         S1CF3 = "",
         S1CF4 = "")

actual_list_nogem <- rbind(actual_nogem_Cinit, actual_nogem_Vinit) %>%
  mutate(S2CF1 = "", 
         S2CF2 = "", 
         S2CF3 = "", 
         S2CF4 = "",
         S3CF1 = "", 
         S3CF2 = "", 
         S3CF3 = "", 
         S3CF4 = "")%>%
  dplyr::select(-final_seg)

## geminates

## consonant-initial

nsyl_actual_list_gem_Cinit <- full_data_disyls %>%
  filter(geminate_A == 1) %>%
  filter(str_detect(ActualCV_edited, '^C')) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_gem_Cinit <- lapply(nsyl_actual_list_gem_Cinit, FUN = function(element) {
  split_syl_Cinit <- element %>% separate(Vremoved_actual, c("seg1", "seg2", "seg3", "seg4"), "V")
  split_sylCinit2 <- split_syl_Cinit %>%
    separate(seg4, c("seg4", "SFC1_actual")) %>%
    mutate(S3CF_actual = ifelse(!is.na(SFC1_actual), seg4, NA),
           SFCF_actual = ifelse(is.na(SFC1_actual), seg4, NA)) %>%
    dplyr::select(-seg4) %>%
    separate(seg3, c("seg3a", "seg3b"), "-") %>%
    mutate(SFC1_actual = ifelse(is.na(SFC1_actual), seg3b, SFC1_actual)) %>%
    mutate(S3C1_actual = ifelse(!is.na(S3CF_actual), seg3a, NA),
           SFC1_actual = ifelse(is.na(SFC1_actual) & 
                                grepl("V$", ActualCV_edited), seg3a, SFC1_actual),
           SFCF_actual = ifelse(is.na(SFC1_actual) & !is.na(seg3a), seg3a, SFCF_actual)) %>%
    dplyr::select(-seg3a, -seg3b) %>%
    separate(seg2, c("seg2a", "seg2b"), "-") %>%
    mutate(SFC1_actual = ifelse((is.na(SFC1_actual)|SFC1_actual == "") & !is.na(seg2b), seg2b, SFC1_actual),
           SFC1_actual = ifelse((is.na(SFC1_actual)|SFC1_actual == "") & grepl("V$", ActualCV_edited), 
                              seg2b, SFC1_actual),
           SFCF_actual = ifelse((is.na(SFC1_actual)|SFC1_actual == "") & grepl("C$", ActualCV_edited),
                                seg2a, SFCF_actual),
           S1CF_actual = ifelse(!is.na(seg2b) & !grepl("-", seg1), seg2a, NA),
           S2CF_actual = ifelse(!is.na(SFC1_actual) & grepl("-", seg1), seg2a, NA)) %>%
    dplyr::select(-seg2a, -seg2b) %>% 
    separate(seg1, c("S1C1_actual", "S2C1_actual"), "-") %>%
    mutate(checks = ifelse(is.na(SFC1_actual) & !is.na(S2C1_actual), 1, 0),
           SFC1_actual = ifelse(checks == 1, S2C1_actual, SFC1_actual),
           S2C1_actual = ifelse(checks == 1, NA, S2C1_actual)) %>%
    dplyr::select(-checks)
    split_clust_Cinit_final <- split_sylCinit2 %>%
      separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
      separate(S1CF_actual, c("S1CF1", "S1CF2", "S1CF3", "S1CF4"), sep = "(?<=.)") %>%
      separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
      separate(S2CF_actual, c("S2CF1", "S2CF2", "S2CF3", "S2CF4"), sep = "(?<=.)") %>%
      separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
      separate(S3CF_actual, c("S3CF1", "S3CF2", "S3CF3", "S3CF4"), sep = "(?<=.)") %>%
      separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
      separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)")
})

actual_gem_Cinit <- do.call(rbind.data.frame, loop_actual_base_gem_Cinit)

## vowel-initial

nsyl_actual_list_gem_Vinit <- full_data_disyls %>%
  filter(geminate_A == 1) %>%
  filter(str_detect(ActualCV_edited, '^V')) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_gem_Vinit <- lapply(nsyl_actual_list_gem_Vinit, FUN = function(element) {
  split_syl_Vinit <- element %>% separate(Vremoved_actual, c("S1C1_actual", "seg2", "seg3", "seg4"), "V")
   split_sylVinit2 <- split_syl_Vinit %>%
      separate(seg4, c("seg4", "SFC1_actual")) %>%
     mutate(S3CF_actual = ifelse(!is.na(SFC1_actual), seg4, NA),
            SFC1_actual = ifelse(is.na(SFC1_actual), seg4, SFC1_actual)) %>%
    dplyr::select(-seg4) %>%
     separate(seg3, c("seg3a", "seg3b"), "-") %>%
     mutate(S3C1_actual = ifelse(!is.na(SFC1_actual) & nsyl_actual == 4, seg3b, NA),
            SFC1_actual = ifelse((is.na(SFC1_actual)|SFC1_actual == ""), seg3b, SFC1_actual),
            S2CF_actual = ifelse(!is.na(seg3b) & !is.na(SFC1_actual), seg3a, NA),
            S3C1_actual = ifelse(is.na(seg3b) & !is.na(SFC1_actual), seg3a, S3C1_actual),
            SFCF_actual = ifelse(is.na(seg3b) & is.na(SFC1_actual), seg3a, NA)) %>%
      dplyr::select(-seg3a, -seg3b) %>%
      separate(seg2, c("seg2a", "seg2b"), "-") %>%
      mutate(SFC1_actual = ifelse(is.na(SFC1_actual) & !is.na(seg2b), seg2b, SFC1_actual),
             S1CF_actual = ifelse(!is.na(seg2b), seg2a, NA),
             S2C1_actual = ifelse(is.na(seg2b), seg2a, NA)) %>%
     dplyr::select(-seg2a, -seg2b) 
  split_clust_Vinit_final <- split_sylVinit2 %>%
    separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
    separate(S1CF_actual, c("S1CF1", "S1CF2", "S1CF3", "S1CF4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(S2CF_actual, c("S2CF1", "S2CF2", "S2CF3", "S2CF4"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
    separate(S3CF_actual, c("S3CF1", "S3CF2", "S3CF3", "S3CF4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
    separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)")
})

actual_gem_Vinit <- do.call(rbind.data.frame, loop_actual_base_gem_Vinit)

### to work on next:

# decide which columns to include in each DF
# actual-list_nogem has S2 assigned but not CF in a lot of cases
# in one case CF is 0

actual_list_gem <- rbind(actual_gem_Cinit, actual_gem_Vinit) %>%
  mutate(S4C1 = "",
         S4C2 = "",
         S4C3 = "",
         S4C4 = "",
         S5C1 = "",
         S5C2 = "",
         S5C3 = "",
         S5C4 = "",
         S6C1 = "",
         S6C2 = "",
         S6C3 = "",
         S6C4 = "")

### sort manual alignments:

manually_assign <- c("pɪʔθʷɪʔθtθ", 
                     "wɑɸwəɸʔwε", 
                     "içʔʝi", 
                     "uvχri", 
                     "uvχre", 
                     "uvχri",
                     "əzɪdɫzjaʔ" ,
                    "əjʌkpɹεp")

manual_Cinit <- full_data_disyls %>%
  filter(IPAactual %in% manually_assign) %>%
  filter(str_detect(ActualCV_edited, '^C')) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_man_Cinit <- lapply(manual_Cinit, FUN = function(element) {
  split_syl_Cinit <- element %>% separate(Vremoved_actual, c("S1C1_actual", "seg2", "seg3"), "V") %>%
    mutate(SFC1_actual = ifelse(nsyl_actual == 2, seg2, NA),
           S2C1_actual = ifelse(nsyl_actual == 3, seg2, NA),
           SFCF_actual = ifelse(nsyl_actual == 2, seg3, NA),
           SFC1_actual = ifelse(nsyl_actual == 3, seg3, SFC1_actual)) %>%
    dplyr::select(-seg2, -seg3)
  split_clust_Cinit_final <- split_syl_Cinit %>%
    separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
    separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)") %>%
  mutate(SFC3 = ifelse((is.na(SFC3)|SFC3 == "") & !is.na(SFC4), SFC4, SFC3),
         SFC4 = ifelse(!is.na(SFC4), NA, SFC4),
         SFCF3 = ifelse((is.na(SFCF3)|SFCF3 == "") & !is.na(SFCF4), SFCF4, SFCF3),
         SFCF4 = ifelse(!is.na(SFCF4), NA, SFCF4))
})

actual_man_Cinit <- do.call(rbind.data.frame, loop_actual_base_man_Cinit) %>%
  mutate(S1CF1 = "",
         S1CF2 = "", 
         S1CF3 = "", 
         S1CF4 = "",
         S2CF1 = "", 
         S2CF2 = "", 
         S2CF3 = "", 
         S2CF4 = "",
         S3C1 = "", 
         S3C2 = "", 
         S3C3 = "", 
         S3C4 = "",
         S3CF1 = "", 
         S3CF2 = "", 
         S3CF3 = "", 
         S3CF4 = "", 
         S4C1 = "", 
         S4C2 = "", 
         S4C3 = "", 
         S4C4 = "", 
         S5C1 = "", 
         S5C2 = "", 
         S5C3 = "", 
         S5C4 = "", 
         S6C1 = "", 
         S6C2 = "", 
         S6C3 = "", 
         S6C4 = "")

manual_Vinit <- full_data_disyls %>%
  filter(IPAactual %in% manually_assign) %>%
  filter(str_detect(ActualCV_edited, '^V')) %>%
  split(., f = .$nsyl_actual)

loop_actual_base_man_Vinit <- lapply(manual_Vinit, FUN = function(element) {
  split_syl_Vinit <- element %>% separate(Vremoved_actual, c("S1C1_actual", "seg2", "seg3"), "V") %>%
    mutate(SFC1_actual = ifelse(nsyl_actual == 2, seg2, NA),
           S2C1_actual = ifelse(nsyl_actual == 3, seg2, NA),
           SFC1_actual = ifelse(nsyl_actual == 3, seg3, SFC1_actual)) %>%
     dplyr::select(-seg2, -seg3)
  split_clust_Vinit_final <- split_syl_Vinit %>%
    separate(S1C1_actual, c("S1C1", "S1C2", "S1C3", "S1C4"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
    mutate(SFC3 = ifelse((is.na(SFC3)|SFC3 == "") & !is.na(SFC4), SFC4, SFC3),
           SFC4 = ifelse(!is.na(SFC4), NA, SFC4))
})

actual_man_Vinit <- do.call(rbind.data.frame, loop_actual_base_man_Vinit) %>%
  mutate(S1CF1 = "",
         S1CF2 = "", 
         S1CF3 = "", 
         S1CF4 = "",
         S2CF1 = "", 
         S2CF2 = "", 
         S2CF3 = "", 
         S2CF4 = "",
         S3C1 = "", 
         S3C2 = "", 
         S3C3 = "", 
         S3C4 = "",
         S3CF1 = "", 
         S3CF2 = "", 
         S3CF3 = "", 
         S3CF4 = "", 
         SFCF1 = "", 
         SFCF2 = "", 
         SFCF3 = "", 
         SFCF4 = "",
         S4C1 = "", 
         S4C2 = "", 
         S4C3 = "", 
         S4C4 = "", 
         S5C1 = "", 
         S5C2 = "", 
         S5C3 = "", 
         S5C4 = "", 
         S6C1 = "", 
         S6C2 = "", 
         S6C3 = "", 
         S6C4 = "")

actual_list_man <- rbind(actual_man_Vinit, actual_man_Cinit)

actual_list_all <- rbind(actual_list_nogem, 
                         actual_list_gem, 
                         actual_list_man) %>% 
  mutate(data_type = "actual")  %>% dplyr::select(-Vremoved_target)

####

all_data_sample <- rbind(target_list_all, actual_list_all) %>%
  mutate(geminate_T = ifelse(geminate_T == 1, T, F),
         geminate_A = ifelse(geminate_A == 1, T, F)) %>%
  mutate(across(everything(), ~replace(., . %in% c(" ", "", 0), NA))) %>%
  tibble::rowid_to_column("ID")

all_data_sample_final <- all_data_sample %>% dplyr::select(ID, 
                                                           Subject,
                                                           Language,
                                                           Age,
                                                           Gloss,
                                                           IPAactual,
                                                           IPAtarget,
                                                           nsyl_actual, 
                                                           data_type,
                                                           geminate_T,
                                                           geminate_A
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


###############################################

## target

colnames_target <- all_data_sample %>% 
  filter(data_type == "target") %>% 
  dplyr::select(ID, starts_with("S"), -Subject)
colnames(colnames_target) <- sub("T","",colnames(colnames_target))
target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1],
                                                                             colnames_target[x])),
                        names(colnames_target)[-1])

output_target <- lapply(target_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], 
                                                                distinctive.feature.matrix$Symbol), 2:13],
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0)
})

output_target_df <- as.data.frame(output_target)
colnames(output_target_df)[1] <- "unique"

output_target_df <- output_target_df %>% 
  dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
   rename("ID" = "unique") %>%
  left_join(all_data_sample_final)

## actual

colnames_actual <- all_data_sample %>% 
  filter(data_type == "actual") %>% 
  dplyr::select(ID, starts_with("S"), -Subject)
colnames(colnames_actual) <- sub("T","",colnames(colnames_actual))
actual_list <- setNames(lapply(names(colnames_actual)[-1], function(x) cbind(colnames_actual[1],
                                                                             colnames_actual[x])),
                        names(colnames_actual)[-1])

output_actual <- lapply(actual_list, FUN = function(element) {
  actual_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], 
                                                                distinctive.feature.matrix$Symbol), 2:13],
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0)
})

output_actual_df <- as.data.frame(output_actual)
colnames(output_actual_df)[1] <- "unique"

output_actual_df <- output_actual_df %>% 
  dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
  rename("ID" = "unique") %>%
  left_join(all_data_sample_final)

output_all <- rbind(output_target_df, output_actual_df)

#write_csv(output_all, "output_df.csv")
#write.csv(output_all,"output_df.csv",fileEncoding = "UTF-8")
readr::write_excel_csv(output_all,"output_df.csv")
