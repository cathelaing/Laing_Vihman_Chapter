


#   mutate(S3C1_actual = ifelse(nsyl_actual == 4 & !is.na(SF_actual), seg3b, NA),
#          SF_actual = ifelse(nsyl_actual > 1 & is.na(SF_actual)|SF_actual == "", seg3b, SF_actual)) %>%
#   dplyr::select(-seg3b) %>%
#   mutate(SXCF_actual = ifelse(nsyl_actual < 5 & !is.na(SF_actual), seg3a, NA),
#          S4C1_actual = ifelse(nsyl_actual == 5 & is.na(S3C1_actual), seg3a, S4C1_actual),
#          SF_actual = ifelse(!is.na(seg3a) & is.na(SF_actual), seg3a, SF_actual)) %>%
#   dplyr::select(-seg3a) %>%
#   separate(seg2, c("seg2a", "seg2b"), "-") %>%
#   mutate(SF_actual = ifelse(nsyl_actual == 1 &
#                               (is.na(SF_actual)|SF_actual == ""), seg2b, SF_actual),
#          SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(seg2b), seg2b, NA),
#          S2C1_actual = ifelse(nsyl_actual == 3, seg2b, NA)) %>%
#   dplyr::select(-seg2b) %>%
#   mutate(S1CF_actual = ifelse(nsyl_actual == 1 &
#                                 (!is.na(SF_actual)) & (!grepl("-", seg1)), seg2a, NA),
#          SF_actual = ifelse(nsyl_actual == 1 & is.na(SF_actual), seg2a, SF_actual),
#          ## individual case that prob needs removing:
#          S2C1_actual = ifelse(nsyl_actual == 1 & grepl("βʔβʔβ", seg2a), seg2a, S2C1_actual),
#          SFC1_actual = ifelse(nsyl_actual == 2 & is.na(SFC1_actual) &
#                                 (is.na(SF_actual)|SF_actual == ""), seg2a, SFC1_actual),
#          S2C1_actual = ifelse(nsyl_actual == 2 & !is.na(SXCF_actual), seg2a, S2C1_actual),
#          S1CF_actual = ifelse(nsyl_actual == 2 & (!grepl("-", seg1)) &
#                                 is.na(SXCF_actual), seg2a, S1CF_actual),
#          # now need to sort some specific tokens that don't pattern with others
#          SFC1_actual = ifelse(nsyl_actual == 2 & seg1 == "m-m" & seg2a == "ʔ", "ʔ", SFC1_actual),
#          SXCF_actual = ifelse(nsyl_actual == 2 & seg1 == "m-m" & seg2a == "m", "m", SXCF_actual),
#          S3C1_actual = ifelse(nsyl_actual == 3 & seg1 == "n-n", seg2a, S3C1_actual),
#          S1CF_actual = ifelse(nsyl_actual == 3 & !is.na(S2C1_actual), seg2a, S1CF_actual),
#          S2C1_actual = ifelse(nsyl_actual == 3 & !is.na(SXCF_actual) &
#                                 is.na(S2C1_actual), seg2a, S2C1_actual),
#          S2C1_actual = ifelse(nsyl_actual == 4 & is.na(S2C1_actual), seg2a, S2C1_actual)) %>%
#   dplyr::select(-seg2a) %>%
#   separate(seg1, c("S1C1_actual", "seg1b"), "-") %>%
#   mutate(SFC1_actual = ifelse(nsyl_actual == 1 & !is.na(SF_actual), seg1b, SFC1_actual),
#          SF_actual = ifelse(nsyl_actual == 1 & is.na(SF_actual), seg1b, SF_actual),
#          S2C1_actual = ifelse((nsyl_actual == 2|nsyl_actual == 3) & is.na(S2C1_actual), seg1b, S2C1_actual),
#          S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(S2C1_actual), seg1b, S3C1_actual)) %>%
#   dplyr::select(-seg1b) %>%
#   #fixing some errors:
#   mutate(S3C1_actual = ifelse(Gloss == "money" & IPAactual == "m:ʌm:ɪ", NA, S3C1_actual),
#          S3CF_actual = ifelse(SXCF_actual == "β" & nsyl_actual == 2, "β", NA),
#          SXCF_actual = ifelse(SXCF_actual == "β" & nsyl_actual == 2, NA, SXCF_actual),
#          S2CF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 2, SXCF_actual, NA),
#          SXCF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 2, NA, SXCF_actual),
#          S3CF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 3, SXCF_actual, S3CF_actual),
#          SXCF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 3, NA, SXCF_actual),
#          S2CF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 4, SXCF_actual, S2CF_actual),
#          SXCF_actual = ifelse(!is.na(SXCF_actual) & nsyl_actual == 4, NA, SXCF_actual)) %>%
#   dplyr::select(-SXCF_actual) #%>%
# ## now sort nsyls so it's accurate ## update: ignoring, it's complicated and may not be needed
# # mutate(nsyl_actual = ifelse(nsyl_actual == 1, 2, nsyl_actual),
# #        nsyl_actual = ifelse(nsyl_actual == 2 & !is.na(S2C1_actual) & is.na(S3C1_actual), 3, nsyl_actual),
# #        nsyl_actual = ifelse(nsyl_actual == 3 & !is.na(S3C1_actual) & is.na(S4C1_actual), 4, nsyl_actual),
# #        nsyl_actual = ifelse(nsyl_actual == 4 & !is.na(S4C1_actual), 5, nsyl_actual))




#### Now work on vowel-initial forms

# split_syl_Vinit <- element %>% filter(ActualCV %in% Vinitial$ActualCV) %>%
#   separate(Vremoved_actual, c("seg1", "seg2", "seg3", "seg4", "seg5"), "V") %>%
#   separate(seg2, c("seg2a", "seg2b", "seg2c"), "-", remove = F) %>%
#   mutate(S1CF_actual = ifelse(grepl("-", seg2), seg2a, NA),
#          S2C1_actual = ifelse(!grepl("-", seg2), seg2a, NA),
#          S2C1_actual = ifelse(grepl("-", seg2), seg2b, S2C1_actual),
#          SFC1_actual = ifelse(!is.na(seg2c), seg2c, NA)) %>%
#   dplyr::select(-seg1, -seg2, -seg2a, -seg2b, -seg2c) %>%
#   separate(seg3, c("seg3a", "seg3b"), "-", remove = F) %>%
#   mutate(
#     S3C1_actual = ifelse(nsyl_actual ==4 & is.na(seg3b), seg3a, NA),
#     S3C1_actual = ifelse(nsyl_actual ==4 & !is.na(seg3b), seg3b, S3C1_actual),
#     SFC1_actual = ifelse(nsyl_actual ==3 & !is.na(seg3b), seg3b, SFC1_actual),
#     S3C1_actual = ifelse(nsyl_actual ==3 & grepl("-", seg4), seg3a, S3C1_actual),
#     S2CF_actual = ifelse(grepl("-", seg3), seg3a, NA),
#     SFC1_actual = ifelse(nsyl_actual == 3 & (!is.na(seg4)|seg4 !=""), seg3a, SFC1_actual)) %>%
#   dplyr::select(-seg3, -seg3a, -seg3b) %>%
#   separate(seg4, c("seg4a", "seg4b"), "-", remove = F) %>%
#   mutate(SFC1_actual = ifelse(seg5 == "" & is.na(seg4b) & is.na(SFC1_actual), seg4a, SFC1_actual),
#          SFCF_actual = ifelse(!is.na(seg4b), seg4b, NA),
#          SFCF_actual = ifelse(is.na(seg5) & is.na(seg4b) & is.na(SFCF_actual), seg4a, SFCF_actual)) %>%
#   dplyr::select(-seg4, -seg4a, -seg4b, -seg5)
# split_sylVinit2 <- split_syl_Vinit %>%
#   mutate(
#     nsyl_actual = ifelse(nsyl_actual == 1, 2, nsyl_actual),
#     ## adding data manually due to idiosyncratic token
#     S3CF_actual = ifelse(ActualCV == "VCVCCVCCC", "s", NA),
#     SFC1_actual = ifelse(ActualCV == "VCVCCVCCC", NA, SFC1_actual),
#     nsyl_actual = ifelse(ActualCV == "VCVCCVCCC", 4, nsyl_actual))
# split_clust_Vinit_final <- split_sylVinit2 %>%
#   separate(S1CF_actual, c("S1CF1", "S1CF2", "S1CF3", "S1CF4"), sep = "(?<=.)") %>%
#   separate(S2C1_actual, c("S2C1", "S2C2", "S2C3", "S2C4"), sep = "(?<=.)") %>%
#   separate(S2CF_actual, c("S2CF1", "S2CF2", "S2CF3", "S2CF4"), sep = "(?<=.)") %>%
#   separate(S3C1_actual, c("S3C1", "S3C2", "S3C3", "S3C4"), sep = "(?<=.)") %>%
#   separate(S3CF_actual, c("S3CF1", "S3CF2", "S3CF3", "S3CF4"), sep = "(?<=.)") %>%
#   separate(SFC1_actual, c("SFC1", "SFC2", "SFC3", "SFC4"), sep = "(?<=.)") %>%
#   separate(SFCF_actual, c("SFCF1", "SFCF2", "SFCF3", "SFCF4"), sep = "(?<=.)") %>%
#   mutate(S1C1 = "",
#          S1C2 = "",
#          S1C3 = "",
#          S1C4 = "",
#          S4C1 = "",
#          S4C2 = "",
#          S4C3 = "",
#          S4C4 = "")