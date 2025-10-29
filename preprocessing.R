library(arsenal)
library(tidyverse)
options("encoding" = "UTF-8")

data_ready <- read_csv("unprepared_data_edited.csv")%>%
  pivot_longer(
    cols = starts_with("IPAactual"),
    names_to = "utterance",
    names_prefix = "wk",
    values_to = "IPAactual",
    values_drop_na = TRUE
  ) %>%
fill(Language, Subject, Age, Gloss, .direction = "down") %>%
  dplyr::select(-utterance)

imitations <- data_ready %>% mutate(imitation = ifelse(grepl("(im.)", Gloss, fixed= T), T, F),
                                    imitation = ifelse(grepl("(im.)", IPAactual, fixed= T), T, imitation)) %>%
  filter(imitation == T)

# remove data in parentheses
data_ready$IPAactual <- gsub("\\s*\\([^\\)]+\\)","", as.character(data_ready$IPAactual))
data_ready$Gloss <- gsub("\\s*\\([^\\)]+\\)","", as.character(data_ready$Gloss))
data_ready$IPAtarget <- gsub("\\s*\\([^\\)]+\\)","", as.character(data_ready$IPAtarget))

FR_prepped <- read_csv("Prepared_data_FR.csv") %>%
  fill(Gloss, IPAtarget, .direction = "down") %>%
  mutate(Language = "French")

# remove data in parentheses
FR_prepped$IPAactual <- gsub("\\s*\\([^\\)]+\\)","", as.character(FR_prepped$IPAactual))
FR_prepped$Gloss <- gsub("\\s*\\([^\\)]+\\)","", as.character(FR_prepped$Gloss))
FR_prepped$IPAtarget <- gsub("\\s*\\([^\\)]+\\)","", as.character(FR_prepped$IPAtarget))

all_data <- rbind(FR_prepped, data_ready)

#usable_data <- all_data %>% group_by(Subject, Language) %>% tally()

IPA_transcriptions <- read_csv("IPA transcriptions.csv")%>%
  fill(Language, Subject) %>%
  group_by(Subject, Language, Gloss) %>% slice(1)

usable_data <- c("Ella",
                 "Ivy",
                 "Lewis",
                 "Patrick" ,
                 "Rachel",
                 "Tobias",
                 "Atte",
                 "Eelis",
                 "Eliisa",
                 "Mira",
                 "Sini",
                 "Adeline",
                 "Basile",  
                 "Gaël",
                 "Julien", 
                 "Romuald",
                 "Vincent" ,   
                 "Hiromi",
                 "Takeru",
                 "Haruo",
                 "Kazuko",
                 "Taro", 
                 "Afan",
                 "Radia",
                 "Shazim",
                 "Gwyn", 
                 "Carys",
                 "Fflur",
                 "Elen" 
                 )


all_data <- all_data %>% filter(Subject %in% usable_data) %>%
  left_join(IPA_transcriptions) %>%
  mutate(IPAtarget = ifelse(is.na(IPAtarget), IPA, IPAtarget)) #%>%
  #fill(IPAtarget)

to_transcribe <- all_data %>% filter(is.na(IPAtarget)) %>% group_by(Language, Subject, Gloss) %>% tally()

write_csv(all_data, "data_ready.csv")
