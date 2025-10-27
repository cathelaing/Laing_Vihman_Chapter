
output_df <- read_csv("output_df.csv")


distance_df <- output_df %>%
  mutate(#subj_session = paste(Subject, Age, sep="_"),
         GlossID = paste(Gloss, ID, sep="")) %>%
  write_csv("distance_df.csv")

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list <- distance_df %>%    
  split(., f = .$Subject)

global_matrix <- lapply(data_list, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  anterior_vec.S1C1 <- element$S1C1.Anterior
  anterior_mat.S1C1 <- (anterior_vec.S1C1 %*% t(ones) - ones %*% t(anterior_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1 + 
    anterior_mat.S1C1
  
  rownames(mat.S1C1) <- element$GlossID
  colnames(mat.S1C1) <- element$GlossID
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  anterior_vec.S1C2 <- element$S1C2.Anterior
  anterior_mat.S1C2 <- (anterior_vec.S1C2 %*% t(ones) - ones %*% t(anterior_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2 + 
    anterior_mat.S1C2
  
  rownames(mat.S1C2) <- element$GlossID
  colnames(mat.S1C2) <- element$GlossID
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  anterior_vec.S1C3 <- element$S1C3.Anterior
  anterior_mat.S1C3 <- (anterior_vec.S1C3 %*% t(ones) - ones %*% t(anterior_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3 + 
    anterior_mat.S1C3
  
  rownames(mat.S1C3) <- element$GlossID
  colnames(mat.S1C3) <- element$GlossID
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  anterior_vec.S1C4 <- element$S1C4.Anterior
  anterior_mat.S1C4 <- (anterior_vec.S1C4 %*% t(ones) - ones %*% t(anterior_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4 + 
    anterior_mat.S1C4
  
  rownames(mat.S1C4) <- element$GlossID
  colnames(mat.S1C4) <- element$GlossID  
  
  sonorant_vec.S1CF1 <- element$S1CF1.Sonorant
  sonorant_mat.S1CF1 <- (sonorant_vec.S1CF1 %*% t(ones) - ones %*% t(sonorant_vec.S1CF1))^2
  
  consonantal_vec.S1CF1 <- element$S1CF1.Consonantal
  consonantal_mat.S1CF1 <- (consonantal_vec.S1CF1 %*% t(ones) - ones %*% t(consonantal_vec.S1CF1))^2
  
  voice_vec.S1CF1 <- element$S1CF1.Voice
  voice_mat.S1CF1 <- (voice_vec.S1CF1 %*% t(ones) - ones %*% t(voice_vec.S1CF1))^2
  
  nasal_vec.S1CF1 <- element$S1CF1.Nasal
  nasal_mat.S1CF1 <- (nasal_vec.S1CF1 %*% t(ones) - ones %*% t(nasal_vec.S1CF1))^2
  
  degree_vec.S1CF1 <- element$S1CF1.Degree
  degree_mat.S1CF1 <- (degree_vec.S1CF1 %*% t(ones) - ones %*% t(degree_vec.S1CF1))^2
  
  labial_vec.S1CF1 <- element$S1CF1.Labial
  labial_mat.S1CF1 <- (labial_vec.S1CF1 %*% t(ones) - ones %*% t(labial_vec.S1CF1))^2
  
  palatal_vec.S1CF1 <- element$S1CF1.Palatal
  palatal_mat.S1CF1 <- (palatal_vec.S1CF1 %*% t(ones) - ones %*% t(palatal_vec.S1CF1))^2
  
  pharyngeal_vec.S1CF1 <- element$S1CF1.Pharyngeal
  pharyngeal_mat.S1CF1 <- (pharyngeal_vec.S1CF1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1CF1))^2
  
  round_vec.S1CF1 <- element$S1CF1.Round
  round_mat.S1CF1 <- (round_vec.S1CF1 %*% t(ones) - ones %*% t(round_vec.S1CF1))^2
  
  tongue_vec.S1CF1 <- element$S1CF1.Tongue
  tongue_mat.S1CF1 <- (tongue_vec.S1CF1 %*% t(ones) - ones %*% t(tongue_vec.S1CF1))^2
  
  radical_vec.S1CF1 <- element$S1CF1.Radical
  radical_mat.S1CF1 <- (radical_vec.S1CF1 %*% t(ones) - ones %*% t(radical_vec.S1CF1))^2
  
  anterior_vec.S1CF1 <- element$S1CF1.Anterior
  anterior_mat.S1CF1 <- (anterior_vec.S1CF1 %*% t(ones) - ones %*% t(anterior_vec.S1CF1))^2
  
  mat.S1CF1 <- sonorant_mat.S1CF1 + 
    consonantal_mat.S1CF1 + 
    voice_mat.S1CF1 + 
    nasal_mat.S1CF1 + 
    degree_mat.S1CF1 + 
    labial_mat.S1CF1 + 
    palatal_mat.S1CF1 + 
    pharyngeal_mat.S1CF1 + 
    round_mat.S1CF1 + 
    tongue_mat.S1CF1 + 
    radical_mat.S1CF1 + 
    anterior_mat.S1CF1
  
  rownames(mat.S1CF1) <- element$GlossID
  colnames(mat.S1CF1) <- element$GlossID
  
  sonorant_vec.S1CF2 <- element$S1CF2.Sonorant
  sonorant_mat.S1CF2 <- (sonorant_vec.S1CF2 %*% t(ones) - ones %*% t(sonorant_vec.S1CF2))^2
  
  consonantal_vec.S1CF2 <- element$S1CF2.Consonantal
  consonantal_mat.S1CF2 <- (consonantal_vec.S1CF2 %*% t(ones) - ones %*% t(consonantal_vec.S1CF2))^2
  
  voice_vec.S1CF2 <- element$S1CF2.Voice
  voice_mat.S1CF2 <- (voice_vec.S1CF2 %*% t(ones) - ones %*% t(voice_vec.S1CF2))^2
  
  nasal_vec.S1CF2 <- element$S1CF2.Nasal
  nasal_mat.S1CF2 <- (nasal_vec.S1CF2 %*% t(ones) - ones %*% t(nasal_vec.S1CF2))^2
  
  degree_vec.S1CF2 <- element$S1CF2.Degree
  degree_mat.S1CF2 <- (degree_vec.S1CF2 %*% t(ones) - ones %*% t(degree_vec.S1CF2))^2
  
  labial_vec.S1CF2 <- element$S1CF2.Labial
  labial_mat.S1CF2 <- (labial_vec.S1CF2 %*% t(ones) - ones %*% t(labial_vec.S1CF2))^2
  
  palatal_vec.S1CF2 <- element$S1CF2.Palatal
  palatal_mat.S1CF2 <- (palatal_vec.S1CF2 %*% t(ones) - ones %*% t(palatal_vec.S1CF2))^2
  
  pharyngeal_vec.S1CF2 <- element$S1CF2.Pharyngeal
  pharyngeal_mat.S1CF2 <- (pharyngeal_vec.S1CF2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1CF2))^2
  
  round_vec.S1CF2 <- element$S1CF2.Round
  round_mat.S1CF2 <- (round_vec.S1CF2 %*% t(ones) - ones %*% t(round_vec.S1CF2))^2
  
  tongue_vec.S1CF2 <- element$S1CF2.Tongue
  tongue_mat.S1CF2 <- (tongue_vec.S1CF2 %*% t(ones) - ones %*% t(tongue_vec.S1CF2))^2
  
  radical_vec.S1CF2 <- element$S1CF2.Radical
  radical_mat.S1CF2 <- (radical_vec.S1CF2 %*% t(ones) - ones %*% t(radical_vec.S1CF2))^2
  
  anterior_vec.S1CF2 <- element$S1CF2.Anterior
  anterior_mat.S1CF2 <- (anterior_vec.S1CF2 %*% t(ones) - ones %*% t(anterior_vec.S1CF2))^2
  
  mat.S1CF2 <- sonorant_mat.S1CF2 + 
    consonantal_mat.S1CF2 + 
    voice_mat.S1CF2 + 
    nasal_mat.S1CF2 + 
    degree_mat.S1CF2 + 
    labial_mat.S1CF2 + 
    palatal_mat.S1CF2 + 
    pharyngeal_mat.S1CF2 + 
    round_mat.S1CF2 + 
    tongue_mat.S1CF2 + 
    radical_mat.S1CF2 + 
    anterior_mat.S1CF2
  
  rownames(mat.S1CF2) <- element$GlossID
  colnames(mat.S1CF2) <- element$GlossID
  
  sonorant_vec.S1CF3 <- element$S1CF3.Sonorant
  sonorant_mat.S1CF3 <- (sonorant_vec.S1CF3 %*% t(ones) - ones %*% t(sonorant_vec.S1CF3))^2
  
  consonantal_vec.S1CF3 <- element$S1CF3.Consonantal
  consonantal_mat.S1CF3 <- (consonantal_vec.S1CF3 %*% t(ones) - ones %*% t(consonantal_vec.S1CF3))^2
  
  voice_vec.S1CF3 <- element$S1CF3.Voice
  voice_mat.S1CF3 <- (voice_vec.S1CF3 %*% t(ones) - ones %*% t(voice_vec.S1CF3))^2
  
  nasal_vec.S1CF3 <- element$S1CF3.Nasal
  nasal_mat.S1CF3 <- (nasal_vec.S1CF3 %*% t(ones) - ones %*% t(nasal_vec.S1CF3))^2
  
  degree_vec.S1CF3 <- element$S1CF3.Degree
  degree_mat.S1CF3 <- (degree_vec.S1CF3 %*% t(ones) - ones %*% t(degree_vec.S1CF3))^2
  
  labial_vec.S1CF3 <- element$S1CF3.Labial
  labial_mat.S1CF3 <- (labial_vec.S1CF3 %*% t(ones) - ones %*% t(labial_vec.S1CF3))^2
  
  palatal_vec.S1CF3 <- element$S1CF3.Palatal
  palatal_mat.S1CF3 <- (palatal_vec.S1CF3 %*% t(ones) - ones %*% t(palatal_vec.S1CF3))^2
  
  pharyngeal_vec.S1CF3 <- element$S1CF3.Pharyngeal
  pharyngeal_mat.S1CF3 <- (pharyngeal_vec.S1CF3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1CF3))^2
  
  round_vec.S1CF3 <- element$S1CF3.Round
  round_mat.S1CF3 <- (round_vec.S1CF3 %*% t(ones) - ones %*% t(round_vec.S1CF3))^2
  
  tongue_vec.S1CF3 <- element$S1CF3.Tongue
  tongue_mat.S1CF3 <- (tongue_vec.S1CF3 %*% t(ones) - ones %*% t(tongue_vec.S1CF3))^2
  
  radical_vec.S1CF3 <- element$S1CF3.Radical
  radical_mat.S1CF3 <- (radical_vec.S1CF3 %*% t(ones) - ones %*% t(radical_vec.S1CF3))^2
  
  anterior_vec.S1CF3 <- element$S1CF3.Anterior
  anterior_mat.S1CF3 <- (anterior_vec.S1CF3 %*% t(ones) - ones %*% t(anterior_vec.S1CF3))^2
  
  mat.S1CF3 <- sonorant_mat.S1CF3 + 
    consonantal_mat.S1CF3 + 
    voice_mat.S1CF3 + 
    nasal_mat.S1CF3 + 
    degree_mat.S1CF3 + 
    labial_mat.S1CF3 + 
    palatal_mat.S1CF3 + 
    pharyngeal_mat.S1CF3 + 
    round_mat.S1CF3 + 
    tongue_mat.S1CF3 + 
    radical_mat.S1CF3 + 
    anterior_mat.S1CF3
  
  rownames(mat.S1CF3) <- element$GlossID
  colnames(mat.S1CF3) <- element$GlossID
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  anterior_vec.S2C1 <- element$S2C1.Anterior
  anterior_mat.S2C1 <- (anterior_vec.S2C1 %*% t(ones) - ones %*% t(anterior_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1 + 
    anterior_mat.S2C1
  
  rownames(mat.S2C1) <- element$GlossID
  colnames(mat.S2C1) <- element$GlossID
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  anterior_vec.S2C2 <- element$S2C2.Anterior
  anterior_mat.S2C2 <- (anterior_vec.S2C2 %*% t(ones) - ones %*% t(anterior_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2 + 
    anterior_mat.S2C2
  
  rownames(mat.S2C2) <- element$GlossID
  colnames(mat.S2C2) <- element$GlossID
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  anterior_vec.S2C3 <- element$S2C3.Anterior
  anterior_mat.S2C3 <- (anterior_vec.S2C3 %*% t(ones) - ones %*% t(anterior_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3 + 
    anterior_mat.S2C3
  
  rownames(mat.S2C3) <- element$GlossID
  colnames(mat.S2C3) <- element$GlossID
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  anterior_vec.S2C4 <- element$S2C4.Anterior
  anterior_mat.S2C4 <- (anterior_vec.S2C4 %*% t(ones) - ones %*% t(anterior_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4 + 
    anterior_mat.S2C4
  
  rownames(mat.S2C4) <- element$GlossID
  colnames(mat.S2C4) <- element$GlossID
  
  sonorant_vec.S2CF1 <- element$S2CF1.Sonorant
  sonorant_mat.S2CF1 <- (sonorant_vec.S2CF1 %*% t(ones) - ones %*% t(sonorant_vec.S2CF1))^2
  
  consonantal_vec.S2CF1 <- element$S2CF1.Consonantal
  consonantal_mat.S2CF1 <- (consonantal_vec.S2CF1 %*% t(ones) - ones %*% t(consonantal_vec.S2CF1))^2
  
  voice_vec.S2CF1 <- element$S2CF1.Voice
  voice_mat.S2CF1 <- (voice_vec.S2CF1 %*% t(ones) - ones %*% t(voice_vec.S2CF1))^2
  
  nasal_vec.S2CF1 <- element$S2CF1.Nasal
  nasal_mat.S2CF1 <- (nasal_vec.S2CF1 %*% t(ones) - ones %*% t(nasal_vec.S2CF1))^2
  
  degree_vec.S2CF1 <- element$S2CF1.Degree
  degree_mat.S2CF1 <- (degree_vec.S2CF1 %*% t(ones) - ones %*% t(degree_vec.S2CF1))^2
  
  labial_vec.S2CF1 <- element$S2CF1.Labial
  labial_mat.S2CF1 <- (labial_vec.S2CF1 %*% t(ones) - ones %*% t(labial_vec.S2CF1))^2
  
  palatal_vec.S2CF1 <- element$S2CF1.Palatal
  palatal_mat.S2CF1 <- (palatal_vec.S2CF1 %*% t(ones) - ones %*% t(palatal_vec.S2CF1))^2
  
  pharyngeal_vec.S2CF1 <- element$S2CF1.Pharyngeal
  pharyngeal_mat.S2CF1 <- (pharyngeal_vec.S2CF1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2CF1))^2
  
  round_vec.S2CF1 <- element$S2CF1.Round
  round_mat.S2CF1 <- (round_vec.S2CF1 %*% t(ones) - ones %*% t(round_vec.S2CF1))^2
  
  tongue_vec.S2CF1 <- element$S2CF1.Tongue
  tongue_mat.S2CF1 <- (tongue_vec.S2CF1 %*% t(ones) - ones %*% t(tongue_vec.S2CF1))^2
  
  radical_vec.S2CF1 <- element$S2CF1.Radical
  radical_mat.S2CF1 <- (radical_vec.S2CF1 %*% t(ones) - ones %*% t(radical_vec.S2CF1))^2
  
  anterior_vec.S2CF1 <- element$S2CF1.Anterior
  anterior_mat.S2CF1 <- (anterior_vec.S2CF1 %*% t(ones) - ones %*% t(anterior_vec.S2CF1))^2
  
  mat.S2CF1 <- sonorant_mat.S2CF1 + 
    consonantal_mat.S2CF1 + 
    voice_mat.S2CF1 + 
    nasal_mat.S2CF1 + 
    degree_mat.S2CF1 + 
    labial_mat.S2CF1 + 
    palatal_mat.S2CF1 + 
    pharyngeal_mat.S2CF1 + 
    round_mat.S2CF1 + 
    tongue_mat.S2CF1 + 
    radical_mat.S2CF1 + 
    anterior_mat.S2CF1
  
  rownames(mat.S2CF1) <- element$GlossID
  colnames(mat.S2CF1) <- element$GlossID
  
  sonorant_vec.S2CF2 <- element$S2CF2.Sonorant
  sonorant_mat.S2CF2 <- (sonorant_vec.S2CF2 %*% t(ones) - ones %*% t(sonorant_vec.S2CF2))^2
  
  consonantal_vec.S2CF2 <- element$S2CF2.Consonantal
  consonantal_mat.S2CF2 <- (consonantal_vec.S2CF2 %*% t(ones) - ones %*% t(consonantal_vec.S2CF2))^2
  
  voice_vec.S2CF2 <- element$S2CF2.Voice
  voice_mat.S2CF2 <- (voice_vec.S2CF2 %*% t(ones) - ones %*% t(voice_vec.S2CF2))^2
  
  nasal_vec.S2CF2 <- element$S2CF2.Nasal
  nasal_mat.S2CF2 <- (nasal_vec.S2CF2 %*% t(ones) - ones %*% t(nasal_vec.S2CF2))^2
  
  degree_vec.S2CF2 <- element$S2CF2.Degree
  degree_mat.S2CF2 <- (degree_vec.S2CF2 %*% t(ones) - ones %*% t(degree_vec.S2CF2))^2
  
  labial_vec.S2CF2 <- element$S2CF2.Labial
  labial_mat.S2CF2 <- (labial_vec.S2CF2 %*% t(ones) - ones %*% t(labial_vec.S2CF2))^2
  
  palatal_vec.S2CF2 <- element$S2CF2.Palatal
  palatal_mat.S2CF2 <- (palatal_vec.S2CF2 %*% t(ones) - ones %*% t(palatal_vec.S2CF2))^2
  
  pharyngeal_vec.S2CF2 <- element$S2CF2.Pharyngeal
  pharyngeal_mat.S2CF2 <- (pharyngeal_vec.S2CF2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2CF2))^2
  
  round_vec.S2CF2 <- element$S2CF2.Round
  round_mat.S2CF2 <- (round_vec.S2CF2 %*% t(ones) - ones %*% t(round_vec.S2CF2))^2
  
  tongue_vec.S2CF2 <- element$S2CF2.Tongue
  tongue_mat.S2CF2 <- (tongue_vec.S2CF2 %*% t(ones) - ones %*% t(tongue_vec.S2CF2))^2
  
  radical_vec.S2CF2 <- element$S2CF2.Radical
  radical_mat.S2CF2 <- (radical_vec.S2CF2 %*% t(ones) - ones %*% t(radical_vec.S2CF2))^2
  
  anterior_vec.S2CF2 <- element$S2CF2.Anterior
  anterior_mat.S2CF2 <- (anterior_vec.S2CF2 %*% t(ones) - ones %*% t(anterior_vec.S2CF2))^2
  
  mat.S2CF2 <- sonorant_mat.S2CF2 + 
    consonantal_mat.S2CF2 + 
    voice_mat.S2CF2 + 
    nasal_mat.S2CF2 + 
    degree_mat.S2CF2 + 
    labial_mat.S2CF2 + 
    palatal_mat.S2CF2 + 
    pharyngeal_mat.S2CF2 + 
    round_mat.S2CF2 + 
    tongue_mat.S2CF2 + 
    radical_mat.S2CF2 + 
    radical_mat.S2CF2
  
  rownames(mat.S2CF2) <- element$GlossID
  colnames(mat.S2CF2) <- element$GlossID
  
  sonorant_vec.S2CF3 <- element$S2CF3.Sonorant
  sonorant_mat.S2CF3 <- (sonorant_vec.S2CF3 %*% t(ones) - ones %*% t(sonorant_vec.S2CF3))^2
  
  consonantal_vec.S2CF3 <- element$S2CF3.Consonantal
  consonantal_mat.S2CF3 <- (consonantal_vec.S2CF3 %*% t(ones) - ones %*% t(consonantal_vec.S2CF3))^2
  
  voice_vec.S2CF3 <- element$S2CF3.Voice
  voice_mat.S2CF3 <- (voice_vec.S2CF3 %*% t(ones) - ones %*% t(voice_vec.S2CF3))^2
  
  nasal_vec.S2CF3 <- element$S2CF3.Nasal
  nasal_mat.S2CF3 <- (nasal_vec.S2CF3 %*% t(ones) - ones %*% t(nasal_vec.S2CF3))^2
  
  degree_vec.S2CF3 <- element$S2CF3.Degree
  degree_mat.S2CF3 <- (degree_vec.S2CF3 %*% t(ones) - ones %*% t(degree_vec.S2CF3))^2
  
  labial_vec.S2CF3 <- element$S2CF3.Labial
  labial_mat.S2CF3 <- (labial_vec.S2CF3 %*% t(ones) - ones %*% t(labial_vec.S2CF3))^2
  
  palatal_vec.S2CF3 <- element$S2CF3.Palatal
  palatal_mat.S2CF3 <- (palatal_vec.S2CF3 %*% t(ones) - ones %*% t(palatal_vec.S2CF3))^2
  
  pharyngeal_vec.S2CF3 <- element$S2CF3.Pharyngeal
  pharyngeal_mat.S2CF3 <- (pharyngeal_vec.S2CF3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2CF3))^2
  
  round_vec.S2CF3 <- element$S2CF3.Round
  round_mat.S2CF3 <- (round_vec.S2CF3 %*% t(ones) - ones %*% t(round_vec.S2CF3))^2
  
  tongue_vec.S2CF3 <- element$S2CF3.Tongue
  tongue_mat.S2CF3 <- (tongue_vec.S2CF3 %*% t(ones) - ones %*% t(tongue_vec.S2CF3))^2
  
  radical_vec.S2CF3 <- element$S2CF3.Radical
  radical_mat.S2CF3 <- (radical_vec.S2CF3 %*% t(ones) - ones %*% t(radical_vec.S2CF3))^2
  
  anterior_vec.S2CF3 <- element$S2CF3.Anterior
  anterior_mat.S2CF3 <- (anterior_vec.S2CF3 %*% t(ones) - ones %*% t(anterior_vec.S2CF3))^2
  
  mat.S2CF3 <- sonorant_mat.S2CF3 + 
    consonantal_mat.S2CF3 + 
    voice_mat.S2CF3 + 
    nasal_mat.S2CF3 + 
    degree_mat.S2CF3 + 
    labial_mat.S2CF3 + 
    palatal_mat.S2CF3 + 
    pharyngeal_mat.S2CF3 + 
    round_mat.S2CF3 + 
    tongue_mat.S2CF3 + 
    radical_mat.S2CF3 + 
    anterior_mat.S2CF3
  
  rownames(mat.S2CF3) <- element$GlossID
  colnames(mat.S2CF3) <- element$GlossID
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  anterior_vec.S3C1 <- element$S3C1.Anterior
  anterior_mat.S3C1 <- (anterior_vec.S3C1 %*% t(ones) - ones %*% t(anterior_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1 + 
    anterior_mat.S3C1
  
  rownames(mat.S3C1) <- element$GlossID
  colnames(mat.S3C1) <- element$GlossID
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  anterior_vec.S3C2 <- element$S3C2.Anterior
  anterior_mat.S3C2 <- (anterior_vec.S3C2 %*% t(ones) - ones %*% t(anterior_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2 + 
    anterior_mat.S3C2
  
  rownames(mat.S3C2) <- element$GlossID
  colnames(mat.S3C2) <- element$GlossID
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  anterior_vec.S3C3 <- element$S3C3.Anterior
  anterior_mat.S3C3 <- (anterior_vec.S3C3 %*% t(ones) - ones %*% t(anterior_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3 + 
    anterior_mat.S3C3
  
  rownames(mat.S3C3) <- element$GlossID
  colnames(mat.S3C3) <- element$GlossID
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  anterior_vec.S3C4 <- element$S3C4.Anterior
  anterior_mat.S3C4 <- (anterior_vec.S3C4 %*% t(ones) - ones %*% t(anterior_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4 + 
    anterior_mat.S3C4
  
  rownames(mat.S3C4) <- element$GlossID
  colnames(mat.S3C4) <- element$GlossID
  
  sonorant_vec.S3CF1 <- element$S3CF1.Sonorant
  sonorant_mat.S3CF1 <- (sonorant_vec.S3CF1 %*% t(ones) - ones %*% t(sonorant_vec.S3CF1))^2
  
  consonantal_vec.S3CF1 <- element$S3CF1.Consonantal
  consonantal_mat.S3CF1 <- (consonantal_vec.S3CF1 %*% t(ones) - ones %*% t(consonantal_vec.S3CF1))^2
  
  voice_vec.S3CF1 <- element$S3CF1.Voice
  voice_mat.S3CF1 <- (voice_vec.S3CF1 %*% t(ones) - ones %*% t(voice_vec.S3CF1))^2
  
  nasal_vec.S3CF1 <- element$S3CF1.Nasal
  nasal_mat.S3CF1 <- (nasal_vec.S3CF1 %*% t(ones) - ones %*% t(nasal_vec.S3CF1))^2
  
  degree_vec.S3CF1 <- element$S3CF1.Degree
  degree_mat.S3CF1 <- (degree_vec.S3CF1 %*% t(ones) - ones %*% t(degree_vec.S3CF1))^2
  
  labial_vec.S3CF1 <- element$S3CF1.Labial
  labial_mat.S3CF1 <- (labial_vec.S3CF1 %*% t(ones) - ones %*% t(labial_vec.S3CF1))^2
  
  palatal_vec.S3CF1 <- element$S3CF1.Palatal
  palatal_mat.S3CF1 <- (palatal_vec.S3CF1 %*% t(ones) - ones %*% t(palatal_vec.S3CF1))^2
  
  pharyngeal_vec.S3CF1 <- element$S3CF1.Pharyngeal
  pharyngeal_mat.S3CF1 <- (pharyngeal_vec.S3CF1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3CF1))^2
  
  round_vec.S3CF1 <- element$S3CF1.Round
  round_mat.S3CF1 <- (round_vec.S3CF1 %*% t(ones) - ones %*% t(round_vec.S3CF1))^2
  
  tongue_vec.S3CF1 <- element$S3CF1.Tongue
  tongue_mat.S3CF1 <- (tongue_vec.S3CF1 %*% t(ones) - ones %*% t(tongue_vec.S3CF1))^2
  
  radical_vec.S3CF1 <- element$S3CF1.Radical
  radical_mat.S3CF1 <- (radical_vec.S3CF1 %*% t(ones) - ones %*% t(radical_vec.S3CF1))^2
  
  anterior_vec.S3CF1 <- element$S3CF1.Anterior
  anterior_mat.S3CF1 <- (anterior_vec.S3CF1 %*% t(ones) - ones %*% t(anterior_vec.S3CF1))^2
  
  mat.S3CF1 <- sonorant_mat.S3CF1 + 
    consonantal_mat.S3CF1 + 
    voice_mat.S3CF1 + 
    nasal_mat.S3CF1 + 
    degree_mat.S3CF1 + 
    labial_mat.S3CF1 + 
    palatal_mat.S3CF1 + 
    pharyngeal_mat.S3CF1 + 
    round_mat.S3CF1 + 
    tongue_mat.S3CF1 + 
    radical_mat.S3CF1 + 
    anterior_mat.S3CF1
  
  rownames(mat.S3CF1) <- element$GlossID
  colnames(mat.S3CF1) <- element$GlossID
  
  sonorant_vec.S3CF2 <- element$S3CF2.Sonorant
  sonorant_mat.S3CF2 <- (sonorant_vec.S3CF2 %*% t(ones) - ones %*% t(sonorant_vec.S3CF2))^2
  
  consonantal_vec.S3CF2 <- element$S3CF2.Consonantal
  consonantal_mat.S3CF2 <- (consonantal_vec.S3CF2 %*% t(ones) - ones %*% t(consonantal_vec.S3CF2))^2
  
  voice_vec.S3CF2 <- element$S3CF2.Voice
  voice_mat.S3CF2 <- (voice_vec.S3CF2 %*% t(ones) - ones %*% t(voice_vec.S3CF2))^2
  
  nasal_vec.S3CF2 <- element$S3CF2.Nasal
  nasal_mat.S3CF2 <- (nasal_vec.S3CF2 %*% t(ones) - ones %*% t(nasal_vec.S3CF2))^2
  
  degree_vec.S3CF2 <- element$S3CF2.Degree
  degree_mat.S3CF2 <- (degree_vec.S3CF2 %*% t(ones) - ones %*% t(degree_vec.S3CF2))^2
  
  labial_vec.S3CF2 <- element$S3CF2.Labial
  labial_mat.S3CF2 <- (labial_vec.S3CF2 %*% t(ones) - ones %*% t(labial_vec.S3CF2))^2
  
  palatal_vec.S3CF2 <- element$S3CF2.Palatal
  palatal_mat.S3CF2 <- (palatal_vec.S3CF2 %*% t(ones) - ones %*% t(palatal_vec.S3CF2))^2
  
  pharyngeal_vec.S3CF2 <- element$S3CF2.Pharyngeal
  pharyngeal_mat.S3CF2 <- (pharyngeal_vec.S3CF2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3CF2))^2
  
  round_vec.S3CF2 <- element$S3CF2.Round
  round_mat.S3CF2 <- (round_vec.S3CF2 %*% t(ones) - ones %*% t(round_vec.S3CF2))^2
  
  tongue_vec.S3CF2 <- element$S3CF2.Tongue
  tongue_mat.S3CF2 <- (tongue_vec.S3CF2 %*% t(ones) - ones %*% t(tongue_vec.S3CF2))^2
  
  radical_vec.S3CF2 <- element$S3CF2.Radical
  radical_mat.S3CF2 <- (radical_vec.S3CF2 %*% t(ones) - ones %*% t(radical_vec.S3CF2))^2
  
  anterior_vec.S3CF2 <- element$S3CF2.Anterior
  anterior_mat.S3CF2 <- (anterior_vec.S3CF2 %*% t(ones) - ones %*% t(anterior_vec.S3CF2))^2
  
  mat.S3CF2 <- sonorant_mat.S3CF2 + 
    consonantal_mat.S3CF2 + 
    voice_mat.S3CF2 + 
    nasal_mat.S3CF2 + 
    degree_mat.S3CF2 + 
    labial_mat.S3CF2 + 
    palatal_mat.S3CF2 + 
    pharyngeal_mat.S3CF2 + 
    round_mat.S3CF2 + 
    tongue_mat.S3CF2 + 
    radical_mat.S3CF2 + 
    anterior_mat.S3CF2
  
  rownames(mat.S3CF2) <- element$GlossID
  colnames(mat.S3CF2) <- element$GlossID
  
  sonorant_vec.S3CF3 <- element$S3CF3.Sonorant
  sonorant_mat.S3CF3 <- (sonorant_vec.S3CF3 %*% t(ones) - ones %*% t(sonorant_vec.S3CF3))^2
  
  consonantal_vec.S3CF3 <- element$S3CF3.Consonantal
  consonantal_mat.S3CF3 <- (consonantal_vec.S3CF3 %*% t(ones) - ones %*% t(consonantal_vec.S3CF3))^2
  
  voice_vec.S3CF3 <- element$S3CF3.Voice
  voice_mat.S3CF3 <- (voice_vec.S3CF3 %*% t(ones) - ones %*% t(voice_vec.S3CF3))^2
  
  nasal_vec.S3CF3 <- element$S3CF3.Nasal
  nasal_mat.S3CF3 <- (nasal_vec.S3CF3 %*% t(ones) - ones %*% t(nasal_vec.S3CF3))^2
  
  degree_vec.S3CF3 <- element$S3CF3.Degree
  degree_mat.S3CF3 <- (degree_vec.S3CF3 %*% t(ones) - ones %*% t(degree_vec.S3CF3))^2
  
  labial_vec.S3CF3 <- element$S3CF3.Labial
  labial_mat.S3CF3 <- (labial_vec.S3CF3 %*% t(ones) - ones %*% t(labial_vec.S3CF3))^2
  
  palatal_vec.S3CF3 <- element$S3CF3.Palatal
  palatal_mat.S3CF3 <- (palatal_vec.S3CF3 %*% t(ones) - ones %*% t(palatal_vec.S3CF3))^2
  
  pharyngeal_vec.S3CF3 <- element$S3CF3.Pharyngeal
  pharyngeal_mat.S3CF3 <- (pharyngeal_vec.S3CF3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3CF3))^2
  
  round_vec.S3CF3 <- element$S3CF3.Round
  round_mat.S3CF3 <- (round_vec.S3CF3 %*% t(ones) - ones %*% t(round_vec.S3CF3))^2
  
  tongue_vec.S3CF3 <- element$S3CF3.Tongue
  tongue_mat.S3CF3 <- (tongue_vec.S3CF3 %*% t(ones) - ones %*% t(tongue_vec.S3CF3))^2
  
  radical_vec.S3CF3 <- element$S3CF3.Radical
  radical_mat.S3CF3 <- (radical_vec.S3CF3 %*% t(ones) - ones %*% t(radical_vec.S3CF3))^2
  
  anterior_vec.S3CF3 <- element$S3CF3.Anterior
  anterior_mat.S3CF3 <- (anterior_vec.S3CF3 %*% t(ones) - ones %*% t(anterior_vec.S3CF3))^2
  
  mat.S3CF3 <- sonorant_mat.S3CF3 + 
    consonantal_mat.S3CF3 + 
    voice_mat.S3CF3 + 
    nasal_mat.S3CF3 + 
    degree_mat.S3CF3 + 
    labial_mat.S3CF3 + 
    palatal_mat.S3CF3 + 
    pharyngeal_mat.S3CF3 + 
    round_mat.S3CF3 + 
    tongue_mat.S3CF3 + 
    radical_mat.S3CF3 + 
    anterior_mat.S3CF3
  
  rownames(mat.S3CF3) <- element$GlossID
  colnames(mat.S3CF3) <- element$GlossID
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  anterior_vec.S4C1 <- element$S4C1.Anterior
  anterior_mat.S4C1 <- (anterior_vec.S4C1 %*% t(ones) - ones %*% t(anterior_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1 + 
    anterior_mat.S4C1
  
  rownames(mat.S4C1) <- element$GlossID
  colnames(mat.S4C1) <- element$GlossID
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  anterior_vec.S4C2 <- element$S4C2.Anterior
  anterior_mat.S4C2 <- (anterior_vec.S4C2 %*% t(ones) - ones %*% t(anterior_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2 + 
    anterior_mat.S4C2
  
  rownames(mat.S4C2) <- element$GlossID
  colnames(mat.S4C2) <- element$GlossID
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  anterior_vec.S4C3 <- element$S4C3.Anterior
  anterior_mat.S4C3 <- (anterior_vec.S4C3 %*% t(ones) - ones %*% t(anterior_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3 + 
    anterior_mat.S4C3
  
  rownames(mat.S4C3) <- element$GlossID
  colnames(mat.S4C3) <- element$GlossID
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  anterior_vec.S4C4 <- element$S4C4.Anterior
  anterior_mat.S4C4 <- (anterior_vec.S4C4 %*% t(ones) - ones %*% t(anterior_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4 + 
    anterior_mat.S4C4
  
  rownames(mat.S4C4) <- element$GlossID
  colnames(mat.S4C4) <- element$GlossID
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  anterior_vec.S5C1 <- element$S5C1.Anterior
  anterior_mat.S5C1 <- (anterior_vec.S5C1 %*% t(ones) - ones %*% t(anterior_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1 + 
    anterior_mat.S5C1
  
  rownames(mat.S5C1) <- element$GlossID
  colnames(mat.S5C1) <- element$GlossID
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  anterior_vec.S5C2 <- element$S5C2.Anterior
  anterior_mat.S5C2 <- (anterior_vec.S5C2 %*% t(ones) - ones %*% t(anterior_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2 + 
    anterior_mat.S5C2
  
  rownames(mat.S5C2) <- element$GlossID
  colnames(mat.S5C2) <- element$GlossID
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  anterior_vec.S5C3 <- element$S5C3.Anterior
  anterior_mat.S5C3 <- (anterior_vec.S5C3 %*% t(ones) - ones %*% t(anterior_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3 + 
    anterior_mat.S5C3
  
  rownames(mat.S5C3) <- element$GlossID
  colnames(mat.S5C3) <- element$GlossID
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  anterior_vec.S5C4 <- element$S5C4.Anterior
  anterior_mat.S5C4 <- (anterior_vec.S5C4 %*% t(ones) - ones %*% t(anterior_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4 + 
    anterior_mat.S5C4
  
  rownames(mat.S5C4) <- element$GlossID
  colnames(mat.S5C4) <- element$GlossID
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  anterior_vec.SFC1 <- element$SFC1.Anterior
  anterior_mat.SFC1 <- (anterior_vec.SFC1 %*% t(ones) - ones %*% t(anterior_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1 + 
    anterior_mat.SFC1
  
  rownames(mat.SFC1) <- element$GlossID
  colnames(mat.SFC1) <- element$GlossID
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  anterior_vec.SFC2 <- element$SFC2.Anterior
  anterior_mat.SFC2 <- (anterior_vec.SFC2 %*% t(ones) - ones %*% t(anterior_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2 + 
    anterior_mat.SFC2
  
  rownames(mat.SFC2) <- element$GlossID
  colnames(mat.SFC2) <- element$GlossID
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  anterior_vec.SFC3 <- element$SFC3.Anterior
  anterior_mat.SFC3 <- (anterior_vec.SFC3 %*% t(ones) - ones %*% t(anterior_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3 + 
    anterior_mat.SFC3
  
  rownames(mat.SFC3) <- element$GlossID
  colnames(mat.SFC3) <- element$GlossID
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  anterior_vec.SFC4 <- element$SFC4.Anterior
  anterior_mat.SFC4 <- (anterior_vec.SFC4 %*% t(ones) - ones %*% t(anterior_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4 + 
    anterior_mat.SFC4
  
  rownames(mat.SFC4) <- element$GlossID
  colnames(mat.SFC4) <- element$GlossID
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S1CF1[,]) + 
    sqrt(mat.S1CF2[,]) + 
    sqrt(mat.S1CF3[,]) +
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) +    
    sqrt(mat.S2CF1[,]) + 
    sqrt(mat.S2CF2[,]) + 
    sqrt(mat.S2CF3[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S3CF1[,]) + 
    sqrt(mat.S3CF2[,]) + 
    sqrt(mat.S3CF3[,]) +
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})


# Take Euclidean distances from each infant's data and turn into a single dataframe

# Distance DF -------------------------------------------------------------

globaldistance_melted <- reshape2::melt(global_matrix) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  filter(gloss1 != gloss2) %>%
  rename(Speaker = L1)%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance <- as.data.frame(globaldistance_melted)

globaldistance_list <- list(globaldistance)

globaldistance_list_updated <- lapply(globaldistance_list, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) #%>%
    #filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  actual_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  actual_globaldistance <- actual_globaldistance_speaker %>%
    # mutate(maxdist = max(distance),
    #        distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
    #        data_type = "actual") %>%    
    #dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, .keep_all = TRUE) 
  actual_globaldistance_final <- list(actual_globaldistance)
})

globaldistance_final <- reshape2::melt(globaldistance_list_updated) %>%
  dplyr::select(-L1, -L2, -variable) %>%
  rename("distance" = value) %>%
  group_by(Speaker) %>%
  mutate(maxdist = max(distance),
         distance_norm = distance/maxdist) %>%
  dplyr::select(-maxdist)  %>%
  ungroup()

write_csv(globaldistance_final, "globaldistance_final.csv")
