## Advent of code 2023
## Chargement des libraries ####
library("tidyverse")
library(stringi)
library("readxl")

#Définition du dossier de travail
setwd("~/projets_git/adv_code/adv_code_2023")

## DAY 1 ####
#Chargement de l'input
input <- read.table("data/input1.txt")

#PREMIERE ETOILE
#Récupérer les chiffres numériques de toutes les lignes, extraire le 1 et le dernier chiffre 
#Prendre le premier et le dernier caractère puis faire la somme 
data_temp <- input %>%
  mutate(premier_u = str_sub(str_extract(V1,pattern = "\\d+"),1,1),
       dernier_u = str_sub(stri_extract_last(V1, regex = "\\d+"),-1,-1),
       somme = sum(as.numeric(paste0(premier_u, dernier_u))))

#DEUXIEME ETOILE
#Création d'un dataframe d'équivalence chiffres/lettres
conversion_chiffre <- c(1,1,2,2,3,3,4,5,6,7,8,8,8,9,9) 
conversion_lettre <-c("one","twone","two","eightwo","eighthree","three", 
                      "four" ,"five", "six", "seven", "nineight", "eight","oneight","sevenine", "nine") 
conversion <-tibble(bind_cols(conversion_chiffre, conversion_lettre)) %>%
rename(chiffre = ...1,
       lettre = ...2)

#Récupérer les chiffres  numériques et textuels 
data_temp <- input %>%
mutate(premier = str_extract(V1,pattern = "\\d+|one|two|three|four|five|six|seven|eight|nine"),
       dernier = stri_extract_last(V1, regex = "\\d+|oneight|twone|threeight|fiveight|sevenine|eightwo|eighthree|nineight|one|two|three|four|five|six|seven|eight|nine"))%>%
  #jointure pour récupérer les chiffres
  left_join(conversion, by = c("premier" = "lettre"))%>%
  left_join(conversion, by = c("dernier" = "lettre"))%>%
    mutate(
    #Extraction du premier chiffre et du dernier chiffre des premiers et derniers nombres sélectionnés
    premier_u = ifelse(is.na(chiffre.x), str_sub(premier,1,1), chiffre.x),
    dernier_u = ifelse(is.na(chiffre.y), str_sub(dernier,-1,-1), chiffre.y),
    #Somme des chiffres accolés
    somme = sum(as.numeric(paste0(premier_u, dernier_u))))

##DAY 2 ####
#Chargement de l'input
input <- read_excel("data/jour2.xlsm", col_names = FALSE )

# ETOILE 1
#Récupération des effectifs max par couleur
data_temp <- input %>%
  pivot_longer(cols = -...1,
               names_to = "tirage",
               values_to = "valeur" )%>%
  #séparer les effectifs des couleurs
  mutate(split_valeur = str_split(valeur, " "),
         effectif = map_int(split_valeur, ~ as.numeric(.x[1])),
         couleur = map_chr(split_valeur, ~ as.character(.x[2])))%>%
  #grouper par couleur/id pour récupérer le max
  group_by(...1, couleur) %>%
  summarise(max_effectif = max(effectif))%>%
  #enlever les NA
  drop_na()%>%
  #pivot
  pivot_wider(names_from = couleur,
              values_from = max_effectif)

data_temp1 <- data_temp %>%
  #Ajout d'une colonne pour vérifier si le tirage est possible
  #14 blue, 13 green, 12 red
  mutate(tirage_ok = ifelse(blue <= 14 & green <= 13 & red <= 12, 1,0))%>%
  #grouper par tirage ok
  group_by(tirage_ok)%>%
  #Sommer les id pour tous les tirages possibles
  summarise(tot = sum(...1))
 
# ETOILE 2

data_temp_2 <- data_temp %>%
  mutate(power = blue * green * red)%>%
  ungroup()%>%
  summarise(tot_power = sum(power))
  
