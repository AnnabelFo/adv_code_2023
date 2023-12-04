## Advent of code 2023
## Chargement des libraries ####
library("tidyverse")
library(stringi)

#Définition du dossier de travail
setwd("~/projets_git/adv_code/adv_code_2023")

## DAY 1 ####
#Chargement de l'input
input <- read.table("data/input1.txt")

#Récupérer les chiffres numériques de toutes les lignes 
#Extraire le 1 et le dernier chiffre 
#Prendre le premier et le dernier caractère 
#Faire la somme 
data_temp <- input %>%
  mutate(premier_u = str_sub(str_extract(V1,pattern = "\\d+"),1,1),
       dernier_u = str_sub(stri_extract_last(V1, regex = "\\d+"),-1,-1),
       somme = sum(as.numeric(paste0(premier_u, dernier_u))))

#Création d'un dataframe d'équivalence chiffres/lettres
conversion_chiffre <- c(1,1,2,2,3,3,4,5,6,7,8,8,9,9) 
conversion_lettre <-c("one","two", "three", "four" ,"five", "six", "seven", "eight","nine",) 
conversion <-tibble(bind_cols(conversion_chiffre, conversion_lettre)) %>%
rename(chiffre = ...1,
       lettre = ...2)

#Récupérer les chiffres  numériques et textuels 
data_temp <- input %>%
mutate(premier = str_extract(V1,pattern = "\\d+|one|two|three|four|five|six|seven|eight|nine"),
       dernier = stri_extract_last(V1, regex = "\\d+|one|two|three|four|five|six|seven|eight|nine"))%>%
  #jointure pour récupérer les chiffres
  left_join(conversion, by = c("premier" = "lettre"))%>%
  left_join(conversion, by = c("dernier" = "lettre"))%>%
  mutate(premier_u = ifelse(is.na(chiffre.x), str_sub(premier,1,1), chiffre.x),
         dernier_u = ifelse(is.na(chiffre.y), str_sub(dernier,-1,-1), chiffre.y),
         somme = sum(as.numeric(paste0(premier_u, dernier_u))))

stri_extract_last("qfvjvgeight2vbpjnftcttwonegn", regex =
                    "\\d+|one|two|three|four|five|six|seven|eight|nine|ne|ight|ine|wo|hree")

