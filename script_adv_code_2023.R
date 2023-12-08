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

## DAY 2 ####
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
  

## DAY 3 ####
# ETOILE 1
#Chargement de l'input
input <- read_excel("data/input_3.xlsm", col_names = FALSE )

#récupération des emplacement des caractères spéciaux
data_caract <- input %>%
  rename (schema = ...1)%>%
  mutate(
    #ajouter un numéro identifiant
    n_ligne = row_number(),
    location = str_locate_all(schema, pattern = "[^[:alnum:]\\s.]" ))%>%
  unnest(location)%>%
  mutate(n_colonne = location[,1])%>%
  select(n_ligne, n_colonne)%>%
  #Définir la plage où les numériques seront valides
  mutate(n_ligne_prec = n_ligne - 1,
         n_ligne_suiv = n_ligne + 1,
         n_colonne_prec = n_colonne -1,
         n_colonne_suiv = n_colonne + 1,
         #définir un identifiant par caratère
         id_cara = row_number())%>%
  #pivot lignes
  pivot_longer(starts_with("n_ligne"),
               names_to = "lignes",
               values_to = "val_ligne")%>%
  #pivot colonnes
  pivot_longer(starts_with("n_colonne"),
               names_to = "colonnes",
               values_to = "val_colonne")%>%
  #selection colonnes
  select(val_ligne, val_colonne, id_cara)%>%
  #ajouter une valeur adjacent_symbole et une valeur ligne_col, un identifiant
  mutate(adjacent_symbole = 1,
         ligne_col = paste0(val_ligne, "_", val_colonne))


#Récupération des nombres et leur position
data_numerique <- input %>%
  rename (schema = ...1)%>%
  mutate(
    #ajouter un numéro identifiant
    n_ligne_nb = row_number(),
    #récupérer les localisation des nombres
    location = str_locate_all(schema, pattern = "\\d+"),
    #récupérer les nombres
    liste_num = str_extract_all(schema, pattern = "\\d+"),
    numerique_1 = map_int(liste_num, ~as.numeric(.x[1])),
    numerique_2 = map_int(liste_num, ~as.numeric(.x[2])),
    numerique_3 = map_int(liste_num, ~as.numeric(.x[3])),
    numerique_5 = map_int(liste_num, ~as.numeric(.x[5])),
    numerique_4 = map_int(liste_num, ~as.numeric(.x[4])),
    numerique_6 = map_int(liste_num, ~as.numeric(.x[6])),
    numerique_7 = map_int(liste_num, ~as.numeric(.x[7])),
    numerique_8 = map_int(liste_num, ~as.numeric(.x[8])),
    numerique_9 = map_int(liste_num, ~as.numeric(.x[9])),
    numerique_10 = map_int(liste_num, ~as.numeric(.x[10])),
    numerique_11 = map_int(liste_num, ~as.numeric(.x[11])),
    numerique_12 = map_int(liste_num, ~as.numeric(.x[12])),
    numerique_13 = map_int(liste_num, ~as.numeric(.x[13])),
    numerique_14 = map_int(liste_num, ~as.numeric(.x[14])),
    numerique_15 = map_int(liste_num, ~as.numeric(.x[15])),
    numerique_16 = map_int(liste_num, ~as.numeric(.x[16])),
    numerique_17 = map_int(liste_num, ~as.numeric(.x[17]))
    )%>%
  # Utiliser unnest pour séparer les listes
  unnest(location)%>%
  #récupérer la colonne de début et de fin de chaque nombre
  mutate(n_colonne_debut = location[,1],
         n_colonne_fin = location[,2])%>%
  select(n_ligne_nb, n_colonne_debut,n_colonne_fin, liste_num, numerique_1,
         numerique_2, numerique_3, numerique_4,
         numerique_5,numerique_6,numerique_7,numerique_8,numerique_9,
         numerique_10,numerique_11,numerique_12,numerique_13,numerique_14,numerique_15,
         numerique_16, numerique_17)

data_numerique_pos<- data_numerique %>%
  select(n_ligne_nb,n_colonne_debut, n_colonne_fin)%>%
  distinct()%>%
  #ajouter un identifiant
  mutate(id = row_number())

  data_numerique_tot <- data_numerique %>%
    pivot_longer(cols = starts_with("numerique_"),
               names_to = "position_liste",
               values_to = "numerique")%>%
    drop_na()%>%
    select(n_ligne_nb, numerique, position_liste)%>%
    #enlever les doublons
    distinct()%>%
    #ajouter un identifiant
    mutate(id = row_number())%>%
    #jointure par l'id
    left_join(data_numerique_pos)%>%
    #Ajouter les numéros de colonnes concernés
    mutate(n_colonne_milieu = ifelse(n_colonne_fin - n_colonne_debut == 2, n_colonne_debut + 1,
                                     n_colonne_debut))%>%
    #pivot des colonnes
    pivot_longer(starts_with("n_colonne"),
                 values_to = "num_colonne",
                 names_to = "type_colonne")%>%
    #retirer les champs inutiles
    select(-position_liste, -type_colonne)%>%
    distinct()%>%
    #création de l'indice ligne_colonne
    mutate(ligne_col = paste0(n_ligne_nb, "_", num_colonne))


#Jointure des tables numériques et symboles
  data_final <- data_numerique_tot %>%
    left_join(data_caract, by = "ligne_col")%>%
    #récupérer la somme des chiffres qui sont adjacent à un symbole
    select(id, numerique, adjacent_symbole)%>%
    drop_na()%>%
    distinct()%>%
    summarise(total = sum(numerique))

  # ETOILE 2
  
  #récupérer les numériques qui sont adjacent à un même symbole
  data_final <- data_numerique_tot %>%
    left_join(data_caract, by = "ligne_col")%>%
    drop_na()%>%
    select(id, numerique, id_cara)%>%
    distinct()%>%
    #regarder tous les caractères qui sont en double
    group_by(id_cara)%>%
    mutate(occurence_car = n())%>%
    filter(occurence_car > 1)%>%
    #multiplier par id_car les numériques
    summarise(multiplication = prod(numerique))%>%
    ungroup()%>%
    summarise(somme_multiplication = sum(multiplication))
    
    

## DAY 4 ####
  # ETOILE 1
  #Chargement de l'input
  input <- read_excel("data/input_4.xlsm", col_names = TRUE )

  # Enregistrez le temps de départ
  temps_debut <- Sys.time()
  
  #manip data
  #Récupérer la liste des numéro gagnants et la liste des numéro de la carte
  #prendre les numéros qui sont présents dans les 2 listes
  
  data_temp <-input %>%
    mutate(#extraires les nombres des 2 listes
      win_nb_liste = str_extract_all(win_nb, pattern = "\\d+"),
      card_nb_liste = str_extract_all(card_nb, pattern = "\\d+"),
      # regarder les nombres présents dans les 2 listes
      intersection = map2(win_nb_liste, card_nb_liste, ~intersect(.x,.y)),
      #compter le nb d'éléments communs
      nb_elements_com = lengths(intersection),
      #appliquer les scores en fonction du nb d'éléments communs
      score = case_when(nb_elements_com == 1 ~ 1,
                        nb_elements_com == 0 ~ 0,
                        .default = 2^(nb_elements_com - 1)),
      #faire la somme des score
      tot_points = sum(score)
      )
  
  # Enregistrez le temps à la fin de votre code
  temps_fin <- Sys.time()
  
  # Calculez la différence
  temps_execution <- as.numeric(difftime(temps_fin, temps_debut, units = "secs"))
  
  # Affichez le temps d'exécution en millisecondes
  cat("Temps d'exécution:", round(temps_execution , 6), "secondes\n")
     

  # ETOILE 2
  data_temp2 <- data_temp %>%
    select(num_card, nb_elements_com)%>%
    filter(nb_elements_com != 0)%>%
    mutate(num_deb = num_card,
           num_2= ifelse(nb_elements_com >=1, num_card +1, 0),
           num_3= ifelse(nb_elements_com >=2, num_card +2, 0),
           num_4= ifelse(nb_elements_com >=3, num_card +3, 0),
           num_5= ifelse(nb_elements_com >=4, num_card +4, 0),
           num_6= ifelse(nb_elements_com >=5, num_card +5, 0),
           num_7= ifelse(nb_elements_com >=6, num_card +6, 0),
           num_8= ifelse(nb_elements_com >=7, num_card +7, 0),
           num_9= ifelse(nb_elements_com >=8, num_card +8, 0),
           num_10 = ifelse(nb_elements_com >=9, num_card +9, 0),
           num_11 = ifelse(nb_elements_com >=10, num_card +10, 0)) %>%
      rename(numero_carte = num_card)%>%
     pivot_longer(starts_with("num_"),
                names_to = "numero",
                values_to = "nb_doubles"
                )%>%
    filter(nb_doubles != 0)%>%
    group_by(nb_doubles)%>%
    summarise(premier_multiplicateur = n())
    

  data_temp3 <- data_temp %>%
    select(num_card, nb_elements_com)%>%
    filter(nb_elements_com != 0)%>%
    mutate(num_deb = num_card,
           num_2= ifelse(nb_elements_com >=1, num_card +1, 0),
           num_3= ifelse(nb_elements_com >=2, num_card +2, 0),
           num_4= ifelse(nb_elements_com >=3, num_card +3, 0),
           num_5= ifelse(nb_elements_com >=4, num_card +4, 0),
           num_6= ifelse(nb_elements_com >=5, num_card +5, 0),
           num_7= ifelse(nb_elements_com >=6, num_card +6, 0),
           num_8= ifelse(nb_elements_com >=7, num_card +7, 0),
           num_9= ifelse(nb_elements_com >=8, num_card +8, 0),
           num_10 = ifelse(nb_elements_com >=9, num_card +9, 0),
           num_11 = ifelse(nb_elements_com >=10, num_card +10, 0)) %>%
    rename(numero_carte = num_card)%>%
    pivot_longer(starts_with("num_"),
                 names_to = "numero",
                 values_to = "nb_doubles"
    )


  # ETOILE 2 TEST ####
 matrice_gagnant <- data_temp %>%
    select(num_card, nb_elements_com)%>%
    filter(nb_elements_com != 0)%>%
    mutate(#num_initial = num_card,
           num_2= ifelse(nb_elements_com >=1, num_card +1, 0),
           num_3= ifelse(nb_elements_com >=2, num_card +2, 0),
           num_4= ifelse(nb_elements_com >=3, num_card +3, 0),
           num_5= ifelse(nb_elements_com >=4, num_card +4, 0),
           num_6= ifelse(nb_elements_com >=5, num_card +5, 0),
           num_7= ifelse(nb_elements_com >=6, num_card +6, 0),
           num_8= ifelse(nb_elements_com >=7, num_card +7, 0),
           num_9= ifelse(nb_elements_com >=8, num_card +8, 0),
           num_10 = ifelse(nb_elements_com >=9, num_card +9, 0),
           num_11 = ifelse(nb_elements_com >=10, num_card +10, 0)) %>%
      rename(numero_carte = num_card)%>%
    select(-nb_elements_com)
  
  data_copie1 <- matrice_gagnant%>%
    mutate(tot_copie0 = n())%>%
     pivot_longer(starts_with("num_"),
                names_to = "numero",
                values_to = "copie_1"
                )%>%
    filter(copie_1 != 0)%>%
    select(-numero) %>%
    #calculer le nombre de copie_1
    mutate(tot_copie1 = n())


  data_copie2 <- data_copie1 %>%
    left_join(matrice_gagnant, by = c("copie_1" = "numero_carte"))%>%
    pivot_longer(starts_with("num_"),
                 names_to = "numero",
                 values_to = "copie_2")%>%
    filter(copie_2 != 0)%>%
    select(-numero)%>%
  #calculer le nombre de copie_2
  mutate(tot_copie2 = n())
  
  ##TEST
  numero_copie_n1 <- 2
  
  data_copie2_test <- data_copie1 %>%
    left_join(matrice_gagnant, 
              by = setNames("numero_carte", paste0("copie_", numero_copie_n1 - 1)))%>%
              #by = c(paste0("copie_", numero_copie_n1 -1) = "numero_carte"))%>%
    pivot_longer(starts_with("num_"),
                 names_to = "numero",
                 values_to = paste0("copie_", numero_copie_n1) )%>%
    filter(across(everything(), ~. != 0)) %>%
    select(-numero)%>%
    #calculer le nombre de copie_2
    mutate(!!paste0("tot_copie_", numero_copie_n1) := n(),
           tot_copie = n())  
  nb_copie <- data_copie2_test$tot_copie[1]
  
  #Création d'une fonction pour ajouter une colonne de num de carte copiées
  calcul_copie <- function(numero_copie_n1, data_copie_n){
    data_copie_n1 <- data_copie_n %>% 
      mutate_all(as.character)%>%
      left_join(matrice_gagnant%>% mutate_all(as.character), 
                by = setNames("numero_carte", paste0("copie_", numero_copie_n1 - 1)))%>%
      #by = c(paste0("copie_", numero_copie_n1 -1) = "numero_carte"))%>%
      pivot_longer(starts_with("num_"),
                   names_to = "numero",
                   values_to = paste0("copie_", numero_copie_n1) )%>%
      filter(across(everything(), ~. != 0)) %>%
      select(-numero)%>%
      #calculer le nombre de copie_2
      mutate(!!paste0("tot_copie_", numero_copie_n1) := n())
      return(data_copie_n1)
  }
  
  data_copie3 <- calcul_copie(3, data_copie2)
  data_copie4 <- calcul_copie(4, data_copie3)
  data_copie5 <- calcul_copie(5, data_copie4)
  data_copie6 <- calcul_copie(6, data_copie5)
  data_copie4<-calcul_copie(4, data_copie3)
  data_copie5<-calcul_copie(5, data_copie4)
  data_copie6<-calcul_copie(6, data_copie5)
  data_copie7<-calcul_copie(7, data_copie6)
  data_copie8<-calcul_copie(8, data_copie7)
  data_copie9<-calcul_copie(9, data_copie8)
  data_copie10<-calcul_copie(10, data_copie9)
  data_copie11<-calcul_copie(11, data_copie10)
  data_copie12<-calcul_copie(12, data_copie11)
  data_copie13<-calcul_copie(13, data_copie12)
  data_copie14<-calcul_copie(14, data_copie13)
  data_copie15<-calcul_copie(15, data_copie14)
  data_copie16<-calcul_copie(16, data_copie15)
  data_copie17<-calcul_copie(17, data_copie16)
  data_copie18<-calcul_copie(18, data_copie17)
  data_copie19<-calcul_copie(19, data_copie18)
  data_copie20<-calcul_copie(20, data_copie19)
  data_copie21<-calcul_copie(21, data_copie20)
  data_copie22<-calcul_copie(22, data_copie21)

  
  data_final <- data_copie22%>%
    mutate_all(as.numeric)%>%
    pivot_longer(cols = starts_with("tot_copie"),
                 names_to = "copies",
                 values_to = "nb_copies")%>%
    summarise(somme_nb_copie = sum(nb_copies))
