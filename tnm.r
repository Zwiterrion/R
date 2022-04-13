library(readxl)
Registre_poumon <- read_excel("R:/Masters/Stage_G_BARETH/etude_POUMON/poumon_selec.xls")
# Première étape : importer le tableau de classification TNM correspondant à la bonne version UICC. 
Tableau_TNM_8 <- read_excel("R:\\Masters\\Stage_G_BARETH\\etude_POUMON\\Classification_TNM_poumon.xlsx")
# Si le tableau_TNM n'est pas mis sous forme de data-frame, voici le message d'erreur : Warning message:
# Setting row names on a tibble is deprecated. 
Tableau_TNM_8 <- as.data.frame(Tableau_TNM_8)
row.names(Tableau_TNM_8) <-  c("T1a", "T1b", "T1c", "T2a", "T2b", "T3", "T4")



library(operators) # Permet à R de lire les %!in%

bonnes_valeurs = c("is", "0", "1", "1a", "1b", "1c", "1mi", "2", "2a", "2b", "3", "4")
neo_adujvant = c("y0","y1","y1a","y1b","y1c","y2a","y2b","y3","y4","y0","y1","y2","y3")
caduque <- c("", "vide", "0", "8", "9","x", "X")
caduque_sans_x <- c("", "vide", "0", "8", "9")
imprecis <- c("", "vide", "8", "9","x", "X")

# Toutes les valeurs "fixes" que peuvent prendre le TNM. 
valeurs_possibles <- append(bonnes_valeurs, neo_adujvant)
valeurs_possibles_totales <- append(valeurs_possibles, caduque)


possiblesT = c("0","1", "1mi", "1A")
#possiblesN2 = c("2mi")

################## Définition des fonctions :   ####################
Stade_clinique <- function(cT, cN, cM){
  
  # S'il n'y a aucune valeur pour le cTNM : 
  if (cT %in% caduque & cN %in% caduque & cM %in% caduque ){
    return(c("Stade non retrouvé", "aucune valeur présente"))
  }
  
  #### Trouver le M #### 
  # Si le M est différent de NULL ou différent de 8 dans ce cas : 
  else if (cM %!in% caduque) {
    if (cM == "1c") {
      return(c("IV-B", ""))
    }
    else if (cM == "1") {
      return(c("IV-B", "imprécis : M")) 
    }
    else {
      return(c("IV-A", ""))
    }
  }
  ##### Trouver le stade grâce au tableau et valeurs du pT et pN #####
  else {
  if(cM %in% imprecis){
    if(cN %in% imprecis){
      if (cT %in% possiblesT){
        cN <- "N0"
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : M + N"))
      }
      else if (cT %in% caduque) {
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : M + N + T"))
      }
      else if (cT == "2") {
        cN <- "N0"
        cT <- "T2a"
        return(c(Tableau_TNM[cT,cN],"imprécis : M + N + T"))
      }
      else {
        cN <- "N0"
        cT <- paste("T", cT, sep = "")
        return(c(Tableau_TNM[cT,cN],"imprécis : M + N"))
      }
    } 
    else {
      #cN = 0,1,2,3
      cN <- paste("N", cN, sep = "") 
      if (cT %in% possiblesT){
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : M"))
      } 
      else if (cT %in% imprecis) {
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : M + T"))
      } else if (cT == "2") {
        cT <- "T2a"
        return(c(Tableau_TNM[cT,cN],"imprécis : M + T"))
      }
      else {
        cT <- paste("T", cT, sep = "")
        return(c(Tableau_TNM[cT,cN],"imprécis : M"))
      }
    }
  } else {
    # Si M = 0 alors 
    if(cN %in% imprecis){
      if (cT %in% possiblesT){
        cN <- "N0"
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : N"))
      } else if (cT %in% imprecis) {
        cN <- "N0"
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : N + T"))
      } else if (cT == "2") {
        cN <- "N0"
        cT <- "T2a"
        return(c(Tableau_TNM[cT,cN],"imprécis : N + T"))
      } else {
        cN <- "N0"
        cT <- paste("T", cT, sep = "")
        return(c(Tableau_TNM[cT,cN],"imprécis : N"))
      }
    } else {
      #cN = 0,1,2,3
      cN <- paste("N", cN, sep = "")
      if (cT %in% possiblesT){
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],""))
      } else if (cT %in% imprecis) {
        cT <- "T1a"
        return(c(Tableau_TNM[cT,cN],"imprécis : T"))
      } else if (cT == "2") {
        cT <- "T2a"
        return(c(Tableau_TNM[cT,cN],"imprécis : T"))
      } else {
        cT <- paste("T", cT, sep = "")
        return(c(Tableau_TNM[cT,cN],""))
      }
    }
   }
  }
}

Stade_pathologique <- function(pT, pN, cM){
  
  #### Trouver le M #### 
  # Si le M est différent de NULL ou différent de 8 dans ce cas : 
  if (pM %!in% caduque) {
    if (pM == "1c") {
      return(c("IV-B", ""))
    }
    else if (pM == "1") {
      return(c("IV-B", "imprécis : M")) 
    }
    else {
      return(c("IV-A", ""))
    }
  }
  ##### Trouver le stade grâce au tableau et valeurs du pT et pN #####
  else {
      if(pM %in% imprecis){
        if(pN %in% imprecis){
          if (pT %in% possiblesT){
            pN <- "N0"
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],"imprécis : M + N"))
          }  else if (pT == "2") {
            pN <- "N0"
            pT <- "T2a"
            return(c(Tableau_TNM[pT,pN],"imprécis : M + N + T"))
          } else {
            pN <- "N0"
            pT <- paste("T", pT, sep = "")
            return(c(Tableau_TNM[pT,pN],"imprécis : M + N"))
          }
        } 
        else {
          #pN = 0,1,2,3
          pN <- paste("N", pN, sep = "") 
          if (pT %in% possiblesT){
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],"imprécis : M"))
          } 
          else if (pT %in% imprecis) {
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],"imprécis : M + T"))
          } else if (pT == "2") {
            pT <- "T2a"
            return(c(Tableau_TNM[pT,pN],"imprécis : M + T"))
          }
          else {
            pT <- paste("T", pT, sep = "")
            return(c(Tableau_TNM[pT,pN],"imprécis : M"))
          }
        }
      } else {
        # Si M = 0 alors 
        if(pN %in% imprecis){
          if (pT %in% possiblesT){
            pN <- "N0"
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],"imprécis : N"))
          } else if (pT %in% imprecis) {
            pN <- "N0"
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],"imprécis : N + T"))
          } else if (pT == "2") {
            pN <- "N0"
            pT <- "T2a"
            return(c(Tableau_TNM[pT,pN],"imprécis : N + T"))
          } else {
            pN <- "N0"
            pT <- paste("T", pT, sep = "")
            return(c(Tableau_TNM[pT,pN],"imprécis : N"))
          }
        } else {
          #pN = 0,1,2,3
          pN <- paste("N", pN, sep = "")
          if (pT %in% possiblesT){
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],""))
          } else if (pT %in% imprecis) {
            pT <- "T1a"
            return(c(Tableau_TNM[pT,pN],"imprécis : T"))
          } else if (pT == "2") {
            pT <- "T2a"
            return(c(Tableau_TNM[pT,pN],"imprécis : T"))
          } else {
            pT <- paste("T", pT, sep = "")
            return(c(Tableau_TNM[pT,pN],""))
          }
        }
      }
    }
  }


Stade_TNM <- function(cT, cN, cM, pT, pN, pM, UICC) {
  
  # Si il y a une erreur de codification: 
  if  (pT %!in% valeurs_possibles_totales) {
    return(c("Erreur dans la codification du TNM", "revoir le pT"))
  }
  else if (pN %!in% valeurs_possibles_totales){
    return(c("Erreur dans la codification du TNM", "revoir le pN"))
  }
  else if (pM %!in% valeurs_possibles_totales) {
    return(c("Erreur dans la codification du TNM", "revoir le pM"))
  }
  else if (cT %!in% valeurs_possibles_totales){
    return(c("Erreur dans la codification du TNM", "revoir le cT"))
  }
  else if (cN %!in% valeurs_possibles_totales){
    return(c("Erreur dans la codification du TNM", "revoir le cN"))
  }
  else if (cM %!in% valeurs_possibles_totales){
    return(c("Erreur dans la codification du TNM", "revoir le cM"))
  }
  
  # Si l'UICC est inconnu alors : 
  else if(UICC == "vide") {
    return(c("Version UICC non connue",""))}
  
  
  # Si l'UICC = 8 alors on rentre dans un  nouveau if/else : 
  else if (UICC == "8") {
    
    Tableau_TNM  <-  Tableau_TNM_8
    
    if (pT %in% caduque & pN %in% caduque & pM %in% caduque ){
      if (pT %!in% caduque_sans_x){
        return(c("Carcinome occulte",""))
      } else{
        return(Stade_clinique(cT, cN, cM))
      }
    }
    
    else if (pT %in% neo_adujvant | pN %in% neo_adujvant | pM %in% neo_adujvant  ) {
      return(Stade_clinique(cT, cN, cM))
    }
    
    else if (pT == "is"){
      if (pM %in% imprecis & pN %!in% imprecis) {
        return(c("0","imprécis : M"))
      } else if (pM %in% imprecis & pN %in% imprecis) {
        return(c("0","imprécis : M + N"))
      } else if (pM %!in% imprecis & pN %in% imprecis) {
        return(c("0","imprécis : N"))
      } else {
        return(c("0",""))
      }
    }
    ## on utilise le pTNM
    else { 
        return(Stade_pathologique(pT,pN,pM))
    }
  }
  
  #Si l'UICC est une version antérieure alors : 
  else {
    return (
      c("Version UICC antérieure", "")
    )
  }
}



for (i in 1:nrow(Registre_poumon)) {
  # Convertion des valeurs NA en une chaine de charatères et attribution des valeurs. 
  cT <- ifelse(is.na(Registre_poumon[i,"CT"]), "vide", Registre_poumon[i,"CT"])
  cN <- ifelse(is.na(Registre_poumon[i,"CN"]), "vide", Registre_poumon[i,"CN"])
  cM <- ifelse(is.na(Registre_poumon[i,"CM"]), "vide", Registre_poumon[i,"CM"])
  
  pT <- ifelse(is.na(Registre_poumon[i,"PT"]), "vide", Registre_poumon[i,"PT"])
  pN <- ifelse(is.na(Registre_poumon[i,"PN"]), "vide", Registre_poumon[i,"PN"])
  pM <- ifelse(is.na(Registre_poumon[i,"PM"]), "vide", Registre_poumon[i,"PM"])
  
  UICC <- ifelse(is.na(Registre_poumon[i,"UICC"]), "vide", Registre_poumon[i,"UICC"])
  
  #print(c("Line", pT, pN, pM, UICC))
  TNM <- Stade_TNM(cT, cN, cM, pT, pN, pM, UICC)
  Registre_poumon[i,"STADE"] <- TNM[1]
  Registre_poumon[i,"STADE_PRECISION"] <- TNM[2]
  
  print(c(i, TNM))
}


tableau_UICC8 <- subset(Registre_poumon, Registre_poumon$UICC == 8)
tableau_pTNM <- subset(tableau_UICC8, tableau_UICC8$STADE != "on utilise le cTNM")
tableau_cTNM <- subset(tableau_UICC8, tableau_UICC8$STADE == "on utilise le cTNM")
tableau_erreur <- subset(Registre_poumon, Registre_poumon$STADE == "Erreur dans la codification du TNM")

# table(Registre_poumon$STADE, useNA = "always")
# TTTT<- subset(Registre_poumon, Registre_poumon$PT == "7")

#write.csv2(Registre_poumon, file = "TNM_STADE_Final.csv")

####################### VERIFICATIONS #########################

###### Différentes valeurs de PM : #######
#   table(Registre_poumon[,"PM"], useNA = "always")
#     1    1a    1b    1c     8   y1a  <NA> 
#   435   472   839   356     2     1 13483


###### Différentes valeurs de PN : #######

#    0     1     2   2mi  2mic     3     8     x    y0    y1    y2  <NA> 
# 1976   539   829     2     1   387   258    37    59    33    28 11439 


###### Différentes valeurs de PT : ######

# 0     1   11a    1a    1A    1b    1c   1mi     2    24    2a    2b     3     4     7     8     x    y0    y1   y1a   y1b   y1c 
# 1   184     1   488     1   486   136     6   234     1   676   199   460   189     1   258     1    10     9    13     9     5 
# 
#   y2   y2a   y2b    y3    y4  yt2a  <NA> 
#   12    24     8    28    15     1 12132

######################### TEST de contrôle sur l'échantillon HR poumon ###################

A_comparer <- read_excel("R:/Masters/Stage_G_BARETH/etude_POUMON/Etude_poumon/HR_poumon.xlsx")
AVOIR <- merge(Registre_poumon, A_comparer, by = "NUM_IDTUMEUR")

LA_comparaison <- AVOIR[,c("NUM_IDTUMEUR", "STADE", "STADE_PRECISION", "STADE_clinique", "STADE_pathologique")]

for (i in 1:nrow(LA_comparaison)) {
  
  # Changement des valeurs NA en "XXX"
  ifelse(is.na(LA_comparaison[i,"STADE_clinique"]), LA_comparaison[i,"STADE_clinique"]<- "XXX", LA_comparaison[i,"STADE_clinique"] <- LA_comparaison[i,"STADE_clinique"])
  ifelse(is.na(LA_comparaison[i,"STADE_pathologique"]), LA_comparaison[i,"STADE_pathologique"]<- "XXX", LA_comparaison[i,"STADE_pathologique"] <- LA_comparaison[i,"STADE_pathologique"])
  
  # Mise en conformitée des STADES #
  ifelse(LA_comparaison[i,"STADE_clinique"] == "IA1", LA_comparaison[i,"STADE_clinique"] <- "IA-1", LA_comparaison[i,"STADE_clinique"] <- LA_comparaison[i,"STADE_clinique"])
  ifelse(LA_comparaison[i,"STADE_clinique"] == "IA2", LA_comparaison[i,"STADE_clinique"] <- "IA-2", LA_comparaison[i,"STADE_clinique"] <- LA_comparaison[i,"STADE_clinique"])
  ifelse(LA_comparaison[i,"STADE_clinique"] == "IA3", LA_comparaison[i,"STADE_clinique"] <- "IA-3", LA_comparaison[i,"STADE_clinique"] <- LA_comparaison[i,"STADE_clinique"])
  ifelse(LA_comparaison[i,"STADE_clinique"] == "IVA", LA_comparaison[i,"STADE_clinique"] <- "IV-A", LA_comparaison[i,"STADE_clinique"] <- LA_comparaison[i,"STADE_clinique"])
  ifelse(LA_comparaison[i,"STADE_clinique"] == "IVB", LA_comparaison[i,"STADE_clinique"] <- "IV-B", LA_comparaison[i,"STADE_clinique"] <- LA_comparaison[i,"STADE_clinique"])
  
  ifelse(LA_comparaison[i,"STADE_pathologique"] == "IA1", LA_comparaison[i,"STADE_pathologique"] <- "IA-1", LA_comparaison[i,"STADE_pathologique"] <- LA_comparaison[i,"STADE_pathologique"])
  ifelse(LA_comparaison[i,"STADE_pathologique"] == "IA2", LA_comparaison[i,"STADE_pathologique"] <- "IA-2", LA_comparaison[i,"STADE_pathologique"] <- LA_comparaison[i,"STADE_pathologique"])
  ifelse(LA_comparaison[i,"STADE_pathologique"] == "IA3", LA_comparaison[i,"STADE_pathologique"] <- "IA-3", LA_comparaison[i,"STADE_pathologique"] <- LA_comparaison[i,"STADE_pathologique"])
  ifelse(LA_comparaison[i,"STADE_pathologique"] == "IVA", LA_comparaison[i,"STADE_pathologique"] <- "IV-A", LA_comparaison[i,"STADE_pathologique"] <- LA_comparaison[i,"STADE_pathologique"])
  ifelse(LA_comparaison[i,"STADE_pathologique"] == "IVB", LA_comparaison[i,"STADE_pathologique"] <- "IV-B", LA_comparaison[i,"STADE_pathologique"] <- LA_comparaison[i,"STADE_pathologique"])
 }


LA_comparaison$STATUT <- ifelse(LA_comparaison$STADE == LA_comparaison$STADE_clinique, 'TRUE_clinque',
                                ifelse(LA_comparaison$STADE == LA_comparaison$STADE_pathologique, 'TRUE_patho', 'FALSE'))


TABLEAU_DES_FAUX <- subset(LA_comparaison, LA_comparaison$STATUT=="FALSE")

VERIFIER <- merge(Registre_poumon, TABLEAU_DES_FAUX, by = "NUM_IDTUMEUR")


#####################################################

