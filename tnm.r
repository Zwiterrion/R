library(readxl)
Registre_poumon <- read_excel("R:/Masters/Stage_G_BARETH/etude_POUMON/poumon_selec_copie.xls")
library(operators) # Permet à R de lire les %!in%

bonnes_valeurs = c("0", "1", "1a", "1b", "1c", "1mi", "2", "2a", "2b", "3", "4")
neo_adujvant = c("y0","y1","y1a","y1b","y1c","y2a","y2b","y3","y4","y0","y1","y2","y3")
caduque <- c("", NA, "0", "8", "9","x")
imprecis <- c("", NA, "8", "9","x")

# Toutes les valeurs "fixes" que peuvent prendre le TNM. 
valeurs_possibles <- append(bonnes_valeurs, neo_adujvant)
valeurs_possibles_totales <- append(valeurs_possibles, caduque)


possiblesT = c("1", "1mi", "1A")

#possiblesN2 = c("2mi")

Stade_TNM <- function(i, cT, cN, cM, pT, pN, pM, UICC) {
  
  if (pT %!in% valeurs_possibles_totales | pN %!in% valeurs_possibles_totales | pM %!in% valeurs_possibles_totales) {
    if (pT %!in% valeurs_possibles_totales) {
      return(c("Erreur dans la codification du TNM", "revoir le pT"))
    }
    else if (pN %!in% valeurs_possibles_totales){
      return(c("Erreur dans la codification du TNM", "revoir le pN"))
    }
    else{
      return(c("Erreur dans la codification du TNM", "revoir le pM"))
    }
  }
  
  # Si l'UICC est inconnu alors : 
  else if(is.na(UICC)) {
    return(c("Version UICC non connue",""))}
  

  # Si l'UICC = 8 alors on rentre dans un  nouveau if/else : 
  else if (UICC == 8) {

    # Première étape : importer le tableau de classification TNM correspondant à la bonne version UICC. 
    Tableau_TNM <- read_excel("R:\\Masters\\Stage_G_BARETH\\etude_POUMON\\Classification_TNM_poumon.xlsx")
    
    # Si le tableau_TNM n'est pas mis sous forme de data-frame, voici le message d'erreur : Warning message:
    # Setting row names on a tibble is deprecated. 
    Tableau_TNM <- as.data.frame(Tableau_TNM)
    row.names(Tableau_TNM) <-  c("T1a", "T1b", "T1c", "T2a", "T2b", "T3", "T4")
    
   if (pT %in% caduque & pN %in% caduque & pM %in% caduque ){
      return(c("on utilise le cTNM",""))
    } 
    else if (pT %in% neo_adujvant | pN %in% neo_adujvant | pM %in% neo_adujvant  ) {
      return(c("on utilise le cTNM", "neo-adjuvant"))
    }
   
    ## on utilise le pTNM
    else { 
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
                }  else if (pT == 2) {
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
                  } else if (is.na(pT) | pT == "8") {
                  pT <- "T1a"
                  return(c(Tableau_TNM[pT,pN],"imprécis : M + T"))
                } else if (pT == 2) {
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
             } else if (is.na(pT) | pT == "8") {
               pN <- "N0"
               pT <- "T1a"
               return(c(Tableau_TNM[pT,pN],"imprécis : N + T"))
             } else if (pT == 2) {
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
             } else if (is.na(pT) | pT == "8") {
               pT <- "T1a"
               return(c(Tableau_TNM[pT,pN],"imprécis : T"))
             } else if (pT == 2) {
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
  }
  
  #Si l'UICC est une version antérieure alors : 
  else {
    return (
      c("Version UICC antérieure", "")
    )
  }
}

for (i in 1:nrow(Registre_poumon)) {
  cT <- Registre_poumon[i,"CT"]
  cN <- Registre_poumon[i,"CN"]
  cM <- Registre_poumon[i,"CM"]
  
  pT <- Registre_poumon[i,"PT"]
  pN <- Registre_poumon[i,"PN"]
  pM <- Registre_poumon[i,"PM"]
  
  UICC <- Registre_poumon[i,"UICC"]
  
  #print(c("Line", pT, pN, pM, UICC))
  tnm <- Stade_TNM(i, cT, cN, cM, pT, pN, pM, UICC)
  Registre_poumon[i,"STADE"] <- tnm[1]
  Registre_poumon[i,"STADE_PRECISION"] <- tnm[2]
  
  print(c(i, tnm))
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
