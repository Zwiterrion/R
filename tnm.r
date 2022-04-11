
library(readxl)
Registre_poumon <- read.csv2('./poumons.csv')

possiblesN = c("y0","y1","y1a","y1b","y1c","y2a","y2b","y3","y4","y0","y1","y2","y3")
possiblesT = c("0", "1", "11a", "1mi", "1A")

Stade_TNM <- function(i, Tx, Nx, Mx, UICC) {
  
  # Si l'UICC est inconnu alors : 
  if(is.na(UICC)) {
    return(c("Version UICC non connue",""))}
  
  # Si l'UICC = 8 alors on rentre dans un  nouveau if/else : 
  else if (UICC == 8) {
    Tx <- paste("T", Tx, sep = "")
    Nx <- paste("N", Nx, sep = "")
    
    # Première étape : importer le tableau de classification TNM correspondant à la bonne version UICC. 
    Tableau_TNM <- read.csv2("./tnm8.csv")
    # Si le tableau_TNM n'est pas mis sous forme de data-frame, voici le message d'erreur : Warning message:
    # Setting row names on a tibble is deprecated. 
    Tableau_TNM <- as.data.frame(Tableau_TNM)
    row.names(Tableau_TNM) <-  c("T1a", "T1b", "T1c", "T2a", "T2b", "T3", "T4")

    
    if (Nx == "" & Nx== "" & Mx== "" ){
      return(c("on utilise le cTNM",""))
    } 
    else if (Nx == "0" & Nx== "0" & Mx== "0" ){
      return(c("on utilise le cTNM",""))
    }
    else if (Nx %in% possiblesN) {
      return(c("on utilise le cTNM", "neo-adjuvant"))
    }
    ## on utilise le pTNM
    else { 
      #### Trouver le M #### 
      # Si le M est différent de NULL ou différent de 8 dans ce cas : 
      if (!is.na(Mx) & Mx != "" & Mx != "8") {
        if (Mx == "1c") {
          return(c("Stade IV-B", ""))
        }
        else if (Mx == "1") {
          return(c("Stade IV-B", "imprécis")) 
        }
        else {
          return(c("Stade IV-A", ""))
        }
      }
      else if (is.na(Mx) | Mx != "y1a") { return(c("Voir le cTNM","")) }
      else {
        if(Tx %in% possiblesT){ 
          Tx <- "T1a"}
        else if (Tx == "2") { 
          Tx <- "2a"}
        return(c(Tableau_TNM[Tx,Nx],""))
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
  
  Tx <- Registre_poumon[i,"PT"]
  Nx <- Registre_poumon[i,"PN"]
  Mx <- Registre_poumon[i,"PM"]
  UICC <- Registre_poumon[i,"UICC"]
  
  print(c("Line", Tx, Nx, Mx, UICC))
  tnm <- Stade_TNM(i, Tx, Nx, Mx, UICC)
  Registre_poumon[i,"STADE"] <- tnm[1]
  Registre_poumon[i,"STADE_PRECISION"] <- tnm[2]
  
  print(c(i, tnm))
}



####################### VERIFICATIONS #########################

###### Différentes valeurs de PM : #######
#   table(Registre_poumon[,"PM"], useNA = "always")
#     1    1a    1b    1c     8   y1a  <NA> 
#   435   472   839   356     2     1 13483



###### Différentes valeurs de PT : ######
# 0     1   11a    1a    1A    1b    1c   1mi     2    24    2a    2b     3     4     7     8     x    y0    y1   y1a   y1b   y1c 
# 1   184     1   488     1   486   136     6   234     1   676   199   460   189     1   258     1    10     9    13     9     5 
# 
#   y2   y2a   y2b    y3    y4  yt2a  <NA> 
#   12    24     8    28    15     1 12132
