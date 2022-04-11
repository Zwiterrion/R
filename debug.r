library(readxl)
Registre_poumon <- read_excel("R:/Masters/Stage_G_BARETH/etude_POUMON/poumon_selec_copie.xls")

Stade_TNM <- function(i, Tx, Nx, Mx) {
  
  # Si l'UICC est inconnu alors : 
  if(is.na(Registre_poumon[i,"UICC"])){
    return(Registre_poumon[i,"STADE"] <- "Version UICC non connue")}
  
  # Si l'UICC = 8 alors on rentre dans un  nouveau if/else : 
  else if (Registre_poumon[i,"UICC"] == 8) {
    
    Tx <- paste("T", Registre_poumon[i,"PT"], sep = "")
    Nx <- paste("N", Registre_poumon[i,"PN"], sep = "")
    
    # Première étape : importer le tableau de classification TNM correspondant à la bonne version UICC. 
    library(readxl)
    Tableau_TNM <- read_excel("R:/Masters/Stage_G_BARETH/etude_POUMON/Classification TNM du cancer du poumon par stades.xlsx")
    # Si le tableau_TNM n'est pas mis sous forme de data-frame, voici le message d'erreur : Warning message:
    # Setting row names on a tibble is deprecated. 
    Tableau_TNM <- as.data.frame(Tableau_TNM)
    row.names(Tableau_TNM) <-  c("T1a", "T1b", "T1c", "T2a", "T2b", "T3", "T4")
    
    if (Registre_poumon[i,"PT"] == "" & Registre_poumon[i,"PN"]== "" & Registre_poumon[i,"PM"]== "" ){
      return(Registre_poumon[i,"STADE"] <- "on utilise le cTNM")} 
    else if (Registre_poumon[i,"PT"] == "0" & Registre_poumon[i,"PN"]== "0" & Registre_poumon[i,"PM"]== "0" ){
      return(Registre_poumon[i,"STADE"] <- "on utilise le cTNM")}
    else if (Registre_poumon[i,"PT"] == "y0" | Registre_poumon[i,"PT"] == "y1" | Registre_poumon[i,"PT"] == "y1a" | Registre_poumon[i,"PT"] == "y1b" | Registre_poumon[i,"PT"] == "y1c" | Registre_poumon[i,"PT"] == "y2a" | Registre_poumon[i,"PT"] == "y2b" | Registre_poumon[i,"PT"] == "y3" | Registre_poumon[i,"PT"] == "y4" | Registre_poumon[i,"PN"] == "y0" | Registre_poumon[i,"PN"] == "y1" | Registre_poumon[i,"PN"] == "y2" | Registre_poumon[i,"PN"] == "y3") {
      return(Registre_poumon[i,"STADE"] <- "on utilise le cTNM",
      Registre_poumon[i,"STADE_precision"] <- "neo-adjuvant")
    }
    ## on utilise le pTNM
    else { 
    
    #### Trouver le M #### 
    # Si le M est différent de NULL ou différent de 8 dans ce cas : 
    if (Registre_poumon[i,"PM"] != "" & Registre_poumon[i,"PM"] != "8") {
      if (Registre_poumon[i,"PM"] == "1c") {
        return(Registre_poumon[i,"STADE"] <- "Stade IV-B")
        }
      else if (Registre_poumon[i,"PM"] == "1") {
        return(Registre_poumon[i,"STADE"] <- "Stade IV-B",
        Registre_poumon[i,"STADE_precision"] <- "imprécis") 
        }
      else {
        return(Registre_poumon[i,"STADE"] <- "Stade IV-A")
        }
    }
    else if (Registre_poumon[i,"PM"] != "y1a") { return( Registre_poumon[i,"STADE"] <- "Voir le cTNM") }
    else {
      if(Registre_poumon[i,"PT"] == "0" | Registre_poumon[i,"PT"] == "1" | Registre_poumon[i,"PT"] == "11a" | Registre_poumon[i,"PT"] == "1mi" | Registre_poumon[i,"PT"] == "1A"){ 
        Tx <- "T1a"}
      else if (Registre_poumon[i,"PT"] == "2") { 
        Tx <- "2a"}
      return(Registre_poumon[i,"STADE"] <- Tableau_TNM[Tx,Nx])
    }
    }
   }
  
  #Si l'UICC est une version antérieure alors : 
  else {return(Registre_poumon[i,"STADE"] <- "Version UICC antérieure")}
    
  }


for (i in 1:nrow(Registre_poumon)) {
  
  Tx <- Registre_poumon[i,"PT"]
  Nx <- Registre_poumon[i,"PN"]
  Mx <- Registre_poumon[i,"PM"]
    
  Stade_TNM(i, Tx, Nx, Mx)
 print(i)
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
