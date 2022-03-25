1. crée ton fichier csv avec les adresses postales
2. itère dessus avec une boucle for pour faire un appel à https://pyris.datajazz.io/api/search/?geojson=false&q= et récupére le `complete_code` et le `city`
3. Stocke dans chaque tour de boucle ces infos dans deux tableaux différentes, villes et codeIris par exemple
4. Affiche la carte avec le code du dessus.

````r
library(sf)

adresses <- read.csv('./adresses.csv')
colnames(adresses)

villes <- c()
codeIris <- c()

for (i in 1:nrow(adresses)) {
  adresse <- adresses[i, "ADRESSE"]
  adresseFormattePourUneUrl <- gsub(" ", "+", adresse)
  #url <- paste("https://api-adresse.data.gouv.fr/search/?q=",adresseFormattePourUneUrl, sep= "", collapse="")
  
  url <- paste("https://pyris.datajazz.io/api/search/?geojson=false&q=",adresseFormattePourUneUrl, sep= "", collapse="")
  
  r <- GET(url)
  
  # print(paste('Code HTTP de la réponde HTTP', status_code(r), sep = " : "))

  body <- content(r, as="parsed")

  complete_code <- body$complete_code
  city <- body$city

  print(paste(c(complete_code, city))
  
  villes <- append(villes, city)
  codeIris <- append(codeIris, complete_code)
}

nc = st_read("CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2021", )
print(colnames(nc))

cities = subset(nc, nc$NOM_COM %in% villes)
irisDeMesAdresses = subset(nc, nc$CODE_IRIS %in% codeIris)

plot(st_geometry(cities))
plot(st_geometry(irisDeMesAdresses), pch = 3, col = 'blue', add = TRUE)
````
