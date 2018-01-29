############### Fonctions utilisées dans le global.R
# Fonction qui renomme les colones des datframe en povenance d'Excel et de Google



#fonction qui remplace dans les colonnes 
SplitColonne <- function(x){
  strResultat <- strsplit(x,"->")
  if (length(strResultat[[1]]) >1) {
    SplitColonne <- strResultat[[1]][2]
  } else {
    SplitColonne <- x
  }
  trimws(SplitColonne)
}

#on va transformer le type des cellules car il y a une différence de type entre les datas Google ramenées
remplaceVal <- function (x) {
  x <- str_replace_all(x,",",".")
  x <- str_replace_all(x,"%",".")
  x
}