library(ggvis)
library(dplyr)
library(class)####### KNN algorithm
library(gmodels)
library(rpart)
##########################
#regression linéaire     #
##########################
linkedin <- sample(35,21)
# Create the days vector
days <- seq(length(linkedin))

# Fit a linear model called on the linkedin views per day: linkedin_lm
linkedin_lm <- lm(linkedin ~ days  )


# Predict the number of views for the next three days: linkedin_pred
future_days <- data.frame(days = 22:24)
linkedin_pred <- predict(linkedin_lm,future_days)

# Plot historical data and predictions
plot(linkedin ~ days, xlim = c(1, 24))
points(22:24, linkedin_pred, col = "green")

#Clustering : diviser les points en k groupes
# Set random seed. Don't remove this line.
set.seed(1)

# Chop up iris in my_iris and species
my_iris <- iris[-5]
species <- iris$Species

# Perform k-means clustering on my_iris: kmeans_iris
kmeans_iris <- kmeans(my_iris, 3)

# Compare the actual Species to the clustering using table()
table(species, kmeans_iris$cluster)


# A decision tree model has been built for you
tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
              data = iris, method = "class")

unseen <- data.frame(Sepal.Length = c(5.3, 7.2),
                     Sepal.Width = c(2.9, 3.9),
                     Petal.Length = c(1.7, 5.4),
                     Petal.Width = c(0.8, 2.3))

# Predict the label of the unseen observations. Print out the result.
predict(tree,unseen,type="class")
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
irisNomalized <- iris
min(iris$Sepal.Length)

irisNomalized$Sepal.Length <- normalize(iris$Sepal.Length)
irisNomalized$Sepal.Width <- normalize(iris$Sepal.Width)
irisNomalized$Petal.Length <- normalize(iris$Petal.Length)
irisNomalized$Petal.Width <- normalize(iris$Petal.Width)


#Constitution du training Set 2/3 training et 1/3 test set

#on definit un nombre aléatoire qui permet d'avoir la meme repartition des echantillons a chaque exécution

set.seed(1234)
#on constitue es 2 chantillons (on définit les indice du dataset que l'on veut conserver)
ind <- sample(2, nrow(irisNomalized), replace=TRUE, prob=c(0.67, 0.33))

#données a étudier
iris.training <- irisNomalized[ind==1, 1:4]
iris.test <- irisNomalized[ind==2, 1:4]

#données a prédire
iris.trainLabels <- irisNomalized[ind==1, 5]
iris.testLabels <- irisNomalized[ind==2, 5]


# KNN  k-Nearest Neighbors (Euclidian distance measure )

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

#Evaluation du modèle
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE) 
# ce tableau permet de regarder l'écrart entre prédition et échantillon test

###################### Arbre de décision 
# http://mehdikhaneboubi.free.fr/random_forest_r.html#random-forest-avec-la-library-caret
library(randomForest)

lycee <- read.csv2("https://www.data.gouv.fr/s/resources/indicateurs-de-resultat-des-lycees-denseignement-general-et-technologique/20160401-163749/MEN-DEPP-indicateurs-de-resultats-des-LEGT-2015.csv", 
                   sep = ";", header = TRUE, fileEncoding = "ISO-8859-15", na.strings = c(" ","."))

nometab <- paste(lycee$Etablissement, lycee$Code.Etablissement)
nometab <- gsub("LYCEE ", "", nometab)
row.names(lycee) <- nometab

lycee2 <- select(lycee, Secteur.Public.PU.Privé.PR, Académie, Sructure.pédagogique.en.7.groupes, 
                 Taux.Brut.de.Réussite.Total.séries, Taux.Réussite.Attendu.toutes.séries, 
                 Effectif.de.seconde, Effectif.de.première, Effectif.de.terminale) %>% 
                mutate(Taux.Réussite.Attendu.toutes.séries=as.numeric(Taux.Réussite.Attendu.toutes.séries))

set.seed(123)

fit <- randomForest(Secteur.Public.PU.Privé.PR ~ ., data = lycee2, na.action = na.roughfix)

print(fit)

varImpPlot(fit)