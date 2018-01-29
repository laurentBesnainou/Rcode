#Données globales
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(D3partitionR)
library(DT)
library(data.table)
library(ggthemes)
# CONNECT TO AN AZURE SQL DATABASE
library(RODBC)           # Provides database connectivity
library(dplyr)           # only used for nice format of Head() function here
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

library(daff)


# Connectionstring > Lien vers la base qui heberge les données du Dashboard
connectionString <- 
  "Driver={SQL Server Native Client 11.0};
Server=tcp:airmis-colmngt-db.database.windows.net,1433;
Database=airmis-pilotage-prod;
Uid=pwbi
Pwd=4:j45_XcN4;
Encrypt=yes;
TrustServerCertificate=no;
Connection Timeout=30;"

#Ouverture de la connexion en ODBC
conn <- odbcDriverConnect('driver={SQL Server Native Client 11.0};server=airmis-colmngt-db.database.windows.net,1433;database=airmis-pilotage-prod;Uid=pwbi;Pwd=4:j45_XcN4;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')

#on charge les données historique CA et Staffing de la semaine passée
 load("HistoriqueCA.RData")
 load("HistoriqueStaffing.RData")

#on récupere les tables Pipe CA_Pipe et Staffing BT
bt_pipebt <- sqlQuery(conn, "SELECT * FROM bt_pipebt;")
bt_capipebt  <- sqlQuery(conn, "SELECT * FROM bt_capipebt;")
groupe_totem  <- sqlQuery(conn, "SELECT * FROM groupe_totem;")
groupe_offre  <- sqlQuery(conn, "SELECT * FROM groupe_offre;")
bt_effectif <- sqlQuery(conn, "SELECT * FROM bt_Effectifbt;")
bt_effectifGrade <- sqlQuery(conn, "SELECT * FROM bt_Effectifbt, groupe_grade where grade_id =groupe_grade.id ;")

bt_staffingbt  <- sqlQuery(conn, "SELECT * FROM bt_staffingbt;")

QueryCA <-  sqlQuery(conn, "SELECT bt_pipebt.id, groupe_offre.offre,annee,mission, ca_entite,ca_sst_interne,ca_sst_externe,ca_markup,probabilite, 
                     etape, offre_id, compte_id, responsable_id FROM bt_pipebt, bt_capipebt, groupe_offre 
                     WHERE pipe_id=bt_pipebt.id AND bt_pipebt.offre_id = groupe_offre.Id and annee=2018;")
bt_pipeTotem <- left_join(bt_pipebt, groupe_totem, by = c("totem_id" = "id"))
bt_pipeTotemCA <- left_join(bt_capipebt, bt_pipeTotem, by = c("pipe_id" = "id"))
#bt_pipeTotemCAStaffing <- left_join(bt_pipeTotemCA, bt_staffingbt, by = c("totem_id" = "totem_id"))
close(conn)

#on récupère le mois courrant our avoir le staffing sur 3 mois
mois <- month(today())

#On affiche le mois actif + les deu mois suivants
moisPlus1 <- if(mois<12) {mois+1} else {1}
moisPlus2 <- if(mois<11) {mois+2} else {2}

#Annee courante
annee <- year(today())
anneePlus1 <- if(mois>10) {annee+1} else {annee}




# #On va faire un graphe avec les pourcentages
# staff3mois %>% ggplot(aes(x=Mois,y=PFerme)) + 
#   
#   geom_bar(aes(y=PProvisoire),stat = "identity",fill="#FF0099", colour="black")+
#   geom_bar(position = "dodge",fill="#FF9999", colour="black")+
#   geom_hline(yintercept = .80, colour ="red",linetype="dotted")

#HistoriqueStaffing <- bt_staffingbt %>% mutate(Semaine=3)
#save(HistoriqueStaffing,file="HistoriqueStaffing.RData")