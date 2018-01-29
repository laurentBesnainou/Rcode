# CONNECT TO AN AZURE SQL DATABASE
library(RODBC)           # Provides database connectivity
library(dplyr)           # only used for nice format of Head() function here
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

#On va sauvegarder les données dans la semaine en cours

numSemaine <- week(now())



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

#on récupere les tables Pipe CA_Pipe et Staffing BT
bt_pipebt <- sqlQuery(conn, "SELECT * FROM bt_pipebt;")
bt_capipebt  <- sqlQuery(conn, "SELECT * FROM bt_capipebt;")
groupe_totem  <- sqlQuery(conn, "SELECT * FROM groupe_totem;")

bt_staffingbt  <- sqlQuery(conn, "SELECT * FROM bt_staffingbt;")


"[RODBC] ERROR: Could not SQLExecDirect 'SELECT annee,ca_entite,ca_sst_interne,ca_sst_externe,ca_markup,probabilite, 
etape, offre_id, compte_id, responsable_id FROM gbt_pipebt, bt_capipebt WHERE pipe_id=bt_pipebt.id;'"

QueryCA <-  sqlQuery(conn, "SELECT annee,mission, ca_entite,ca_sst_interne,ca_sst_externe,ca_markup,probabilite, 
                     etape, offre_id, compte_id, responsable_id 
                     FROM bt_pipebt, bt_capipebt WHERE pipe_id=bt_pipebt.id AND annee=2017;")

# Merge 
QueryCA %>% filter (etape %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
  replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
  mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
  mutate (totalCAPondere = totalCA * probabilite) %>%
  group_by(etape) %>% summarise(total=sum(totalCA),totalPondere = sum(totalCAPondere))


QueryCA %>% filter (etape %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
  replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
  mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
  mutate (totalCAPondere = totalCA * probabilite) %>%
  group_by(etape) %>% summarise(total=sum(totalCA))

data <- QueryCA %>% filter (etape %in% c("0 - A qualifier"))  %>%
replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
  mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) 

  mutate (TOTAL = ca_entite + ca_sst_interne )
bt_pipeTotem <- left_join(bt_pipebt, groupe_totem, by = c("totem_id" = "id"))
bt_pipeTotemCA <- left_join(bt_capipebt, bt_pipeTotem, by = c("pipe_id" = "id"))
bt_pipeTotemCAStaffing <- left_join(bt_pipeTotemCA, bt_staffingbt, by = c("totem_id" = "totem_id"))
test <- bt_pipeTotemCAStaffing %>% filter (activite=="1. Mission ferme")
sum(test$jh)
factor(bt_pipeTotemCAStaffing$activite)
#Missions gagnées pour POS100
pos100 <- bt_pipeTotemCA  %>% filter(totem=="POS100") %>% 
  replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
  mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
  select (mission,totalCA,totem,ca_entite,ca_sst_interne,ca_sst_externe,ca_markup)
  

CumulPos100 <- pos100 %>% summarise(total= sum(totalCA,na.rm=TRUE))
  
bt_staffingbt %>% group_by(nom) %>% summarise(jhTotal = sum(jh))
sum(bt_staffingbt$jh)
sum(pos100$ca_entite) /sum(bt_staffingbt$jh)
