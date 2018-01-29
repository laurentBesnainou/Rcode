library(shiny)
library(ggplot2)
library(shinyAce)
library(dplyr)
library(rpart)


#Liste des missions dans le pipe
data <- pilotage_data %>% filter (STEP %in% c("3 - Emise","3 - Emise" ), WEEK == max(WEEK) ) %>%
  select(COMPTE,SUJET,CA_BT__N__KE, STEP) 

#nombre de lignes
nbLine <- nrow(data)
#noeuds pour les missions
nodesMissions <- data.frame(id=1:nrow(data), label = data$SUJET, value = data$CA_BT__N__KE, 
                            title=paste(data$SUJET,"<br>",data$CA_BT__N__KE), group = data$COMPTE)
#On recupere les Comptes avec le montant associé
dataComptes <- data %>% group_by(COMPTE) %>% summarize(Total =sum(CA_BT__N__KE,na.rm =TRUE )) 
debut <- nrow(data)+1
fin <- debut + nrow(dataComptes) -1

nodesComptes <- data.frame(id=debut:fin,label = dataComptes$COMPTE,title = dataComptes$COMPTE, value =dataComptes$Total, group=dataComptes$COMPTE)
debut <- nrow(data)+1 + nbComptes
fin <- debut + 1

nodesStep <- data.frame(id=debut:fin,label =  c("2 - A émettre","3 - Emise"),
                        title=c("2 - A émettre","3 - Emise"),group= c("2 - A émettre", "3 - Emise" ),value=1)
#lien entre comptes et Step
nodes <- rbind(nodesMissions,nodesComptes,nodesStep)
data <- data%>% mutate(noeud = ifelse(STEP=="2 - A émettre",nbLine+1,nbLine+2))


liens <- merge(x = data, y = nodesComptes, by.x=c("COMPTE"), by.y=c("label"), all.x = TRUE)

edges <- data.frame(from = 1:nrow(data),
                    to = liens$id)


debut <- nrow(data)+1
fin <- debut + nrow(dataComptes) -1
LiensComptesStep <- data %>% select(COMPTE,STEP) %>% group_by(COMPTE,STEP) %>% distinct()
LiensComptesStep <- merge(x = LiensComptesStep, y = nodesComptes, by.x=c("COMPTE"), by.y=c("label"), all.x = TRUE)

edgesResultat <- merge(x = LiensComptesStep, y = nodesStep, by.x=c("STEP"), by.y=c("label"), all.x = TRUE)
edgesComptesStep <- data.frame(from = edgesResultat$id.x,
                               to = edgesResultat$id.y)
fin <- debut + nbComptes -1

edgesBis <- data.frame(from = 30:49,
                        to = rep(50,20))
edges <- rbind(edges,edgesComptesStep)
#on ajoute les liens sur les steps


shinyServer(function(input, output, session) {
  
  output$network_proxy <- renderVisNetwork({
    visNetwork(nodes, edges, main = "Répartition des propositions émises") %>%
      
      visGroups(groupname = "2 - A émettre", color = "red") %>%
      visLayout(randomSeed = 1234) %>%
    visGroups(groupname = "3 - Emise", color = "lightblue") 

     # visOptions(selectedBy = list(variable = "group", selected = nodes[1,1])) %>%
     # visLegend(width = 0.1, position = "right", main = "Group") 
    
    # visNetwork(nodes, edges, 
    #            height = "100%", width = "100%",
    #            main = "") %>%
    #   visEvents(click = "function(nodes){
    #             Shiny.onInputChange('click', nodes.nodes[0]);
    #             ;}" %>%
    #               visOptions(selectedBy = list(variable = "group")) )
                
      
})
  
  output$nodes_data_from_shiny <- renderDataTable({
    if(!is.null(input$network_proxy_nodes)){
      info <- data.frame(matrix(unlist(input$network_proxy_nodes), ncol = dim(nodes)[1],
                                byrow=T),stringsAsFactors=FALSE)
      colnames(info) <- colnames(nodes)
      info
    }
  })
  
  
 
  
})