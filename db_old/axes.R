#cirle sur les clients par rapport aux offres
#On constitue la matrice des connaissancess

library(circlize)
library(dplyr)
pilotage_2017 <- pilotage_data
tmp <- pilotage_2017 %>% filter( WEEK ==5, STEP %in% c(  "3 - Emise")) %>% select(GROUPE,OFFRE_PRINCIPALE,CA_BT__N__KE)


matriceConnais <- xtabs(CA_BT__N__KE~ GROUPE + OFFRE_PRINCIPALE, na.omit(tmp))

to <- paste(unique(colnames(matriceConnais)),sep = ",")
from <- paste(rownames(matriceConnais),sep = ",")
mat <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
col <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))

rownames(mat) = unique(from)
colnames(mat) = unique(to)
noms <- c(from,to)

names(gripCol) <- noms

for (i in 1:length(from)) {
  for (j in 1:length(to)) { 
    mat[i,j] <- matriceConnais[i,j]
  }
}
# for(i in from ) {
#   
#   if (i != input$consultant_id ) {
#     
#     col[which(from == i), 1] = "#FFFFFF00"
#     col[which(from == i), 2] = "#FFFFFF00"
#     col[which(from == i), 3] = "#FFFFFF00"
#     col[which(from == i), 4] = "#FFFFFF00"
#     col[which(from == i), 5] = "#FFFFFF00"
#   }
# }


#= = = = = initialize = = = = = #
  par(mar = c(1, 1, 1, 1))
circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))

# = = = = = plot 'circlize' = = = = = #
chordDiagram(mat, annotationTrack = "grid", transparency = 0.8,
             preAllocateTracks = list(track.height = 0.1),
             col = matrix(rainbow(nrow(mat)),nrow=nrow(mat),ncol=ncol(mat)),
             row.col = 1)

# = = = = = add labels = = = = = #
circos.trackPlotRegion(track.index = 1,
                       panel.fun = function(x, y) {
                         xrange = get.cell.meta.data("xlim")
                         labels = get.cell.meta.data("sector.index")
                         circos.text(mean(xrange), 0,
                                     labels = labels, niceFacing = TRUE)
                       },
                       bg.border = NA)
circos.clear()




circos.axis(mat, sector.index, track.index)
chordDiagram(mat,  
             
             directional = TRUE,
             niceFacing=TRUE,
    
             transparency = 0.2)


circos.clear()
circos.trackText(from,to,labels =union(from,to),
                 factors = union(from,to), 
                 col = "#EEEEEE", font = 2, facing = "downward")
# = = = = = add labels = = = = = #

factors = 1:20# just indicate there are 20 sectors
circos.par(gap.degree = 0, cell.padding =c(0, 0, 0, 0),start.degree = 360/20/2, 
           track.margin =c(0, 0), clock.wise = FALSE)
circos.initialize(factors = factors, xlim =c(0, 1))
circos.trackPlotRegion(ylim =c(0, 1), factors = factors, 
                       bg.col = "black",track.height = 0.15)
circos.trackText(rep(0.5, 20),rep(0.5, 20),
                 labels =c(13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10, 6),
                 factors = factors, col = "#EEEEEE", font = 2, facing = "bending.outside")
circos.trackPlotRegion(ylim =c(0, 1), factors = factors,bg.col =rep(c("#E41A1C", "#4DAF4A"), 10), 
                       bg.border = "#EEEEEE", 
                       track.height = 0.05)
circos.trackPlotRegion(ylim =c(0, 1), factors = factors,bg.col =rep(c("black", "white"), 10),
                       bg.border = "#EEEEEE", track.height = 0.275)
circos.trackPlotRegion(ylim =c(0, 1), factors = factors,bg.col =rep(c("#E41A1C", "#4DAF4A"), 10), 
                       bg.border = "#EEEEEE", track.height = 0.05)
circos.trackPlotRegion(ylim =c(0, 1), factors = factors,bg.col =rep(c("black", "white"), 10), 
                       bg.border = "#EEEEEE", track.height = 0.375)
draw.sector(center =c(0, 0), start.degree = 0, end.degree = 360,rou1 = 0.1, 
            col = "#4DAF4A", border = "#EEEEEE")
draw.sector(center =c(0, 0), start.degree = 0, end.degree = 360,rou1 = 0.05, col = "#E41A1C", border = "#EEEEEE")
