# Fichier Global.R

## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(circlize)
library(D3partitionR) # permet de générer le treemap dynamique
library(readxl)
library(ggthemes)
library(tidyr)
library(highcharter) # courbe dynamiques 7
library(RColorBrewer)

# 
# X2016 <- read_excel("D:/Data/anil/2017.xlsx",
#                       col_types = c("text", "text", "date",
#                                    "text", "numeric", "text", "numeric",
#                                   "numeric","text"))
# CB_Data <- X2016
# # # liste_Clients <- unique(CB_Data%>% select(Client) %>% arrange(Client))
#   save(CB_Data,file="CB_Data.Rdata")
# #On charge les données
load("CB_Data.Rdata")

liste_Clients <- unique(CB_Data%>% select(Client) %>% arrange(Client))
CB_Data <- CB_Data %>% mutate(ANNEE =  factor(ANNEE) )%>%
  mutate(MOIS =  factor(Mois))
CB_Mean <- CB_Data %>% filter (Type == "Nourriture") %>% group_by(ANNEE,MOIS) %>%
  summarise(Budget= sum(Montant)) %>%mutate(Budget = mean(Budget))
liste_type <-  sort(unique(CB_Data$Type))
liste_annee <-  sort(unique(CB_Data$ANNEE))
Clients  <-  sort(unique(CB_Data$Client))
# # On regarde pour la nourriture la comparaison par année
# CB_Data %>% filter (Type == "Nourriture") %>% group_by(ANNEE,MOIS) %>%
#   summarise(Budget= sum(Montant)) %>% 
#   # Overlaid histograms
#   ggplot(aes(x=MOIS, y=Budget)) +
#   geom_bar(aes( fill=ANNEE),stat="identity",position="dodge") +
#   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
# 
# #On regarde l'évolution des depenses Bio
# CB_Data %>% filter (Type == "Nourriture") %>% 
#   mutate(BIO = factor(ifelse(Client=="BIOCOOP","BIO","Non BIO"))) %>%
#   group_by(ANNEE, MOIS,BIO) %>% summarise(Budget = sum(Montant)) %>%
#   ggplot(aes(x=MOIS, y=Budget)) +
#   geom_bar(aes( fill=BIO),stat="identity") + facet_grid(. ~ ANNEE)
# 
# #On va faire un Graphe Circulaire avec BIO et Pas BIO pour voir la répartion
# CB_Data_Nourriture <- CB_Data %>% filter (Type == "Nourriture") %>% 
#   mutate(BIO = factor(ifelse(Client %in% c("BIOCOOP","BIOCBONDAUMESNIL"),"BIO","Non BIO"))) %>% Select(Categorie,Montant)
# 
# # CB_Data_Nourriture[grep("CASINO", CB_Data_Nourriture$Client), ]$Client <- "CASINO"
# 
# matrice <- xtabs(Montant ~ Categorie + BIO , na.omit(CB_Data_Nourriture))
# 
# to <- paste(unique(colnames(matrice)),sep = ",")
# from <- paste(rownames(matrice),sep = ",")
# mat <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
# col <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
# 
# rownames(mat) = unique(from)
# colnames(mat) = unique(to)
# noms <- c(from,to)
# group.colors <- c("Data" = "#60D394", "Digital Innovation" = "#AAF683","ETM" ="#FFD97D", "Transformation" = "#FF9B85", "PocLab" = "#ED7D3A")
# 
# # names(gripCol) <- noms
# for (i in 1:length(from)) {
#   
#   for (j in 1:length(to)) { 
#     mat[i,j] <- matrice[i,j]
#     col[,j] <- group.colors[i]
#     
#   }
# }
# 
# 
# #= = = = = initialize = = = = = #
# par(mar = c(1, 1, 1, 1))
# circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
# gripCol <- c(rep("#E69F00",length(unique(from))),rep("#56B4E9",length(unique(to))))
# # = = = = = plot 'circlize' = = = = = #
# c <- chordDiagram(mat, annotationTrack = "grid", transparency = 0.8,
#                   preAllocateTracks = list(track.height = 0.1),
#                   col = matrix(rainbow(nrow(mat)),nrow=nrow(mat),ncol=ncol(mat)),
#                   
#                   grid.col=gripCol)
# 
# # = = = = = add labels = = = = = #
# circos.trackPlotRegion(track.index = 1,
#                        panel.fun = function(x, y) {
#                          xrange = get.cell.meta.data("xlim")
#                          labels = get.cell.meta.data("sector.index")
#                          circos.text(mean(xrange), 0,
#                                      labels = labels, niceFacing = TRUE)
#                        },
#                        bg.border = NA)
# circos.clear()
# print(c)
# 
# 
# 
# 
# 
#   CB_Data_Nourriture <- CB_Data %>% filter (Type == "Nourriture", ANNEE=="2016") %>% 
#     mutate(BIO = factor(ifelse(Client %in% c("BIOCOOP","BIOCBONDAUMESNIL"),"BIO","Non BIO"))) %>% select(ANNEE, MOIS, Client,Categorie,Montant, BIO)
#   seq_Asso <- CB_Data_Nourriture %>% 
#     group_by(ANNEE, MOIS, BIO,Categorie,Client) %>%
#     summarize(CA = sum(Montant,na.rm=TRUE)) %>% mutate (DEBUT = "Dépenses 2016")
#   
#   
#   
#   nom <- list()
#   nb_1 <- length(seq_Asso$ANNEE)
#   
#   for (i in 1:nb_1) {
#     nom[[i]] <- c(seq_Asso$DEBUT[i],seq_Asso$Client[i] ,seq_Asso$Categorie[i],as.numeric(as.character(seq_Asso$MOIS[i])),
#                   paste(seq_Asso$Client[i],seq_Asso$CA[i],sep=": "))
#     
#     
#   }
#   CA_TOTAL <- c(seq_Asso$CA)
#   combo_output <- list(path = nom, value = CA_TOTAL)
#   
#   
#   D3partitionR( data=list(path=combo_output$path,value=combo_output$value),type="treeMap",
#                 tooltipOptions = list(showAbsolutePercent = T, showRelativePercent = T),
#                 width = 1200,height = 600
#                 )
#  


chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)




##' Plot flower plot
##' 
##' @param lengths length of petal outward to extent of circle
##' @param widths width of petal
##' @param labels petal label outside of circel
##' @param disk relative radius of a central donut hole
##' @param max.length ...
##' @param center center value
##' @param main middle value
##' @param fill.col fill colors
##' @param plot.outline size of plot outline
##' @param label.offset label offset
##' @param xlim formatting
##' @param ylim formatting
##' @param uin formatting
##' @param tol formatting
##' @param cex size of middle text
##' @param bty formatting
##' @param lty line thickness
##' @param label.col label color
##' @param label.font label font
##' @param label.cex size of label text
##' @return Generate something akin to a rose plot in which the width and
##' length of each petal are directly specified by the user. Or to put it
##' differently, this is somewhat like a pie chart in which the radius of each
##' wedge is allowed to vary (along with the angular width, as pie charts do).
##' As an additional enhancement, one can specify a central disk of arbitrary
##' radius (from 0 to 1, assuming that the plot itself is scaled to the unit
##' circle), in which case the petal heights are always measured from the edge
##' of the disk rather than the center of the circle; if desired, text can be
##' added in the center.
##' 
##' Although this kind of plot may already be well known in some circles (no
##' pun intended), I haven't seen it clearly defined or labeled anywhere, so
##' I'm anointing it an 'aster' plot because its component parts are
##' reminiscent of composite flower morphology.
##' 
##' The 'lengths' dictates how far out each petal extends, 'widths' dictates
##' the (angular) width of each petal, and 'disk' gives the relative radius of
##' a central donut hole. If no widths are provided, all petals will have equal
##' widths. Additional function arguments can also control whether petals are
##' labeled, whether the petal lengths are rescaled to the maximum score or to
##' a user-input score, whether spokes delineating each petal are extended to
##' an outer circle, and more. I also wrote a quick convenience wrapper for
##' creating a legend plot.
##' 
##' Note that the function here is a repurposed and very heavily modified
##' version of the windrose() function contained in the 'circular' package,
##' although sufficiently rewritten so as not to depend on any functionality in
##' that package.
##' @keywords layers_navigation
##' @author Created by Jim Regetz. Slight modifications by Darren Hardy and Ben Best.
##' @examples
##' 
##' \dontrun{
##' # generate some fake data
##' set.seed(1)
##' scores <- sample(1:10)
##' weights <- sample(1:10)
##' labels <- paste(LETTERS[1:10], "X", sep="")
##' 
##' # do some plots
##' par(mfrow=c(2,2), xpd=NA)
##' aster(lengths=scores, widths=weights, disk=0, main="Example 1",
##'     plot.outline=FALSE)
##' aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
##'     lty=2, fill.col="gray", plot.outline=FALSE)
##' aster.legend(labels=labels, widths=weights)
##' aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
##'     center="Hello world")
##' }
##' @import ggplot2
##' @export
PlotFlower = function (lengths, widths, labels, disk=0.5, max.length,
                       center=NULL, main=NULL, fill.col=NULL, plot.outline=TRUE,
                       label.offset=0.15, xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), uin=NULL,
                       tol=0.04, cex=1, bty="n", lty=1, 
                       label.col='black', label.font=3, label.cex=NULL, ...) {
  
  # Custom R function to generate something akin to a rose plot in which
  # the width and length of each petal are directly specified by the user.
  # Or to put it differently, this is somewhat like a pie chart in which
  # the radius of each wedge is allowed to vary (along with the angular
  # width, as pie charts do). As an additional enhancement, one can
  # specify a central disk of arbitrary radius (from 0 to 1, assuming that
  # the plot itself is scaled to the unit circle), in which case the petal
  # heights are always measured from the edge of the disk rather than the
  # center of the circle; if desired, text can be added in the center.
  #
  # Although this kind of plot may already be well known in some circles
  # (no pun intended), I haven't seen it clearly defined or labeled
  # anywhere, so I'm anointing it an 'aster' plot because its component
  # parts are reminiscent of composite flower morphology.
  #
  # As coded below, 'lengths' dictates how far out each petal extends,
  # 'widths' dictates the (angular) width of each petal, and 'disk' gives
  # the relative radius of a central donut hole. If no widths are
  # provided, all petals will have equal widths. Additional function
  # arguments can also control whether petals are labeled, whether the
  # petal lengths are rescaled to the maximum score or to a user-input
  # score, whether spokes delineating each petal are extended to an outer
  # circle, and more. I also wrote a quick convenience wrapper for
  # creating a legend plot.
  #
  # Note that the function here is a repurposed and very heavily modified
  # version of the windrose() function contained in the 'circular'
  # package, although sufficiently rewritten so as not to depend on any
  # functionality in that package.
  #
  # Example invocations appear below.
  #
  # Jim Regetz
  # NCEAS
  # Created on 13-Sept-2011
  #
  # Mods by Ben Best and Darren Hardy
  # December 2011
  #  - fix blank hairlines between circles and polygons in pedals
  #  - accepts more labeling and title options
  #  - accepts data frames for lengths
  #
  # Example plots...
  #
  # # generate some fake data
  # set.seed(1)
  # scores <- sample(1:10)
  # weights <- sample(1:10)
  # labels <- paste(LETTERS[1:10], "X", sep="")
  # 
  # # do some plots
  # png(file="aster-plots.png", height=600, width=600)
  # par(mfrow=c(2,2), xpd=NA)
  # aster(lengths=scores, widths=weights, disk=0, main="Example 1",
  #     plot.outline=FALSE)
  # aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
  #     lty=2, fill.col="gray", plot.outline=FALSE)
  # aster.legend(labels=labels, widths=weights)
  # aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
  #     center="Hello world")
  # dev.off()
  # main aster function definition
  
  if (is.data.frame(lengths)) {
    lengths <- as.numeric(lengths)
  }
  n.petals <- length(lengths)
  if (missing(widths)) {
    widths <- rep(1, n.petals)
  }
  if (missing(max.length)) {
    max.length <- max(lengths)
  }
  if (missing(labels)) {
    labels <- names(lengths)
  }
  if (missing(label.cex)) {
    label.cex <- 0.7 * cex
  }  
  
  # determine radius of each petal
  if (disk < 0 || 1 < disk) {
    stop("disk radius must be between 0 and 1")
  }
  radii <- disk + (1-disk) * lengths/max.length
  
  # define inner function for drawing circles
  # (from original windrose function)
  circles <- function(rad, sector=c(0, 2 * pi), lty=2,
                      col="white", border=NA, fill=FALSE) {
    values <- seq(sector[1], sector[2], by=(sector[2] - sector[1])/360)
    x <- rad * cos(values)
    y <- rad * sin(values)
    if (fill) {
      polygon(x, y, xpd=FALSE, lty=lty, col=col, border=border)
    }
    lines(x, y, col=1, lty=lty)
  }
  
  # lots of low-level positional details
  # (from original windrose function)
  op <- par(mar=c(1, 1, 2, 1))
  mai <- par("mai")
  on.exit(par(op))
  midx <- 0.5 * (xlim[2] + xlim[1])
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
  midy <- 0.5 * (ylim[2] + ylim[1])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
  oldpin <- par("pin") - c(mai[2] + mai[4], mai[1] + mai[3])
  xuin <- oxuin <- oldpin[1]/diff(xlim)
  yuin <- oyuin <- oldpin[2]/diff(ylim)
  if (is.null(uin)) {
    if (yuin > xuin) {
      xuin <- yuin
    } else {
      yuin <- xuin
    }
  } else {
    if (length(uin) == 1)
      uin <- uin * c(1, 1)
    if (any(c(xuin, yuin) < uin))
      stop("uin is too large to fit plot in")
    xuin <- uin[1]
    yuin <- uin[2]
  }
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  
  # generate breaks (petal boundaries) based on the widths
  breaks <- (2*pi*c(0, cumsum(widths))/sum(widths))[-(n.petals+1)]
  breaks <- c(breaks, 2 * pi)
  plot(c(-1.2, 1.2), c(-1.2, 1.2), xlab="", ylab="", main="",
       xaxt="n", yaxt="n", pch=" ", xlim=xlim, ylim=ylim,
       bty=bty, ...)
  title(main=main, ...)
  
  # plot full petal outlines
  if (plot.outline) {
    # note: go to n.petals not n.breaks because we the last break is
    # the same as the first
    for (i in 1:n.petals) {
      lines(c(0, cos(breaks[i])), c(0, sin(breaks[i])), lty=lty)
    }
    circles(1, lty=lty)
  }
  # plot the petals themselves
  if (is.null(fill.col)) {
    fill.col <- rainbow(n.petals)
  }
  fill.col <- rep(fill.col, length.out=n.petals)
  for (i in 1:n.petals) {
    w1 <- breaks[i]
    w2 <- breaks[i + 1]
    rad <- radii[i]
    xx <- rad * c(0, cos(w1), cos(w2), 0)
    yy <- rad * c(0, sin(w1), sin(w2), 0)
    polygon(xx, yy, xpd=FALSE, col=fill.col[i], border=fill.col[i])
    lines(xx[1:2], yy[1:2])
    lines(xx[3:4], yy[3:4])
    circles(rad=rad, sector=c(w1, w2), fill=TRUE,
            lty=1, col=fill.col[i], border=fill.col[i])
  }
  # plot petal labels, if given
  if (!is.null(labels)) {
    if (plot.outline) {
      height <- label.offset + rep(1, n.petals)
    } else {
      height <- label.offset + radii
    }
    mids <- breaks[1:n.petals] + diff(breaks)/2
    for (i in 1:n.petals) {
      text(height[i] * cos(mids[i]), height[i] * sin(mids[i]),
           labels=labels[i], cex=label.cex, 
           font=label.font, col=label.col)
    }
  }
  
  # add disk, if desired, with optional text in the middle
  if (0 < disk) {
    circles(disk, fill=TRUE, lty=1)
  }
  if (!is.null(center)) {
    text(0, 0, labels=center, font=2, cex=2.2*cex)
  }
  invisible(NULL)
}