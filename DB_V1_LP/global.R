#Données globales
library(shiny)
library(shinyBS)
library(shinyLP)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(stringr)
library(highcharter) # courbe de comparaison des CA de 2015 à 2017
library(ggplot2)
library(ggthemes)
library(RColorBrewer) #coulors pour ggplot
library(lubridate)
library(modelr)
library(splines)
library(tidyverse)
library(DT)
library(data.table)
library(D3partitionR)
###### Package pour définir des prévisions
library(timetk)
library(tidyquant)
#library(h20)
#chargement des données
load("data/pilotage2016_data.RData")
load("data/pilotage2015_data.RData")

#données 2017
load("data/pilotage_data.RData")
load("data/staffing2017.RData")
nbJourMois <- c( 22,20,23,19,20,21,20,22,21,22,21,20,22,20,22)
pilotage_2017 <- pilotage_data

Objectif_BT <- 11120

#lissage de l'objectif
objectif_Vente <-c(528,1056,1584,2112,2640,3000,3360,3720,4080,4248,4416,4584,4752,4920,
                   5220,5520,5820,6120,6192,6264,6336,6408,6480,6690,6900,7110,7320,7500,
                   7680,7860,8040,8208,8376,8544,8712,8880,9210,9540,9870,10200,10440,10680,
                   10920,11160,11304,11448,11592,11736,11880,11940,12000,12060,12120)




objectif_Prod <- c(528,	1056,	1584,	2112,	2640,	3000,	3360,	3720,	4080,	4248,	4416,	4584,
                  4752,	4920,	5220,	5520,	5820,	6120,	6210,	6300,	6390,	6480,	6648,	6816,
                  6984,	7152,	7320,	7500,	7680,	7860,	8040,	8208,	8376,	8544,	8712,	8880,
                  9210,	9540,	9870,	10200,	10440,	10680,	10920,	11160,	11304,	11448,
                  11592,	11736,	11880,	11960,	12040,	12120)


################ on calcul les nombre de jours

#Calcul des effectifs moyens sur le mois
nbM1 <- Staffing %>% filter (TYPE == 0, !is.na(JANV)) %>% group_by(GRADE) %>% count(GRADE) %>% mutate(MOIS = "JANV")
nbM2 <- Staffing %>% filter (TYPE == 0, !is.na(FEV)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "FEV")
nbM3 <- Staffing %>% filter (TYPE == 0, !is.na(MAR)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "MAR")
nbM4 <- Staffing %>% filter (TYPE == 0, !is.na(AVR)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "AVR")
nbM5 <- Staffing %>% filter (TYPE == 0, !is.na(MAI)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "MAI")
nbM6 <- Staffing %>% filter (TYPE == 0, !is.na(JUIN)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "JUIN")
nbM7 <- Staffing %>% filter (TYPE == 0, !is.na(JUIL)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "JUIL")
nbM8 <- Staffing %>% filter (TYPE == 0, !is.na(AOUT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "AOUT")
nbM9 <- Staffing %>% filter (TYPE == 0, !is.na(SEPT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "SEPT")
nbM10 <- Staffing %>% filter (TYPE == 0, !is.na(OCT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "OCT")
nbM11 <- Staffing %>% filter (TYPE == 0, !is.na(NOV)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "NOV")
nbM12 <- Staffing %>% filter (TYPE == 0, !is.na(DEC)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "DEC")
#CALCUL DU NOMBRE DE JOURS MAX POSSIBLE
nbM1$volume <- nbM1$n * nbJourMois[1] 
nbM2$volume <- nbM2$n * nbJourMois[2] 
nbM3$volume <- nbM3$n * nbJourMois[3] 
nbM4$volume <- nbM4$n * nbJourMois[4] 
nbM5$volume <- nbM5$n * nbJourMois[5] 
nbM6$volume <- nbM6$n * nbJourMois[6] 
nbM7$volume <- nbM7$n * nbJourMois[7] 
nbM8$volume <- nbM8$n * nbJourMois[8] 
nbM9$volume <- nbM9$n * nbJourMois[9] 
nbM10$volume <- nbM10$n * nbJourMois[10] 
nbM11$volume <- nbM11$n * nbJourMois[11] 
nbM12$volume <- nbM12$n * nbJourMois[12] 


nbEffecti <- rbind(nbM1, nbM2, nbM3, nbM4, nbM5, nbM6, nbM7, nbM8, nbM9, nbM10, nbM11, nbM12)

#on calcul le nombre de jours vendus par grade et par mois  FERME
nbF1 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
nbF2 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
nbF3 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
nbF4 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
nbF5 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
nbF6 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
nbF7 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
nbF8 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
nbF9 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
nbF10 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
nbF11 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
nbF12 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
nbFERME <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)

#on calcul le nombre de jours vendus par grade et par mois  FERME
nbF1 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
nbF2 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
nbF3 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
nbF4 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
nbF5 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
nbF6 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
nbF7 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
nbF8 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
nbF9 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
nbF10 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
nbF11 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
nbF12 <- Staffing %>% filter (TYPE ==2) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
nbPREVI <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)

#on calcul le nombre de jours de congès + inactivité par grade et par mois
nbC1 <- Staffing %>% filter (TYPE %in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
nbC2 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
nbC3 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
nbC4 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
nbC5 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
nbC6 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
nbC7 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
nbC8 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
nbC9<- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
nbC10 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
nbC11 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
nbC12 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
nbCONGES <- rbind(nbC1, nbC2, nbC3, nbC4, nbC5, nbC6, nbC7, nbC8, nbC9, nbC10, nbC11, nbC12)



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
