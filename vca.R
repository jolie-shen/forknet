install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg", "VCA"))

library(VCA)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)

set.seed(5)
setwd("~/Downloads")

normalglom <- read.csv("~/downloads/normalglom.csv", header = TRUE, 
  colClasses = c("factor", "factor", "numeric"))

#take out zeros
normalglom_nozero <- normalglom[normalglom$FPW != 0, ]

varPlot(FPW~Biopsy/Glom, normalglom_nozero,
        BG=list(var="Biopsy", col=paste0("gray", c(100, 80, 60))),
        Points=list(pch=16, col= "red", cex=1.25),
        MeanLine=list(var=c("int", "Biopsy"), col=c("black", "orange"), lwd=c(2,2)),
        Mean=list(col="cyan", cex=1.25, lwd=2), las=1,
        YLabel=list(text="FPW", las=0, line=3, font=2, cex=1.25),
        Title=list(main="Variance Component Analysis", cex.main=1.75),
        VCnam=list(font=2, cex=1.5),
        VarLab=list(list(cex=0.5, font=2), list(cex=0.25, font=2)))

#use if data is too long
sample <-normalglom_nozero[sample(nrow(normalglom_nozero), 5000), ]

#Fit using ANOVA
fit.anova <- fitVCA(FPW~Biopsy/Glom, normalglom)
fit.anova

#estimate 95% confidence intervals, request CI for all variance components via 'VarVC=TRUE'
inf <- VCAinference(fit.anova, VarVC=TRUE)
inf

#Fit using REML

fit.reml <- fitVCA(FPW~Biopsy/Glom, normalglom, method = "REML")
fit.reml



####WITH NEW LAYER IF WE GET INDIVIDUAL FOOT PROCESS DATA

varPlot(FPW~Biopsy/Glom, normalglom,
    BG=list(var="Biopsy", col=paste0("gray", c(100, 80, 60))),
    Points=list(pch=16, col=asRGB("black", .5), cex=1.25),
    MeanLine=list( var=c("int", "Biopsy", "Glom"),
    col=c("black", "orange", "blue"),
    lwd=c(2,2,2)),
    Mean=list(col="cyan", cex=1.25, lwd=2), las=1,
    YLabel=list(text="FPW", las=0, line=3, font=2, cex=1.25),
    Title=list(main="Variance Component Analysis", cex.main=1.75),
    VCnam=list(font=2, cex=1.5),
    # controls for which variable vertical lines are added between levels
    # and how these are plotted
    VLine=list(var="Glom", col="gray75"),
    VarLab=list(list(cex=1.5), list(cex=1.25), list(cex=1.25)))


##doing the same for Fabry
fabryglom <- read.csv("~/downloads/fabryglom.csv", header = TRUE, 
  colClasses = c("factor", "factor", "numeric"))

fabrygom_nozero <- fabryglom[fabryglom$FPW != 0, ]

varPlot(FPW~Biopsy/Glom, fabryglom_nozero,
        BG=list(var="Biopsy", col=paste0("gray", c(100, 80, 60))),
        Points=list(pch=16, col= "red", cex=1.25),
        MeanLine=list(var=c("int", "Biopsy"), col=c("black", "orange"), lwd=c(2,2)),
        Mean=list(col="cyan", cex=1.25, lwd=2), las=1,
        YLabel=list(text="FPW", las=0, line=3, font=2, cex=1.25),
        Title=list(main="Variance Component Analysis for Fabry", cex.main=1.75),
        VCnam=list(font=2, cex=1.5),
        VarLab=list(list(cex=0.5, font=2), list(cex=0.25, font=2)))

#Fit using ANOVA
fit.anova.fabry <- fitVCA(FPW~Biopsy/Glom, fabryglom_nozero)
fit.anova

#estimate 95% confidence intervals, request CI for all variance components via 'VarVC=TRUE'
inf.fabry <- VCAinference(fit.anova.fabry, VarVC=TRUE)
inf.fabry
