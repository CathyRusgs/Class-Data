## Getting Started Demo video 1b

## or just use an R project
setwd("~/classes/Censored data/Nondetects and Data Analysis/NADA Online 
      3_6/Class Data")

## Code > Source File > Loadlibs.R
## Load all packages for class
source("~/classes/Censored data/Nondetects and Data Analysis/NADA Online 
       3_6/Class Data/Loadlibs.R", encoding = 'UTF-8')

## From Freya - a script to load or install all needed packages at once
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Packages that I want
packages <- c("remotes",
              "tidyr",
              "tidyverse",
              "reshape2",
              "lme4",
              "rgl",
              "ggplot2",
              "modelr",
              "shinystan",
              "survival", ## The survivor package!!
              "survminer", ## for plotting in ggplot
              "gridExtra",
              "dplyr",
              "gtsummary",
              "scales",
              "viridisLite",
              "gghalves",
              "ggdist")

#Run the ipak loop
ipak(packages)

## Open Rdata file
load("~/classes/Censored data/Nondetects and Data Analysis/NADA Online 
     3_6/Class Data/Golden.rda")
View(Golden)

## bad advice
attach(Golden)

## import csv file
Golden2 <- read.csv("~/classes/Censored data/Nondetects and Data Analysis/NADA 
                    Online 3_6/Class Data/Golden2.txt", sep="")
View(Golden2)
attach(Golden2)

##calculations - use Tab to fill in data set and column
mean(Golden2$blood.Pb)
meanlead <- mean(Golden2$blood.Pb)


# Detection and Quantitation Limits video 2 -------------------------------

## Can report all data below LOQ as <LOQ
## Or report data between LOD and LOQ as "remarked values - 1.2E, 2.1E, 1.7E..."
## Or report interval-censored for both <LOD (or 0<value<LOD) and LOD<value<LOQ
## Don't use "Insider Censoring" - report values between LOQ and LOD as 
  ## detects with limit at LOD, report values below LOD as non-detects
  ## with limit at LOQ - produces bias and has variation at low end
## Better to use all values above LOD with limit at LOD, have more variation
  ## at low end but avoid bias


# Storing NonDetects in Databases video 3 ---------------------------------

## Indicator column method or
## interval columns

data("ShePyrene")
## Open .rda file Oahu
## load("~/classes/Censored data/Nondetects and Data Analysis/NADA Online 
  ##3_6/Class Data/Oahu.rda")
## View(Oahu)
## Convert 0=nondetect to 1=nondetect=TRUE
Oahu$ArsenicCens <- as.logical(1-Oahu$LT.0)

## Import Dataset, From Excel LOGSTC1.xlsx

##command line import of text file Example1
Ex1 <- read.table(file = file.choose(), header = TRUE)
## Convert indicator column to interval columns
Ex1$Start <- Ex1$Arsenic*(1-Ex1$NDis1)
Ex1$End <- Ex1$Arsenic


# 4b Plot Data ------------------------------------------------------------

##Code, Source file, Loadlibs
##Import Dataset, text, Zinc

cboxplot(Zinc$Zn,Zinc$ZnLT,Zinc$Zone)

cboxplot(Zinc$Zn,Zinc$ZnLT,Zinc$Zone, LOG=TRUE)

cboxplot(Zinc$Zn,Zinc$ZnLT,Zinc$Zone, LOG=TRUE, show=TRUE)

## open rda file TCEReg
cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen)

cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen, 
          xlab="Population Density", ylab="TCE (ug/L)")

cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen, 
          xlab="Population Density", ylab="TCE (ug/L)", main="XY plot")

cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen, 
          xlab="Population Density", ylab="TCE (ug/L)", main="XY plot", log="y")

cen_ecdf(Zinc$Zn,Zinc$ZnLT,Zinc$Zone)

data("ShePyrene")

cenCompareCdfs(ShePyrene$Pyrene,ShePyrene$PyreneCen)

cenCompareCdfs(ShePyrene$Pyrene,ShePyrene$PyreneCen, dist3="weibull")

cenQQ(ShePyrene$Pyrene,ShePyrene$PyreneCen)

cenCompareQQ(ShePyrene$Pyrene,ShePyrene$PyreneCen)

# AFS-style figures -------------------------------------------------------
# H. C. Glassic, K. C. Heim, C. S. Guy, Creating Figures in R that Meet the 
# AFS Style Guide: Standardization and Supporting Script. 
# Fisheries 44, 539-544 (2019).

#set working directory, you select this on your computer
# setwd(“your/directory/here”)

#generate length and weight data for Channel Catfish and Largemouth Bass
length <- seq(from = 200, to = 500, by = 10)
#a and b values from Fisheries Techniques Ws equations
a_lmb <- (-5.528)
b_lmb <- 3.273
a_cat <- (-5.800)
b_cat <- (3.294)
weight_lmb <- 10^(a_lmb + b_lmb * log10(length))
weight_cat <- 10^(a_cat + b_cat * log10(length))

#begin .tiff file of the following two panel figure, finished and saved at dev.off()
tiff(filename = "baseR_figure.tiff", width = 20.32, height = 7.62, 
     units = "cm", res = 300)
#one row, two panels
par(mfrow = c(1,2))

#make default plot
plot(length, weight_lmb)
#add points to default plot
points(length, weight_cat)
#add panel label A to default plot
mtext("A", at = min(length), adj = 0, line = 2, cex = 1.5)

#make custom figure that meets AFS style guidelines, first set plotting parameters 
#(e.g., a serif font)
par(family = "serif", mar = c(3,4,2,2), mgp = c(3,.6,0))
#make custom plot
plot(length, weight_lmb, axes = FALSE, xlab = NA, ylab = NA, ylim = c(0,2400), 
     xlim = c(100,600), pch = 19)
#add points for catfish
points(length, weight_cat, pch = 21, bg = "white")
#build x-axis
axis(1, pos = 0, at = seq(100,600, by = 100), lwd = 1.5)
#build y-axis
axis(2, pos = 100, at = seq(0,2400, by = 400), las = 1, lwd = 1.5)
#add x-axis label
mtext("Length (mm)", side = 1, line = 1.5)
#add y-axis label
mtext("Weight (g)", side = 2, line = 2)
#add panel label B to custom plot
mtext(expression(bold(B)), at = 100, adj = 0, line = 0, 
      cex = 1.5, family = "sans")
#add legend
legend(125, 2200, c("Largemouth Bass", "Channel Catfish"), 
       pch = c(19,1), bty = "n")
#finish plot and save to disk
dev.off()

#same plot with ggplot2
#load ggplot2 and other useful packages
library(tidyverse)
#load package for combining figures
library(gridExtra)

#set working directory, you select this on your computer
#setwd(“your/directory/here”)

#generate length and weight data for Channel Catfish and Largemouth Bass, 
  #and put in dataframe
length <- seq(from = 200, to = 500, by = 10)
species <- c(rep("lmb", 31), rep("cat", 31))
a_lmb <- (-5.528)
b_lmb <- 3.273
a_cat <- (-5.800)
b_cat <- (3.294)
weight_lmb <- 10^(a_lmb + b_lmb * log10(length))
weight_cat <- 10^(a_cat + b_cat * log10(length))
weight <- c(weight_lmb, weight_cat)
length_weight_data <- data.frame(species, length, weight)

#make default ggplot figure with a legend and annotated label
len_wt_default <- ggplot(data = length_weight_data, aes(x = length, 
      y = weight, fill = species)) + geom_point() + labs(title = "A")
#view the plot, will appear in R plotting window
len_wt_default

#make ggplot figure that meets AFS style guidelines
len_wt_afs <- ggplot(data = length_weight_data, aes(x = length, 
      y = weight, fill = species)) +
#set symbol shape and size
      geom_point(shape = 21, size = 2) +
#set the limits and tick breaks for the y-axis
      scale_y_continuous (limits = c(0,2400), expand = c(0,0), 
                          breaks = seq(0,2400,400)) +
#set the limits and tick spacing for the x-axis
      scale_x_continuous(limits = c(100,600), expand = c(0,0), 
                         breaks = seq(100,600,100)) +
#adjust the order of the legend, make new labels, and select the symbol colors
      scale_fill_manual(limits = c("lmb", "cat"), 
                        labels = c("Largemouth Bass", "Channel Catfish"),
                        values = c("black", "white")) +
#add B to figure
      ggtitle ("B") +
#label the y-axis
      ylab("Weight (g)") +
#label the x-axis
      xlab("Length (mm)") +
#add legend title, but left blank here because we want a legend but no title
      labs(fill = "") +
#makes the figure background white without grid lines
      theme_classic() +

#below are theme settings that provide unlimited control of your figure and 
  #can be a template for other figures
#set the size, spacing, and color for the y-axis and x-axis titles
theme (axis.title.y = element_text(size = 14, 
      margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
      axis.title.x = element_text(size = 14, 
      margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
        #set the font type
      text = element_text(family = "serif"),
        #modify plot title, the B in this case
      plot.title = element_text(face = "bold", family = "sans"),
        #position the legend on the figure
      legend.position = c(0.3,0.85),
        #adjust size of text for legend
      legend.text = element_text(size = 12),
        #margin for the plot
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        #set size of the tick marks for y-axis
      axis.ticks.y = element_line(size = 0.5),
        #set size of the tick marks for x-axis
      axis.ticks.x = element_line(size = 0.5),
        #adjust length of the tick marks
      axis.ticks.length = unit(0.2,"cm"),
        #set size and location of the tick labels for the y axis
      axis.text.y = element_text(colour = "black", size = 14, angle = 0, 
          vjust = 0.5, hjust = 1,
          margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set size and location of the tick labels for the x axis
      axis.text.x = element_text(colour = "black", size = 14, angle = 0, 
          vjust = 0, hjust = 0.5,
          margin = margin(t = 5, r = 0, b = 0, l = 0)),
          #set the axis size, color, and end shape
      axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

#view the plot, will appear in R plotting window
len_wt_afs

#arragne the two plots side by side using the gridExtra package
ggplot_figure <- grid.arrange(len_wt_default, len_wt_afs, ncol = 2)

#save the plot as a .tiff as a very large file, which is publication quality
ggsave(ggplot_figure, file = "ggplot_figure.tiff", width = 20.32, 
       height = 7.62, units = "cm", dpi = 300)

# 5b Estimating Descriptive Statistics ------------------------------------

##Code, Source file, Loadlibs
data("ShePyrene")

censummary(ShePyrene$Pyrene,ShePyrene$PyreneCen)

Pyr.mle <- cenmle(ShePyrene$Pyrene,ShePyrene$PyreneCen)
Pyr.mle

Pyr.mle.envstats <- elnormAltCensored(ShePyrene$Pyrene,ShePyrene$PyreneCen)
Pyr.mle.envstats
Pyr.mle.envstats <- elnormAltCensored(ShePyrene$Pyrene,ShePyrene$PyreneCen,
          ci=TRUE, ci.method = "bootstrap", n.bootstraps = 5000)
Pyr.mle.envstats

pyr.km <- cenfit(ShePyrene$Pyrene, ShePyrene$PyreneCen)
pyr.km

pyr.km.envstats <- enparCensored(ShePyrene$Pyrene, ShePyrene$PyreneCen)
pyr.km.envstats

pyr.km.envstats <- enparCensored(ShePyrene$Pyrene, ShePyrene$PyreneCen, 
          ci=TRUE, ci.method = "bootstrap", n.bootstraps = 5000)
pyr.km.envstats

pyr.ros <- cenros(ShePyrene$Pyrene, ShePyrene$PyreneCen)
pyr.ros
mean(pyr.ros)
sd(pyr.ros)
quantile(pyr.ros)
plot(pyr.ros)

pyr.ros.envstats <- elnormAltCensored(ShePyrene$Pyrene, ShePyrene$PyreneCen,
      method = "rROS", ci = TRUE, ci.method = "bootstrap", n.bootstraps = 5000)
pyr.ros.envstats

censtats(ShePyrene$Pyrene,ShePyrene$PyreneCen)


# 6b Interval Estimates ---------------------------------------------------
##Code, Source file, Loadlibs
data("ShePyrene")
attach(ShePyrene)
#Conf Intervals
# K-M
enparCensored(Pyrene, PyreneCen, ci=TRUE, ci.method="bootstrap", 
              n.bootstraps = 5000)
enparCensored(Pyrene,PyreneCen, ci=TRUE) #normal distribution does not 
  #reflect skewness of data
# MLE
pymle = cenmle(Pyrene, PyreneCen, conf.int=0.95)
mean(pymle)
pymlenorm=cenmle(Pyrene, PyreneCen, dist="gaussian")
mean(pymlenorm)
elnormAltCensored(Pyrene, PyreneCen, ci=TRUE, ci.method = "bootstrap", 
                  n.bootstraps = 5000)
# ROS
elnormAltCensored(Pyrene,PyreneCen, method="rROS", ci = TRUE, 
                  ci.method="bootstrap", n.bootstraps = 5000)

# Prediction Intervals
# MLE
cenPredInt (Pyrene, PyreneCen)
#ROS
cenPredInt (Pyrene, PyreneCen, newobs =2, method = "rROS")

# Tolerance Intervals
# Lognormal, Gamma and Normal with a NADAscript
cenTolInt(Pyrene, PyreneCen, cover=0.9)

# Lognormal MLE
eqlnormCensored (Pyrene, PyreneCen, p=0.9, ci=TRUE, ci.type = "upper")

# Gamma MLE
dat.gamma <- Pyrene^(1/3)
obj.gamma <- eqnormCensored (dat.gamma, PyreneCen, p=0.9, ci=TRUE, 
                             ci.type = "upper")
pct.gamma <- obj.gamma$quantiles^3
ti.gamma <- (obj.gamma$interval$limits[2])^3
pct.gamma
ti.gamma


# 7a,b,c Matched Pair Tests ---------------------------------------------------

##Code, Source file, Loadlibs
#load data from R Workspace file and R data file
load("SedHg.RData")
load("EppSedHg.rda")

cen_signtest(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, EppSedHg$Cens96)
cen_signedranktest(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, 
                   EppSedHg$Cens96)
cen_signedranktest(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, 
                   EppSedHg$Cens96, alternative = "greater")
cen_signedranktest(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, 
                   EppSedHg$Cens96, alternative = "less")
ppw.test(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, EppSedHg$Cens96)
ppw.test(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, EppSedHg$Cens96, 
         alternative = "greater")
cen_paired(EppSedHg$Hg2001, EppSedHg$Cens01, EppSedHg$Hg1996, EppSedHg$Cens96)
cen_paired(EppSedHg$Hg2001, EppSedHg$Cens01, 0.01, alternative = "greater")

#Import Dataset Example1.txt with headers

cenCompareCdfs(Example1$Arsenic,Example1$NDis1)
elnormAltCensored(Example1$Arsenic,Example1$NDis1, ci = TRUE, 
                  ci.type = "lower")

elnormAltCensored(Example1$Arsenic, Example1$NDis1, ci = TRUE, 
                  ci.type = "lower", method = "rROS", ci.method = "bootstrap", 
                  n.bootstraps = 5000)
##Error in qqPlotCensored(x = x, censored = censored, censoring.side = 
  ##censoring.side,  : 'x' must contain at least two non-missing, 
  ##distinct uncensored value.
##Because not enough data to bootstrap? n = 21 and 67% censored

load("AtraUnstacked.RData")
cenCompareCdfs(AtraUnstacked$June,AtraUnstacked$JuneCen)
elnormAltCensored(AtraUnstacked$June,AtraUnstacked$JuneCen, ci = TRUE, 
                  ci.type = "lower")
elnormAltCensored(AtraUnstacked$June,AtraUnstacked$JuneCen, ci = TRUE, 
                  ci.type = "lower", method = "rROS", ci.method = "bootstrap", 
                  n.bootstraps = 5000)
enparCensored(AtraUnstacked$June,AtraUnstacked$JuneCen, ci = TRUE, 
                  ci.type = "lower", ci.method = "bootstrap", 
                  n.bootstraps = 5000)

length(Example1$Arsenic)
cboxplot(Example1$Arsenic, Example1$NDisTRUE, Ylab = "Arsenic (ug/L)", 
         show = TRUE)
cenCompareCdfs(Example1$Arsenic, Example1$NDisTRUE, Yname = "Arsenic (ug/L)")
egammaAltCensored(Example1$Arsenic, Example1$NDisTRUE, ci = TRUE, 
                  ci.type = "upper")
egammaAltCensored(Example1$Arsenic, Example1$NDisTRUE, ci = TRUE, 
                  ci.type = "upper", ci.method = "normal.approx")
enparCensored(Example1$Arsenic, Example1$NDisTRUE, ci = TRUE, ci.type = "upper", 
              ci.method = "bootstrap", n.bootstraps = 5000)



# 7 problem set -----------------------------------------------------------
##problem set
# a. Matched Pair Tests
# Example 1. Arsenic concentrations in groundwater are to be compared to the
# drinking water standard of 10 ug/ -- the Example1.txt dataset. Has the mean
# significantly exceeded the 10 ug/L standard? Use the cen_paired script command to
# find out.

cen_paired(Example1$Arsenic, Example1$NDisTRUE, 10, alternative = "greater")
#answer is no


# Example 2. Test whether atrazine concentrations were the same in June versus
# September groundwaters in a variety of wells (rows) using AtraUnstacked.RData.
# Test both for differences in the mean as well as differences in the cdfs and the
# medians -- use all three of the paired data scripts mentioned in the lecture.

cen_signtest(AtraUnstacked$June, AtraUnstacked$JuneCen, AtraUnstacked$Sept, 
             AtraUnstacked$SeptCen)
#no difference detected


cen_signedranktest(AtraUnstacked$June, AtraUnstacked$JuneCen, AtraUnstacked$Sept, 
                   AtraUnstacked$SeptCen)
#difference detected

cen_paired(AtraUnstacked$June, AtraUnstacked$JuneCen, AtraUnstacked$Sept, 
           AtraUnstacked$SeptCen)
#no difference detected

ppw.test(AtraUnstacked$June, AtraUnstacked$JuneCen, AtraUnstacked$Sept, 
         AtraUnstacked$SeptCen)
#difference detected

# b. Comparing Data to Standards
# Example 1: Arsenic concentrations in groundwater are to be compared to the
# drinking water standard of 10 ug/ -- the Example1.txt dataset. Is the mean
# significantly below the 10 ug/L standard? Assume noncompliance until proven
# otherwise. Use both a distributional method and a nonparametric method.
length(Example1$Arsenic)
cboxplot(Example1$Arsenic, Example1$NDisTRUE, Ylab = "Arsenic (ug/L)", 
         show = TRUE)
cenCompareCdfs(Example1$Arsenic, Example1$NDisTRUE, Yname = "Arsenic (ug/L)")
egammaAltCensored(Example1$Arsenic, Example1$NDisTRUE, ci = TRUE, 
                  ci.type = "upper")
#Yes
enparCensored(Example1$Arsenic, Example1$NDisTRUE, ci = TRUE, ci.type = "upper", 
              ci.method = "bootstrap", n.bootstraps = 5000)
#Yes

# Example 2: Methyl Isobutyl Ketone (MIBK) was measured in air above a mediumsized
# US city. Data were reported only as "ND" or with a measured concentration.
# How can a UCL95 be computed for these data without a recorded detection limit?
#   (not an R exercise. Just think about it.) The dataset is Example2.txt.
#Import Dataset Example2.txt with headers
#use lowest detected value as detection limit
elnormAltCensored (Example2$MIBK, Example2$MIBKcen, method = "rROS", ci=TRUE,
                   ci.method = "bootstrap", ci.type = "upper", n.bootstraps = 5000)
#if detection limit were reported as 0.029
elnormAltCensored (Example2$MIBK2, Example2$MIBK2cen, method = "rROS", ci=TRUE,
                   ci.method = "bootstrap", ci.type = "upper", n.bootstraps = 5000)

# Example 3. Use the Example3.txt dataset. Arsenic in groundwater was measured in
# a private well used as a private supply. All 14 observations are below one of several
# reporting limits (100% NDs). What can be said about arsenic concentrations in
# respect to the drinking water standard of 10 ug/L?
#Import Dataset Example3.txt with headers
length(Example3$Arsenic)
#can't do box plot with less than 3 detects
cen_paired(Example3$Arsenic, Example3$NDisTRUE, 10, alternative = "greater")
# This gives an answer but binomial test is better for +/- data
binom.test(0, 14, alternative="less")
# All detection limits used are below the 10 ppb drinking water MCL for arsenic.
# Therefore we know that 0 out of 14 current observations exceed the MCL of 10 ppb.
# What is the range of percent of observations in the aquifer that might exceed the MCL
# (with 95% probability)? Use the binomial test command, entering the number of
# observations in the dataset that exceed the MCL (0) and the number of total observations
# (14). The ‘alternative =”less”’ option states that this is a one-sided confidence interval –
# we are looking only for possible exceedances, nothing on the low end.


# 8 Two Group Tests -------------------------------------------------------

##Code, Source file, Loadlibs
## import dataset Zinc.txt

cboxplot(Zinc$Zn,Zinc$ZnLT,Zinc$Zone)

cen2means(Zinc$Zn,Zinc$ZnLT,Zinc$Zone, LOG = FALSE)

cenperm2(Zinc$Zn,Zinc$ZnLT,Zinc$Zone)

cen2means(Zinc$Zn,Zinc$ZnLT,Zinc$Zone, LOG = TRUE)

cen1way(Zinc$Zn,Zinc$ZnLT,Zinc$Zone)

load(TCE2.RData)

cen_ecdf(Zinc$Zn,Zinc$ZnLT,Zinc$Zone)

load("TCE2.RData")

ftable(TCE2$Density~TCE2$Below5Cens)

tab= xtabs(~TCE2$Below5Cens+TCE2$Density)
chisq.test(tab)

TCE2$Below5[TCE2$Below5Cens==1] <- -1

wilcox.test(TCE2$Below5~TCE2$Density)

ftable(TCE2$Density~TCE2$Below5Cens)

tce.tab <- xtabs(~TCE2$Below5Cens+TCE2$Density)
chisq.test(tce.tab)


# 9 Three + group tests ---------------------------------------------------

##Code, Source file, Loadlibs
load("PollenThia.RData")

cboxplot(Pollen_Thia$Thiamethoxam,Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent,
         Ylab = "Thiamethoxan (ppb)", Xlab = "Sampling Event", show = TRUE, Ymax = 20,
         minmax = TRUE)

##default lognormal distribution
cenanova(Pollen_Thia$Thiamethoxam,Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent)

##bad fit and unrealistic p value and low-biased, negative means in groups near zero
cenanova(Pollen_Thia$Thiamethoxam,Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent,
         LOG = FALSE)

##better choice if you want to test differences in means on untransformed data
cenpermanova(Pollen_Thia$Thiamethoxam,Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent)


# 9a Nonparametric Peto-Peto test (cen1way) -------------------------------
##Extends Kruskal-Wallis test to censored data

##Code, Source file, Loadlibs
load("PollenThia.RData")

#multiple comparisons uses B-H false discovery rate
cen1way(Pollen_Thia$Thiamethoxam,Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent)

#plot cumulative probability
cen_ecdf(Pollen_Thia$Thiamethoxam, Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent)

cen_ecdf(log(Pollen_Thia$Thiamethoxam), Pollen_Thia$ThiaCens, Pollen_Thia$SamplingEvent,
         xlim = c(-3,5))

#contingency table test of proportion above and below highest DL

head(Pollen_Thia)
Pollen_Thia$ThiaAbvBelow <- Pollen_Thia$ThiaCens
Pollen_Thia$ThiaAbvBelow[Pollen_Thia$Thiamethoxam <0.05] <- 1
ftable(Pollen_Thia$SamplingEvent~Pollen_Thia$ThiaAbvBelow)
tab.Thia = xtabs(~Pollen_Thia$ThiaAbvBelow+Pollen_Thia$SamplingEvent)
chisq.test(tab.Thia)

Gps <- levels(Pollen_Thia$SamplingEvent)
Pct.Abv <- c(90.4, 88.9, 28.6, 14.3) #this is manually typing in the data
barplot(Pct.Abv, names.arg = Gps, ylab = "Percent above 0.05", xlab = "Sampling Event",
        main = "Percent above 0.05 distinguished by the contingency table")

#re-censor to highest detection limit and use standard nonparametric tests (K-W)
#substitute -1's to illustrate they are not concentrations

Pollen_Thia$Below05 <- Pollen_Thia$Thiamethoxam
Pollen_Thia$Below05 [Pollen_Thia$ThiaCens == 1] <- -1
Pollen_Thia$Below05 [Pollen_Thia$Thiamethoxam < 0.05] <- -1
kruskal.test(Pollen_Thia$Below05~Pollen_Thia$SamplingEvent)


