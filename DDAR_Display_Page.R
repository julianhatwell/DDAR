## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(warning = FALSE
                      , message = FALSE
                      )

knitr::opts_template$set(
  fig.wide = list(fig.height = 4.5, fig.width = 8, fig.align='center')
  , fig.wideX = list(fig.height = 3, fig.width = 9, fig.align='center')
  , fig.relaxed = list(fig.height = 6, fig.width = 8, fig.align='center')
  , fig.tile = list(fig.height = 3, fig.width = 3, fig.align='center')
)

## ----load_libs-----------------------------------------------------------
library(knitr)
library(vcd)
library(vcdExtra)
library(ca)

## ----federalist----------------------------------------------------------
data("Federalist", package = "vcd")
Federalist
sum(expand.dft(as.data.frame(Federalist))$nMay)
mean(expand.dft(as.data.frame(Federalist))$nMay)
var(expand.dft(as.data.frame(Federalist))$nMay)

## ----barplot_actual------------------------------------------------------
barplot(sqrt(Federalist)
        , main = "Number of text blocks by Number of Occurrences"
        , xlab = "Occurences of 'may'"
        , ylab = "Sqrt(Actual)"
        , col = "lightgreen"
)

## ----pois_naive_expected-------------------------------------------------
barplot(sqrt(200 * dpois(0:6, mean(expand.dft(as.data.frame(Federalist))$nMay)
))
        , main = "Number of text blocks by Number of Occurrences"
        , ylab = "sqrt(Naive Expected)"
        , xlab = "Occurences of 'may'"
        , col = "lightblue")

## ----fed_goodfit_pois----------------------------------------------------
Fed_fit0 <- goodfit(Federalist, type = "poisson")
# This will show the rate parameter, estimated by Maximum Likelihood
# This estimate is the same as the naive mean. This is to be expected when fitting a poisson.
unlist(Fed_fit0$par)

# This will show a Chi-Square Goodness of fit test against the expected values of a poisson distribution with this parameter
summary(Fed_fit0)

plot(Fed_fit0, type = "hanging", shade = TRUE)

## ----fed_goodfit_nbinom--------------------------------------------------
Fed_fit1 <- goodfit(Federalist, type = "nbinomial")
# This will show the rate and dispersion parameters, estimated by Maximum Likelihood
# prob is the rate. Here it's very close to the poisson mean
# size is the dispersion
unlist(Fed_fit1$par)
summary(Fed_fit1)

plot(Fed_fit1, type = "hanging", shade = TRUE)

## ----nonFederalist-------------------------------------------------------
nonFederalist <- structure(c(128L, 71L, 33L, 16L, 8L, 4L, 2L), .Dim = 7L, .Dimnames = structure(list(
    c("0", "1", "2", "3", "4", "5", "6")), .Names = "nMay"), class = "table")
nonFederalist
sum(expand.dft(as.data.frame(nonFederalist))$nMay)
mean(expand.dft(as.data.frame(nonFederalist))$nMay)
var(expand.dft(as.data.frame(nonFederalist))$nMay)
barplot(sqrt(nonFederalist)
        , main = "Number of text blocks by Number of Occurrences\n unseen text"
        , xlab = "Occurences of 'may'"
        , ylab = "Sqrt(Actual)"
        , col = "lightgreen"
)

## ----fit_nonfed----------------------------------------------------------
# we pass in our fitted parameter list this time
Fed_fit3 <- goodfit(nonFederalist, type = "nbinomial", par = Fed_fit1$par)
summary(Fed_fit3)
plot(Fed_fit3, type = "hanging", shade = TRUE)

## ----MSP_2D--------------------------------------------------------------
# combine results from two cities (same raters are involved in all cases)
MSP <- margin.table(MSPatients, 1:2)
MSP

## ----MSP_accuracy--------------------------------------------------------
# basic accuracy
sum(diag(MSP))/sum(MSP)

## ----MSP_winntrue--------------------------------------------------------
margin.table(MSP, 2)

## ----MSP_kappa-----------------------------------------------------------
# cohen's kappa - the function in the vcd package provides weights and confidence intervals for this statistic
confint(Kappa(MSP, weights = "Fleiss-Cohen"))

## ----MSP_agreement-------------------------------------------------------
op <- par(mar = c(4, 3, 4, 1) + .1)
B <- agreementplot(MSP
                   , main = "MS Patient Ratings"
                   , xlab_rot = -20
)
par(op)

## ----MSP_bangdi----------------------------------------------------------
unlist(B)[1 : 2]

## ----describe_haireye, echo=FALSE----------------------------------------
hr_desc <- data.frame(No=1:3
                      , Name=c("Hair", "Eye", "Sex")
                      , Levels=c("Black, Brown, Red, Blond", "Brown, Blue, Hazel, Green", "Male, Female"))
kable(hr_desc)

## ----haireye-------------------------------------------------------------
haireye <- margin.table(HairEyeColor, 1:2)
haireye <- as.table(haireye[, c("Brown", "Hazel", "Green", "Blue")])
haireye

## ----hr_chisq------------------------------------------------------------
chisq.test(haireye)

## ----hr_expected---------------------------------------------------------
expected = independence_table(haireye)
round(expected, 1)

mosaic(expected
      , shade = TRUE
      , main="Expected frequencies"
      , labeling = labeling_values
      , value_type = "expected"
      , gp_text = gpar(fontface = 1))

## ----hr_actuals----------------------------------------------------------
mosaic(haireye
      , gp = shading_Friendly # shade = TRUE
      , main="Actual frequencies"
      , labeling = labeling_values
      , value_type = "observed"
      , gp_text = gpar(fontface = 1)
      , rot_labels = c(top = -20))

## ----hec-----------------------------------------------------------------
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"), ]
mosaic(HEC
      , gp = shading_Friendly # shade = TRUE
      , main="Actual frequencies"
      , labeling = labeling_values
      , value_type = "observed"
      , gp_text = gpar(fontface = 1), rot_labels = c(right = -45)) 

## ----describe_tv, echo=FALSE---------------------------------------------
tv_desc <- data.frame(No=1:3
                      , Name=c("Day", "Time", "Network")
                      , Levels=c("Monday, Tuesday, Wednesday, Thursday, Friday", "8, 9, 10", "ABC, CBS, NBC"))
kable(tv_desc)

## ----tv_dataprep---------------------------------------------------------
data("TV", package = "vcdExtra")
# The original data is collected in 15 minutes slices.

# Convert 3-D array to a frequency data frame
# This has a row for each cell of the array
# and a new column for the cell value
TV.df <- as.data.frame.table(TV)

# Convert it into hourly slices
levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4,4,3))

# Convert frequency data back to 3-D array, now with just 3 time level
TV3 <- xtabs(Freq~Day+Time+Network, TV.df)

## ----tv_3wayca-----------------------------------------------------------
# create a multiple correspondence analysis
TV3.mca <- mjca(TV3)

# the plot function uses all base R plot stuff
# but needs a bit of manipulation
cols <- c("blue", "black", "red")

# "blank plot"
res <- plot(TV3.mca, labels=0, pch='.', cex.lab=1.2)

# combine Dims, factor names and levels
coords <- data.frame(res$cols, TV3.mca$factors)

# hard-coded from known number of levels
# day, time, network
nlev <- c(5,3,3)

# everything needs to be in semantic order
coords <- coords[ order(coords[,"factor"], coords[,"level"]), ]
# quick fix for ordering
coords$order <- c(5, 1, 4, 2, 3, 6, 7, 8, 11, 9, 10)
coords <- coords[order(coords[, "order"]), ]

# place the points
points(coords[,1:2], pch=rep(16:18, nlev), col=rep(cols, nlev), cex=1.2)

# place the text
pos <- c(1,4,3)
text(coords[,1:2], labels=coords$level, col=rep(cols, nlev), pos=rep(pos,nlev), cex=1.1, xpd=TRUE)

# join things in sequence
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Day", lty=1, lwd=1, col="blue")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Time",  lty=1, lwd=1, col="red")

# add segement from the origin to channels
nw <- subset(coords, factor=="Network")
segments(0, 0, nw[,"Dim1"], nw[, "Dim2"], col = "black", lwd = 0.5, lty = 3)

# add a legend
legend("topright", legend=c("Day", "Time", "Network"),
       title="Factor", title.col="black",
       col=cols, text.col=cols, pch=16:18,
       bg="gray95")

## ----tv_mosaic-----------------------------------------------------------
# pivoting the dimensions for a clearer view
mosaic(xtabs(Freq~Network+Day+Time, TV.df)
       , gp = shading_Friendly # shade = TRUE
       #, rot_labels = c(top = -20)
       )

