## ----MSP_2D--------------------------------------------------------------
# combine results from two cities (same raters are involved in all cases)
MSP <- margin.table(MSPatients, 1:2)

## ----MSP_winntrue--------------------------------------------------------
margin.table(MSP, 2)
props <- prop.table(margin.table(MSP, 2))
props

# The baseline accuracy is
props[which(props == max(props))]

## ----MSP_accuracy--------------------------------------------------------
# basic accuracy
sum(diag(MSP))/sum(MSP)

## ----MSP_confmat---------------------------------------------------------
MSP

## ----MSP_kappa-----------------------------------------------------------
# cohen's kappa - the function in the vcd package provides weights and confidence intervals for this statistic
confint(Kappa(MSP, weights = "Fleiss-Cohen"))

## ----MSP_agreement, results='hold'---------------------------------------
op <- par(mar = c(4, 3, 4, 1) + .1)
B <- agreementplot(MSP
                   , main = "MS Patient Ratings"
                   , xlab_rot = -20
)

## ---- echo=FALSE---------------------------------------------------------
# ignore this
par(op)

## ----MSP_bangdi----------------------------------------------------------
unlist(B)[1 : 2]

## ----dpois---------------------------------------------------------------
set.seed(12345)

KL <- expand.grid(k = 0 : 20, lambda = c(1, 5, 10, 20))
pois_df <- data.frame(KL, prob = dpois(KL$k, KL$lambda))
pois_df$lambda = factor(pois_df$lambda)

xyplot(prob ~ k | lambda, data = pois_df,
       type = c("h", "p"), pch = 16, lwd = 2
       , cex = 1.25, layout = c(4, 1)
       , main = expression(paste("Poisson Distribution by ", lambda))
       , xlab = list("Number of events (k)", cex = 1.25)
       , ylab = list("Probability", cex = 1.25))

## ----dnbin---------------------------------------------------------------
XN <- expand.grid(k = 0 : 20, n = c(1, 5, 10, 20), p = c(0.4, 0.2))
nbin_df <- data.frame(XN, prob = dnbinom(XN$k, XN$n, XN$p))
nbin_df$n <- factor(nbin_df$n)
nbin_df$p <- factor(nbin_df$p)

xyplot(prob ~ k | n + p, data = subset(nbin_df, p == 0.4)
       , main = "Neg. binom by size and prob"
       , xlab = list("Number of failures (k)", cex = 1.25)
       , ylab = list("Probability",  cex = 1.25)
       , type = c("h", "p"), pch = 16, lwd = 2
       , strip = strip.custom(strip.names = TRUE)
)
xyplot(prob ~ k | n + p, data = subset(nbin_df, p == 0.2)
       , main = "Neg. binom by size and prob"
       , xlab = list("Number of failures (k)", cex = 1.25)
       , ylab = list("Probability",  cex = 1.25)
       , type = c("h", "p"), pch = 16, lwd = 2
       , strip = strip.custom(strip.names = TRUE)
)


## ----federalist----------------------------------------------------------
data("Federalist", package = "vcd")
Federalist
sum(Federalist) # number of blocks
sum(expand.dft(as.data.frame(Federalist))$nMay) # number of occurences
mean(expand.dft(as.data.frame(Federalist))$nMay)
var(expand.dft(as.data.frame(Federalist))$nMay)

## ----pois_naive_expected-------------------------------------------------
barplot(sqrt(200 * dpois(0:6, mean(expand.dft(as.data.frame(Federalist))$nMay)
))
        , main = "Number of text blocks by Number of Occurrences\nnaive expected values"
        , ylab = "sqrt(Naive Expected)"
        , xlab = "Occurences of 'may'"
        , col = "lightblue")

## ----barplot_actual------------------------------------------------------
barplot(sqrt(Federalist)
        , main = "Number of text blocks by Number of Occurrences\nactual values"
        , xlab = "Occurences of 'may'"
        , ylab = "Sqrt(Actual)"
        , col = "lightgreen"
)

## ----fed_goodfit_pois_chisq----------------------------------------------
Fed_fit0 <- goodfit(Federalist, type = "poisson")
# This will show the rate parameter, estimated by Maximum Likelihood
# This estimate is the same as the naive mean. This is to be expected when fitting a poisson.
unlist(Fed_fit0$par)

# This will show a Chi-Square Goodness of fit test against the expected values of a poisson distribution with this parameter
summary(Fed_fit0)

## ----fed_goodfit_pois_plot-----------------------------------------------
plot(Fed_fit0, type = "hanging", shade = TRUE)

## ----fed_goodfit_pois_mchisq---------------------------------------------
Fed_fit0 <- goodfit(Federalist, type = "poisson", method = "MinChisq")
# This has found a different value for the rate parameter.
unlist(Fed_fit0$par)

# This will show a Chi-Square Goodness of fit test against the expected values of a poisson distribution with this parameter
summary(Fed_fit0)

## ----fed_goodfit_pois_mplot----------------------------------------------
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
# Some dummy data
nonFederalist <- structure(c(135L, 71L, 36L, 17L, 8L, 4L, 1L), .Dim = 7L, .Dimnames = structure(list(
    c("0", "1", "2", "3", "4", "5", "6")), .Names = "nMay"), class = "table")
nonFederalist
sum(nonFederalist) # number of blocks
sum(expand.dft(as.data.frame(nonFederalist))$nMay) # number of occurences
mean(expand.dft(as.data.frame(nonFederalist))$nMay)
var(expand.dft(as.data.frame(nonFederalist))$nMay)
barplot(sqrt(nonFederalist)
        , main = "Number of text blocks by Number of Occurrences\nunseen text"
        , xlab = "Occurences of 'may'"
        , ylab = "Sqrt(Actual)"
        , col = "lightgreen"
)

## ----fit_nonfed_chisq----------------------------------------------------
# we pass in our fitted parameter list this time
Fed_fit3 <- goodfit(nonFederalist, type = "nbinomial", par = Fed_fit1$par)
summary(Fed_fit3)

## ----fit_nonfed_plot-----------------------------------------------------
plot(Fed_fit3, type = "hanging", shade = TRUE)

