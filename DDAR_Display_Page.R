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

# Convert frequency data back to 3-D array, now with just 3 time levels
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
# quick fix for ordering e.g. day of week, not alphabetical
coords$order <- c(5, 1, 4, 2, 3, 6, 7, 8, 11, 9, 10)
coords <- coords[order(coords[, "order"]), ]

# place the points with separate plot chars and colours
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
legend("topright", legend=c("Day", "Network", "Time"),
       title="Factor", title.col="black",
       col=cols, text.col=cols, pch=16:18,
       bg="gray95")

## ----TV_day_network------------------------------------------------------
margin.table(TV3, 1)
margin.table(TV3, c(1, 3))

## ----TV_time_network-----------------------------------------------------
margin.table(TV3, 2)
t(TV3[4,,]) # Thursday

## ----tv_2wayca-----------------------------------------------------------
# Flatten to 2-D by stacking Time onto Network
# Note: The data shaping choice here controls the specifics of the analysis.
TV3s <- as.matrix(structable(Network~Time+Day, TV3))

# Create the Correspondence Analysis objects
TV3s.ca <- ca(TV3s)

# Generate the plot
res <- plot(TV3s.ca)
# add some segments from the origin to make things clearer
segments(0, 0, res$cols[,1], res$cols[,2], col = "red", lwd = 1)
segments(0, 0, res$rows[,1], res$rows[,2], col = "blue", lwd = 0.5, lty = 3)

## ----TV_monday-----------------------------------------------------------
t(TV3[1,,]) # Monday
TV3[,2,1] # Every day, 9pm, ABC

## ----Titanic_mca---------------------------------------------------------
# one more mca from a 4-D dataset
# this is simpler than it looks
# all the magic happens here
titanic.mca <- mjca(Titanic)

# saving the plot object supplies the coordinate positions
res <- plot(titanic.mca, labels=0, pch='.', cex.lab=1.2)

# extract factor names and levels
coords <- data.frame(res$cols, titanic.mca$factors)

# everything else is handling base R plotting stuff
cols <- c("blue", "red", "brown", "black")
nlev <- c(4,2,2,2)
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
pos <- c(3,1,1,3)
text(coords[,1:2], labels=coords$level, col=rep(cols, nlev), pos=rep(pos,nlev), cex=1.1, xpd=TRUE)
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Class", lty=1, lwd=2, col="blue")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Sex",  lty=1, lwd=2, col="red")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Age",  lty=1, lwd=2, col="brown")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Survived",  lty=1, lwd=2, col="black")

legend("topleft", legend=c("Class", "Sex", "Age", "Survived"),
       title="Factor", title.col="black",
       col=cols, text.col=cols, pch=16:19,
       bg="gray95", cex=1.2)

## ----Titanic.ca----------------------------------------------------------
# Simple ca works on two dimensions
# pivot table of Titanic
Titanic_piv <- structable(~Class+Sex+Survived+Age, Titanic)
Titanic_piv
# all the work happens here
Titanic.ca <- ca(as.matrix(Titanic_piv))

# plot and save the object to get the coords for line segments
res <- plot(Titanic.ca)
segments(0, 0, res$cols[,1], res$cols[,2], col = "red", lwd = 1)
segments(0, 0, res$rows[,1], res$rows[,2], col = "blue", lwd = 0.5, lty = 3)

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

