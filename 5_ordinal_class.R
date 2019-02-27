## ----MSP_2D--------------------------------------------------------------
# combine results from two cities (same raters are involved in all cases)
MSP <- margin.table(MSPatients, 1:2)
class_names <- c("Certain",  "Probable", "Possible", "Doubtful")
dimnames(MSP) <- list(prd = class_names
                      , tru = class_names)

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
