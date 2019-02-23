## ----nursery_data--------------------------------------------------------
set.seed(54321)
nursery <- read.csv(gzfile("nursery.csv.gz"))
summary(nursery)

## ----nurs_randfor--------------------------------------------------------
# train test split
idx <- sample(c(TRUE, FALSE)
              , size = nrow(nursery)
              , prob = c(0.7, 0.3)
              , replace = TRUE)

nurs_train <- nursery[idx, ]
nurs_test <- nursery[!idx, ]

# train rf accepting all the defaults
nurs_rf <- randomForest(decision~., data=nurs_train)

# get the predictions on new data
nurs_preds <- predict(nurs_rf, newdata = nurs_test)

# confusion matrix
confmat <- with(nurs_test, table(nurs_preds, decision))
confmat

## ----strucplot_confmat---------------------------------------------------
# strucplot tile - I like this one best
tile(confmat, labeling = labeling_values)

## ----nurs_varimp---------------------------------------------------------
varImpPlot(nurs_rf)

## ----nurs_firstvar-------------------------------------------------------
# unfortunately, there is no cotabplot option for tile
# this has to be done manually to make any sense

# from the summary, health can take one of three values
# not_recom, priority, recommended

cm_3d <- with(nurs_test
              , table(nurs_preds, decision, health))

re <- cm_3d[,,"recommended"]
pr <- cm_3d[,,"priority"]
nr <- cm_3d[,,"not_recom"]

tile(re, labeling = labeling_values)
tile(pr, labeling = labeling_values)
tile(nr, labeling = labeling_values)

## ----german_randfor------------------------------------------------------
german <- read.csv(gzfile("german.csv.gz"))
summary(german)

set.seed(54321)
# train test split
idx <- sample(c(TRUE, FALSE)
              , size = nrow(german)
              , prob = c(0.7, 0.3)
              , replace = TRUE)

german_train <- german[idx, ]
german_test <- german[!idx, ]

# make the training data balanced in case over fits good
# originally 300 bad, 700 good
german_train <- ovun.sample(rating~.
                            , data=german_train
                            , method = "over"
                            , p = 0.5)$data

# restore the factor levels to correct order
german_train$rating <- relevel(german_train$rating, "bad") 

# train rf accepting all the defaults
german_rf <- randomForest(rating~., data=german_train)

# get the predictions on new data
german_preds <- predict(german_rf, newdata = german_test)

# confusion matrix
confmat <- with(german_test, table(german_preds, rating))
confmat

## ----main_cm_stats-------------------------------------------------------
# accuracy
sum(diag(confmat))/sum(confmat)

# False Negative Rate FN / (TP + FN)
confmat[2, 1]/sum(confmat[, 1])

# False Ommision Rate FN / (TN + FN)
confmat[2, 1]/sum(confmat[2, ])

## ----german_fourf--------------------------------------------------------
# fourfold plot preserves the table structure it was given
# however, default balances the areas to compare odds ratios no matter the individual count values
# that's not what we want
fourfold(confmat)

# std = "all.max" suppresses balancing the areas
# behaves like a rose plot
fourfold(confmat, std = "all.max")

## ----german_varimp-------------------------------------------------------
varImpPlot(german_rf)

## ----my_rose-------------------------------------------------------------
# customise the fourfold plot
my_rose <- function(cm) {
  fourfold(cm
           , std = "all.max" # mimic a 4 way rose plot
           , conf_level = 0 # suppress confint tests, not useful here
           , color = c("#FF0000", "#000080") # need to force colours due to suppressing confint test
  )
}

# while we're at it, let's make a function to get the numeric statistics that we want from the conf mat
my_confmat_stats <- function(cm) {
  cf_stats <- list()
  for (i in 1:(dim(cm)[3])) {
    
    category <- dimnames(cm)[[3]][i]
    acc <- sum(diag(cm[ , , i]))/sum(cm[ , , i])
    
    # False Negative Rate FN / (TP + FN)
    fnr <- cm[2, 1, i]/sum(cm[ , 1, i])
    
    # False Ommision Rate FN / (TN + FN)
    fomr <- cm[2, 1, i]/sum(cm[2, , i])
    
    # collect
    cf_stats[[category]] <-
      c(acc=acc, fnr=fnr, fomr=fomr)
  }
  
  return(t(as.data.frame(cf_stats)))
}

## ----rose_chk------------------------------------------------------------
# stratify the conf mat by chk
confmat_c <- with(german_test
                  , table(german_preds
                          , rating
                          , chk))

# view the stats then plot
my_confmat_stats(confmat_c)
my_rose(confmat_c)

## ----rose_others---------------------------------------------------------
confmat_p <- with(german_test
                  , table(german_preds
                          , rating
                          , pps))

my_confmat_stats(confmat_p)
my_rose(confmat_p)

confmat_h <- with(german_test
                  , table(german_preds
                          , rating
                          , crhis))

my_rose(confmat_h)

confmat_e <- with(german_test
                  , table(german_preds
                          , rating
                          , emp))


## ----my_rose_dur---------------------------------------------------------
confmat_d <- with(german_test
                  , table(german_preds
                          , rating
                          , dur))
my_rose(confmat_d)


## ----lodds_ratio_plots---------------------------------------------------
plot(loddsratio(confmat_c), confidence = FALSE)
plot(loddsratio(confmat_e), confidence = FALSE)

# higher dimensional
confmat_ce <- with(german_test
                   , table(german_preds
                           , rating
                           , chk
                           , emp))

confmat_ce <- with(german_test
                   , table(german_preds
                           , rating
                           , chk
                           , emp))
plot(loddsratio(confmat_ce), confidence = FALSE)

# back to one stratum, but many levels
plot(loddsratio(confmat_d), confidence = FALSE)

