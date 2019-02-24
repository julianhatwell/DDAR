## ----not_DDA-------------------------------------------------------------
# you need the mvtnorm library loaded to run this code
# set the random number generator same every time
set.seed(123)

# a discrete feature to label the groups
n <- 100
grp <- rep(c(1, 2), each = n)

# a random discrete feature
rnd <- sample(c(1, 2)
              , replace = TRUE
              , size = 200)

# some random multivariate normal data for grp1
numvars1 <- rmvnorm(n, c(0, 0)
                    , sigma = matrix(c(3,2,2,3)
                                     , ncol=2))

# some random multivariate normal data
# with different parameters for grp2
numvars2 <- rmvnorm(n, c(1, 1)
                    , sigma = matrix(c(1,0.5,0.5,4)
                                     , ncol=2))

# combine together as one data frame
numvars <- rbind(numvars1, numvars2)

# add a discrete feature based on a linear combination
# + noise to make a soft boundary
noise <- rnorm(2*n, mean = 0, sd = 0.5)
fam <- ifelse(rowSums(numvars) + noise > 3, 1, 2)

# take row sums then table-apply the sum by each group
grp_total <- tapply(rowSums(numvars), grp, sum)
rnd_total <- tapply(rowSums(numvars), rnd, sum)
fam_total <- tapply(rowSums(numvars), fam, sum)

par(mfrow=c(1,3))
# plot, using graphic options to separate the discrete classes
plot(numvars, col = fam, pch = rnd, cex = grp
     , main = "scatterplot with size, shape\nand colour options"
     , ylab = "y"
     , xlab = "x")

# take row sums then table-apply the sum by each group
grp_total <- tapply(rowSums(numvars), grp, sum)
rnd_total <- tapply(rowSums(numvars), rnd, sum)
fam_total <- tapply(rowSums(numvars), fam, sum)

# totals barplot
barplot(c(grp_total, rnd_total, fam_total)
        , xlab="grp rnd fam"
        , col="lightgray", border = 1:2
        , main="barplot of quantitative\ntotals by groups"
        )

# classic box plot
boxplot(rowSums(numvars)~rnd*grp, border=1:2
     , main = "boxplot distribution\nanalysis by groups"
     , xlab = "rnd1.grp1 rnd2.grp1 rnd1.grp2 rnd2.grp2"
     , ylab = "row sum value")

par(mfrow=c(1,1))

## ----count_data----------------------------------------------------------
# length will count the members, table apply by category
grp_count <- tapply(rowSums(numvars), grp, length)
rnd_count <- tapply(rowSums(numvars), rnd, length)
fam_count <- tapply(rowSums(numvars), fam, length)

# counts barplot
barplot(c(grp_count, rnd_count, fam_count)
        , col="lightgray", border = 1:2
        , xlab="grp rnd fam"
        , main="counts by groups")

## ----describe_haireye, echo=FALSE----------------------------------------
hr_desc <- data.frame(No=1:3
                      , Name=c("Hair", "Eye", "Sex")
                      , Levels=c("Black, Brown, Red, Blond", "Brown, Blue, Hazel, Green", "Male, Female"))
kable(hr_desc)

## ----haireye_prep--------------------------------------------------------
# ignore gender for now
haireye <- margin.table(HairEyeColor, 1:2)

# re-arrange order of eye colour from dark to light
# it's already correct for hair
haireye <- as.table(haireye[, c("Brown", "Hazel", "Green", "Blue")])
haireye

## ----not_intuitive-------------------------------------------------------
# With a grouped bar plot
# you're forced to favour one variable over the other.

# Does does hair colour depend on eye colour?
barplot(haireye, beside = TRUE, legend = TRUE)

# Or does eye colour depend on hair colour?
# pivot the other way
barplot(aperm(haireye, 2:1), beside = TRUE, legend = TRUE)

## ----tables--------------------------------------------------------------
# 3D table - Admit on rows (D1), Gender on columns (D2)
# and Department on levels (D3)

UCBAdmissions # Department has 6 levels

# In 2D - collapse over department
margin.table(UCBAdmissions, 2:1)

# pivot the department onto columns
aperm(UCBAdmissions, c(2, 3, 1))

# flatten to a crosstab in default order
structable(UCBAdmissions)

# flatten to a crosstab with pivot
structable(aperm(UCBAdmissions, c(2, 3, 1)))

# More D
structable(Survived~., data=Titanic)

# More D - another pivot
structable(Survived+Sex~., data=Titanic)

## ----crosstabs-----------------------------------------------------------
# An old fashioned crosstab - 2D
CrossTable(margin.table(UCBAdmissions, 2:1)
           , prop.chisq = FALSE
           , format = "SPSS")

## ----hr_expected---------------------------------------------------------
# visualising expected counts for independent features
expected = independence_table(haireye)
round(expected, 1)

mosaic(expected
      , main="Expected frequencies"
      , labeling = labeling_values
      , value_type = "expected"
      , gp_text = gpar(fontface = 1))

## ----hr_chisq------------------------------------------------------------
chisq.test(haireye)

##-----test_ca-----
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

