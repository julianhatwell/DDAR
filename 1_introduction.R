## ----not_DDA-------------------------------------------------------------
# set the random number generator same every time
set.seed(123)

# a discrete value to match the groups
n <- 100
grp <- rep(c(1, 2), each = n)

# a random discrete value
rnd <- sample(c(1, 2)
              , replace = TRUE
              , size = 200)
numvars1 <- rmvnorm(n, c(0, 0)
                    , sigma = matrix(c(3,2,2,3)
                                     , ncol=2))

numvars2 <- rmvnorm(n, c(1, 1)
                    , sigma = matrix(c(1,0.5,0.5,4)
                                     , ncol=2))
numvars <- rbind(numvars1, numvars2)

# a discrete value based on a soft linear boundary
noise <- rnorm(2*n, mean = 0, sd = 0.5)
fam <- ifelse(rowSums(numvars) + noise > 3, 1, 2)

# plot, using graphic options to separate the discrete classes
plot(numvars, col = fam, pch = rnd, cex = grp
     , main = "scatterplot with size, shape and colour options"
     , sub = "typical options for stratifying numeric data by discrete data"
     , xlab = "x"
     , ylab = "y")

# take row sums then table-apply the sum by each group
grp_total <- tapply(rowSums(numvars), grp, sum)
rnd_total <- tapply(rowSums(numvars), rnd, sum)
fam_total <- tapply(rowSums(numvars), fam, sum)

# totals barplot
barplot(c(grp_total, rnd_total, fam_total)
        , col="lightgray", border = 1:2
        , main="barplot with border options"
        , sub = "numeric data stratified on groups")

# classic box plot
# note that you can't tell
# that grp is the actual groups
# and rnd is a random labelling
boxplot(rowSums(numvars)~grp*rnd, border=1:2
        , main = "boxplot distribution analysis by groups"
        , sub = "typical options for stratifying numeric data by discrete data"
        , xlab = "grp1 grp2 rnd1 rnd2"
        , ylab = "row sum value")


## ----count_data----------------------------------------------------------
# length will count the members
grp_count <- tapply(rowSums(numvars), grp, length)
rnd_count <- tapply(rowSums(numvars), rnd, length)
fam_count <- tapply(rowSums(numvars), fam, length)

# counts barplot
barplot(c(grp_count, rnd_count, fam_count)
        , col="lightgray", border = 1:2
        , main="barplot with border options"
        , sub="counts by groups")
