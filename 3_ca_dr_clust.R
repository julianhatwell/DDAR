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

# save this for later
m_coords <- coords[ order(coords[,"factor"], -coords[,"Dim1"]), c("Dim1", "Dim2")]

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
# Flatten to 2-D by stacking Time onto Day
# Note: The data shaping choice here controls the specifics of the analysis.
TV3s <- as.matrix(structable(Network~Time+Day, TV3))

# Create the Correspondence Analysis objects
TV3s.ca <- ca(TV3s)

# Generate the plot
res.ca <- plot(TV3s.ca)
# add some segments from the origin to make things clearer
segments(0, 0, res.ca$cols[,1], res.ca$cols[,2], col = "red", lwd = 1)
segments(0, 0, res.ca$rows[,1], res.ca$rows[,2], col = "blue", lwd = 0.5, lty = 3)

## ----TV_monday-----------------------------------------------------------
t(TV3[1,,]) # Monday
TV3[,2,1] # Every day, 9pm, ABC

## ----tv_ca_dims----------------------------------------------------------
m_coords
res.ca$rows[order(-res.ca$rows[, 1]), ]

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
