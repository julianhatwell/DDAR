# Kaplan Theme

# brand
k.purple <- "#1F0477"
k.darkblue <- "#00539F"
k.brightblue <- "#0098CD"
k.darkred <- "#D6083B"
k.orange <- "#E55302"
k.deepgreen <- "#3D9B35"
k.lime <- "#BED600"
k.pink <- "#E62899"
k.brightorange <- "#FFC82E"

# neutral
k.warmgrey9 <- "#837870"
k.warmgrey2 <- "#D7D2CB"
k.coolgrey9 <- "#747679"
k.coolgrey4 <- "#BDBDBC"
k.wheat <- "#E8ED70"
k.stone <- "#DCCEAB"

myPalBrand <- c(k.purple, k.darkblue, k.brightblue
           , k.darkred, k.orange, k.deepgreen
           , k.lime, k.pink, k.brightorange)
myPalBrand2 <- c(k.purple, k.orange, k.deepgreen
                 , k.darkblue, k.darkred, k.lime
                 , k.brightblue, k.pink, k.brightorange)

myPalNeut <- c(k.warmgrey9, k.warmgrey2, k.coolgrey9, k.coolgrey4, k.wheat, k.stone)
myPalNeut2 <- c(k.coolgrey4, k.coolgrey9, k.warmgrey9, k.warmgrey2, k.stone, k.wheat)

myPalBrandContrasts <- c(k.purple, k.brightorange, k.lime
                    , k.darkblue, k.pink, k.deepgreen
                    , k.orange, k.brightblue, k.darkred, myPalNeut)

k.grad.purple <- colorRampPalette(c("#1D165D", "#7C5DC6"))
k.grad.purple.rev <- colorRampPalette(rev(c("#1D165D", "#7C5DC6")))

k.grad.red <- colorRampPalette(c("#9E1B32", "#E70033"))
k.grad.red.rev <- colorRampPalette(rev(c("#9E1B32", "#E70033")))

k.grad.grey <- colorRampPalette(c("#4D4E53", "#9A9B9D"))
k.grad.grey.rev <- colorRampPalette(rev(c("#4D4E53", "#9A9B9D")))

k.grad.blue <- colorRampPalette(c("#0071B2", "#00B8E4"))
k.grad.blue.rev <- colorRampPalette(rev(c("#0071B2", "#00B8E4")))

k.grad.orange <- colorRampPalette(c("#C05017", "#FF9231"))
k.grad.orange.rev <- colorRampPalette(rev(c("#C05017", "#FF9231")))

k.grad.green <- colorRampPalette(c("#5A8E22", "#A3D869"))
k.grad.green.rev <- colorRampPalette(rev(c("#5A8E22", "#A3D869")))

# need to look up the pink gradients
# k.grad.pink <- colorRampPalette(c("#5A8E22", "#A3D869"))
# k.grad.pink.rev <- colorRampPalette(rev(c("#5A8E22", "#A3D869")))

# applied to lattice
MyLatticeFont <- list(font = 1, cex = 1)
MyLabelFont <- list(font = 1, cex = 0.9)
MyStripFont <- list(font = 1, cex = 0.8)
MyLatticeStrip = strip.custom(par.strip.text = MyStripFont)
MyLatticeTheme <- list(
  par.main.text = MyLatticeFont
  , par.xlab.text = MyLabelFont
  , par.ylab.text = MyLabelFont
  , add.text = MyLabelFont
  , axis.text = MyStripFont
  , fontsize = list(text = 11, points = 7)
  , plot.symbol = list(col = k.purple, pch = 19, alpha = 1, cex = 1)
  , plot.polygon = list(col = k.purple, border = k.pink)
  , dot.symbol = list(col = k.purple, cex = 1)
  , superpose.symbol = list(pch = c(20, 18, 16, 10, 12, 3, 4, 6, 2)
                            , col = myPalBrandContrasts
                            , cex = 1.2)
  , superpose.polygon = list(col = myPalBrand2)
  , box.umbrella = list(col = k.purple, lty = 2, lwd = 1.25)
  , box.rectangle = list(col = k.purple, lwd = 1.5)
  , box.dot = list(col = k.purple, pch = 16, cex = 1.5, alpha = 1)
  , plot.line = list(col = myPalBrand)
  , add.line = list(col = myPalBrand2, lty = 2)
  , strip.background = list(col = myPalNeut[c(2,4,6)])
  , strip.shingle = list(col = myPalNeut[c(1,3,5)])
)
# seems to be an issue setting plot.polygon in the theme
# do it the sledgehammer way here.
#trellis.par.set(
#                , superpose.symbol = list(col = myPalBrandContrasts, pch = 19))

# fourfold colour scheme
fourfold_k <- c(k.darkred, k.purple, k.pink, k.brightblue, k.darkred, k.purple)

# strucplot grapcons
# TO DO implement interpolation
shading_k <- function(observed = NULL, residuals = NULL, expected = NULL,
                      df = NULL, col = c(k.purple, k.pink)) {
  if (length(col) != 2) { stop("Need exactly two colors!") }
  function(res) gpar(fill = ifelse(res > 0, col[1], col[2]))
}
class(shading_k) <- "grapcon_generator"

# applied to ggplot2
myGgTheme <- theme(plot.title = element_text(colour = myPalBrand[1], size = 14)
                   , axis.title = element_text(colour = myPalBrand[1], size = 12)
                   , axis.text = element_text(colour = myPalBrand[1], size = 10)
                   , axis.line = element_line(colour = myPalBrand[1], size = 0.5)
                   , axis.ticks = element_line(colour = myPalBrand[1], size = 0.5)
                   , panel.border = element_rect(colour = "transparent")
                   , strip.background = element_rect(colour = "transparent"
                                                     , fill = myPalBrand[9])
                   , legend.title = element_text(colour = myPalBrand[1])
                   , legend.text = element_text(colour = myPalBrand[1]))

myGgFillScale2 <- scale_fill_manual(values = c(myPalBrand[8], myPalBrand[2]))
myGgFillScale4 <- scale_fill_manual(values = k.grad.purple(4))
myGgColourScale <- scale_colour_manual(values = c(myPalBrand[8], myPalBrand[2]))
myGgPurpleGradient <- scale_color_gradient(low = k.grad.purple(100)[1], high = k.grad.purple(100)[100])
myGgRedGradient <- scale_color_gradient(low = k.grad.red(100)[1], high = k.grad.red(100)[100])
myGgGreyGradient <- scale_color_gradient(low = k.grad.grey(100)[1], high = k.grad.grey(100)[100])
myGgBlueGradient <- scale_color_gradient(low = k.grad.blue(100)[100], high = k.grad.blue(100)[1])
myGgGreenGradient <- scale_color_gradient(low = k.grad.green(100)[1], high = k.grad.green(100)[100])
myGgOrangeGradient <- scale_color_gradient(low = k.grad.orange(100)[1], high = k.grad.orange(100)[100])

myGgHeatGradient <- scale_color_gradient2(low = myPalBrand[9]
, mid = myPalBrand[5]
, high = myPalBrand[4]
, midpoint = 3000)
