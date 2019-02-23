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
# You're forced to favour one variable over the other?

# Or does hair colour depend on eye colour
barplot(haireye, beside = TRUE, legend = TRUE)

# Does eye colour depend on hair colour?
# pivot the other way
barplot(aperm(haireye, 2:1), beside = TRUE, legend = TRUE)

## ----tiles---------------------------------------------------------------
tile(haireye) # vcd package

## ----hr_chisq------------------------------------------------------------
chisq.test(haireye)

## ----hr_expected---------------------------------------------------------
# visualising expected counts for independent features
expected = independence_table(haireye)
round(expected, 1)

mosaic(expected
       , shade = TRUE
       , main="Expected frequencies"
       , labeling = labeling_values
       , value_type = "expected"
       , gp_text = gpar(fontface = 1))

## ----hr_actuals----------------------------------------------------------
# visualising actual counts for independent features
mosaic(haireye
       , main="Actual frequencies"
       , labeling = labeling_values
       , value_type = "observed"
       , gp_text = gpar(fontface = 1)
       , rot_labels = c(top = -20))

# A publication ready colour scheme
mosaic(haireye
       , gp = shading_hcl # shade = TRUE
       , main="Actual frequencies"
       , labeling = labeling_values
       , value_type = "observed"
       , gp_text = gpar(fontface = 1)
       , rot_labels = c(top = -20))

# The Friendly colour scheme suits visual analysis
# Bolder colours and carries more information in the outlines
mosaic(haireye
       , gp = shading_Friendly # shade = TRUE
       , main="Actual frequencies"
       , labeling = labeling_values
       , value_type = "observed"
       , gp_text = gpar(fontface = 1)
       , rot_labels = c(top = -20))

## ----hec-----------------------------------------------------------------
# showing all three dimensions
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"), ]
mosaic(HEC
       , gp = shading_Friendly # shade = TRUE
       , main="Actual frequencies"
       , labeling = labeling_values
       , value_type = "observed"
       , gp_text = gpar(fontface = 1), rot_labels = c(right = -45)) 

# with a different pivot
mosaic(aperm(HEC, 3:1)
       , gp = shading_Friendly # shade = TRUE
       , main="Actual frequencies"
       , labeling = labeling_values
       , value_type = "observed"
       , gp_text = gpar(fontface = 1), rot_labels = c(right = -45)) 

## ----hec_joint_model-----------------------------------------------------
# convert to frequency form
HEC_df <- as.data.frame(HEC)

# explanatory model, hair eye interaction
hec.glm <- glm(Freq~Hair*Eye+Sex, data=HEC_df, family = poisson)

mosaic(hec.glm
       , gp = shading_Friendly
       , formula = ~ Sex + Eye + Hair
       , residuals_type = "rstandard")

## ----titanic_mosaic------------------------------------------------------
mosaic(aperm(Titanic, c(4, 2, 1, 3))
       , gp = shading_Friendly # shade = TRUE
       , main="Who Died and Who Survived the Titanic?"
       , labeling = labeling_values
       , value_type = "observed"
       , gp_text = gpar(fontface = 1), rot_labels = c(right = -45))

## ----titanic_cotab-------------------------------------------------------
# This doesn't appear to render well on the HTML output
# note, this is all the defaults except the shading!
cotabplot(Titanic, gp = shading_Friendly)

