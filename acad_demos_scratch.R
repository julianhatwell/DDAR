nrow(students)

my_mosaic(with(sts, table(faculty, grad, hqual, year)))

glm0 <- glm(Freq~faculty + hqual + year + grad
            , data = sts_freq
            , family = poisson)

my_mosaic(glm0)
summary(glm0)
anova(glm0)

ano <- anova(glm0, glm1, glm2, glm3, glm4, glm5)

glm1 <- glm(Freq~faculty * year + hqual + grad
            , data = sts_freq
            , family = poisson)

my_mosaic(glm1)
summary(glm1)
anova(glm0, glm1)

glm2 <- glm(Freq~faculty * (year + hqual) + grad
            , data = sts_freq
            , family = poisson)

my_mosaic(glm2)
summary(glm2)
anova(glm2)

glm3 <- glm(Freq~faculty * (hqual + year + grad)
            , data = sts_freq
            , family = poisson)

my_mosaic(glm3)
summary(glm3)
anova(glm3)

glm4 <- glm(Freq~(faculty * year) + (faculty * hqual * grad)
            , data = sts_freq
            , family = poisson)

my_mosaic(glm4)
summary(glm4)
anova(glm4)
LRstats(glm4)

glm5 <- glm(Freq~(faculty * hqual * year) + (faculty * hqual * grad)
            , data = sts_freq
            , family = poisson)

my_mosaic(glm5)
summary(glm5)
anova(glm5)

LRstats(glm5)

glm2 <- glm(Freq~(faculty + hqual)^2 + (grad + faculty)^2 + grad + year
            , as.data.frame(with(sts
                                 , table(faculty, hqual, grad, year)))
            , family = poisson)

mosaic(glm2
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))


glm1 <- glm(Freq~(faculty + hqual)^2 + grad + year
            , grad_stats_df
            , family = poisson)

mosaic(glm1
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

glm2 <- glm(Freq~(faculty + hqual)^2 + (grad + faculty)^2 + grad + year
            , grad_stats_df
            , family = poisson)

mosaic(glm2
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))




prop.table(with(students
                , table(year, outcome))[, c(1, 3, 4, 2, 5)]
           , 1)
my_mosaic(with(students
               , table(year, outcome))[, c(1, 3, 4, 2, 5)])

prop.table(with(students
                , table(year, grad)), 1)

my_mosaic(with(students
               , table(year, grad)))

prop.table(with(students
                , table(faculty, grad)), 1)

my_mosaic(with(students
               , table(faculty, grad)))

prop.table(with(students
                , table(hqual, grad)), 1)

my_mosaic(with(students
               , table(hqual, grad)))

my_mosaic(with(students, table(grad, faculty, hqual, year)))

grad_stats <- students[students$faculty %in% faculty_business[1:3], ]
grad_stats$faculty <- factor(grad_stats$faculty)
grad_stats_tbl <- with(grad_stats
                       , table(faculty, hqual, grad, year))

grad_stats_df <- as.data.frame(grad_stats_tbl)

glm1 <- glm(Freq~(faculty + hqual)^2 + grad + year
             , grad_stats_df
             , family = poisson)

mosaic(glm1
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

glm2 <- glm(Freq~(faculty + hqual)^2 + (grad + faculty)^2 + grad + year
            , grad_stats_df
            , family = poisson)

mosaic(glm2
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

glm3 <- glm(Freq~(faculty + hqual)^2 + (grad + faculty)^2 + (faculty + year)^2
            , grad_stats_df
            , family = poisson)

mosaic(glm3
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))


glm4 <- glm(Freq~(faculty + hqual + grad)^2 + year
            , grad_stats_df
            , family = poisson)

mosaic(glm4
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

LRstats(glm4)

glm5 <- glm(Freq~(faculty + hqual + grad + year)^2
            , grad_stats_df
            , family = poisson)

mosaic(glm5
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

LRstats(glm5)
anova(glm1, glm2, glm3, glm4, glm5)

grad_stats_df$dipfin <- factor(ifelse(grad_stats_df$faculty == "fin" &
                                     grad_stats_df$hqual == "dip" 
                                   , TRUE, FALSE))
grad_stats_df$dipmgmt <- factor(ifelse(grad_stats_df$faculty == "mgmt" &
                                     grad_stats_df$hqual == "dip" 
                                   , TRUE, FALSE))

glm6 <- glm(Freq~(faculty + hqual + grad)^2 + (faculty + hqual + grad)^3 + (faculty + hqual + year)^2
            , grad_stats_df
            , family = poisson)

mosaic(glm6
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))


glm7 <- glm(Freq~(faculty + hqual + grad + year)^2 + (faculty + hqual + grad)^3
            , grad_stats_df
            , family = poisson)

mosaic(glm7
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))


anova(glm1, glm2, glm3, glm4, glm5, glm6, glm7)
LRstats(glm5)
LRstats(glm6)





sts <- students[,  c("year", "faculty", "natmix"
                     , "finance", "gender", "hqual"
                     , "t1_success", "outcome", "grad")]

nrow(students)
prop.table(with(students
                , table(year, outcome))[, c(1, 3, 4, 2, 5)]
           , 1)

prop.table(with(students #[students$outcome != "defer", ]
                , table(year, grad)), 1)
prop.table(with(students#[students$outcome != "defer", ]
                , table(faculty, grad)), 1)
prop.table(with(students#[students$outcome != "defer", ]
                , table(hqual, grad)), 1)



plot(logit_t1_success~logit_abs_rate, data = students)
plot(logit_withdraw~logit_abs_rate, data = students)
plot(logit_defer~logit_abs_rate, data = students)
plot(logit_outcome~logit_abs_rate, data = students)
plot(logit_outcome~gender, data = students)
plot(logit_outcome~natmix, data = students)
plot(logit_outcome~finance, data = students)
plot(logit_outcome~hqual, data = students)
plot(logit_outcome~faculty, data = students)
plot(logit_outcome~year, data = students)

mosaic(with(students
            , table(year, outcome))[, c(1, 3, 4, 2, 5)]#[, c(2, 4, 5, 3, 1, 6)]
       , shade = TRUE
       , rot_labels = c(45, 0, 0, 90))

mosaic(with(students#[students$outcome != "defer", ]
            , table(year, grad))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))

prop.table(with(students#[students$outcome != "defer", ]
                , table(hqual, grad)), 1)

mosaic(with(students#[students$outcome != "defer", ]
            , table(hqual, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))

mosaic(with(students
            , table(faculty, hqual, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))

students$facfin <- factor(ifelse(students$faculty == "fin", TRUE, FALSE))
students$facmgmt <- factor(ifelse(students$faculty == "mgmt", TRUE, FALSE))

glm1a <- glm(Freq~(faculty + hqual)^2 + year
             , as.data.frame(
                     with(students
                          , table(faculty, hqual, year))
             )
             , family = poisson)

glm2 <- glm(Freq~(faculty + hqual + facfin)^2 + year
            , as.data.frame(
                    with(students
                         , table(faculty, hqual, year, facfin))
            )
            , family = poisson)

glm1b <- glm(Freq~(faculty + hqual)^2 + year
             , as.data.frame(
                     with(students
                          , table(faculty, hqual, year, facfin))
             )
             , family = poisson)


mosaic(glm1a
       , shade = TRUE
       , formula = ~ faculty + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, 45, 0, 90))

mosaic(glm1b
       , shade = TRUE
       , formula = ~ faculty + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, 45, 0, 90))


mosaic(glm2
       , shade = TRUE
       , formula = ~ faculty + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, 45, 0, 90))

LRstats(glm1b)
LRstats(glm2)

anova(glm1b, glm2)

mosaic(with(students[students$outcome != "defer", ]
            , table(hqual, faculty, grad, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))


mosaic(with(students[students$outcome != "defer", ]
            , table(year, grad, faculty))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))

mosaic(with(students, table(grad, faculty, year))
       , shade = TRUE
       , rot_labels = c(0, -90, 0, 90))

mosaic(with(students[students$outcome != "defer", ]
            , table(year, faculty, grad))
       , shade = TRUE
       , rot_labels = c(0, -90, 0, 90))


prop.table(with(students, table(grad, faculty, year)), 1:2)
prop.table(with(students[students$defer == FALSE, ]
                , table(grad, faculty, year)), 1:2)

mosaic(with(students[students$defer == FALSE, ]
            , table(grad, faculty, year))
       , shade = TRUE)
mosaic(with(students[students$defer == FALSE, ]
            , table(grad, faculty, year, hqual))
       , shade = TRUE
       , rot_labels = c(0, -90, 45, 90))


prop.table(with(students[students$defer == FALSE, ]
                , table(grad, faculty, year)), 1:2)

mosaic(with(students[students$defer == FALSE, ]
            , table(grad, hqual, year))
       , shade = TRUE)
prop.table(with(students[students$defer == FALSE, ]
                , table(grad, hqual, year)), 1:2)


mosaic(with(students[students$defer == FALSE, ]
            , table(faculty, hqual, year))
       , shade = TRUE)
prop.table(with(students[students$defer == FALSE, ]
                , table(faculty, hqual, year)), 1:2)



mosaic(structable(year~faculty, data = students), shade = TRUE)
mosaic(structable(hqual~faculty, data = students), shade = TRUE)
mosaic(structable(t1_success~faculty, data = students), shade = TRUE)
mosaic(structable(withdraw~faculty, data = students), shade = TRUE)
mosaic(structable(defer~faculty, data = students), shade = TRUE)

mosaic(with(students, table(natmix, finance)), shade = TRUE)
mosaic(with(students, table(natmix, finance, year))
       , shade = TRUE
       , rot_labels = c(0, 70, 0, 90))
mosaic(with(students, table(gender, year))
       , shade = TRUE)
mosaic(with(students, table(faculty, natmix))
       , shade = TRUE)
mosaic(with(students, table(faculty, finance))
       , shade = TRUE)
mosaic(with(students, table(faculty, natmix, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))
mosaic(with(students, table(faculty, finance, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))

mosaic(with(students, table(natmix, hqual)), shade = TRUE)
mosaic(with(students, table(finance, hqual)), shade = TRUE)
mosaic(with(students, table(gender, hqual)), shade = TRUE)
mosaic(with(students, table(faculty, hqual)), shade = TRUE)
mosaic(with(students, table(faculty, hqual, finance)), shade = TRUE)

mosaic(with(students, table(t1_success, hqual))
       , shade = TRUE)
mosaic(with(students, table(t1_success, faculty, gender))
       , shade = TRUE)
mosaic(with(students, table(t1_success, natmix))
       , shade = TRUE)
mosaic(with(students, table(t1_success, finance))
       , shade = TRUE)
mosaic(with(students, table(t1_success, year))
       , shade = TRUE)

mosaic(with(students, table(t1_success, withdraw))
       , shade = TRUE)
mosaic(with(students, table(faculty, withdraw))
       , shade = TRUE)
mosaic(with(students, table(faculty, outcome)[, c(2, 4, 5, 3, 1, 6)])
       , shade = TRUE)
mosaic(with(students, table(outcome, year, natmix)[c(2, 4, 5, 3, 1, 6), ,])
       , shade = TRUE)
mosaic(with(students, table(outcome, year)[c(2, 4, 5, 3, 1, 6) ,])
       , shade = TRUE)
mosaic(with(students, table(year, faculty, outcome)[, , c(2, 4, 5, 3, 1, 6)])
       , shade = TRUE)

mosaic(with(students, table(faculty, grad, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))

mosaic(with(students, table(grad, year))
       , shade = TRUE
       , rot_labels = c(0, 45, 0, 90))


summary(glm(grad~natmix+finance+year+faculty+hqual+t1_success+abs_rate
            , data=students
            , family = binomial))
summary(glm(grad~year+faculty
            , data=students
            , family = binomial))
summary(glm(grad~year*faculty
            , data=students
            , family = binomial))

glm1 <- glm(grad~year
            , data=students
            , family = binomial)
glm2 <- glm(grad~year+faculty
            , data=students
            , family = binomial)
glm3 <- glm(grad~year*faculty
            , data=students
            , family = binomial)
glm4 <- glm(grad~natmix+finance+year+faculty+hqual+t1_success+abs_rate
            , data=students
            , family = binomial)

preds <- factor(ifelse(predict(glm4, type = "response") > 0.5, TRUE, FALSE))
mean(preds == students$grad)
plot(Effect(c("faculty"
              , "hqual"
              , "year")
            , z.var = "year"
            , x.var = "hqual"
            , glm4)
     , multiline = TRUE
     , type = "response"
     , main = "Gender * Absence Rate * T1 Pass Rate"
     , xlab = "Pass Rate Term 1"
     , ylab = "P(Grad)"
     , rug = FALSE
     #, key.args = list(cex = 0.75)
     #, colors = myPalBrand
)


gam1 <- glm(grad~year*faculty
            , data=students
            , family = binomial)
gam2 <- glm(grad~year+faculty
            , data=students
            , family = binomial)
gam3 <- glm(grad~natmix+finance+year+faculty+hqual+t1_success+abs_rate
            , data=students
            , family = binomial)


Anova(glm2, glm1)
library(psych)

sts_ca <- ca(with(sts, table(hqual, faculty)))
plot(sts_ca)

my_gbars <- function(g) {
        g + stat_identity(geom = "bar"
                          , position = position_dodge()) +
                myGgFillScale4 +
                theme_bw() +
                myGgTheme
}

hf <- as.data.frame(with(sts, table(faculty, hqual)))
g1 <- ggplot(data = as.data.frame(hf)
             , aes(x = hqual
                   , fill = faculty
                   , y = Freq))
g1 <- my_gbars(g1)


g2 <- ggplot(data = as.data.frame(hf)
             , aes(x = faculty
                   , fill = hqual
                   , y = Freq))

g2 <- my_gbars(g2)

grid.arrange(g1, g2, nrow=1)


structable(year+grad~faculty+hqual
           , data = with(sts
                , table(faculty, hqual, grad, year)))


with(sts, table(faculty, hqual))













<!-- ## Loglinear Models -->
        
        <!-- ```{r freq_glm, echo=TRUE} -->
        <!-- # convert to frequency form data frame -->
        <!-- HEC_df <- as.data.frame(HEC) -->
        <!-- head(HEC_df, 3) -->
        
        <!-- # explanatory model, hair eye interaction -->
        <!-- hec.glm1 <- glm(Freq~Hair*Eye+Sex -->
                                     <!--                , data=HEC_df -->
                                     <!--                , family = poisson) -->
        <!-- ``` -->
        
        <!-- ## HEC Observed -->
        
        <!-- ```{r hec2, eval=TRUE} -->
        <!-- ``` -->
        
        
        <!-- ## HEC GLM -->
        
        <!-- ```{r hec_glm, echo=TRUE, eval=FALSE} -->
        <!-- mosaic(hec.glm1 # invokes the glm plotting method -->
                    <!--        , main="Hair*Eye" -->
                            <!--        , gp = shading_Friendly -->
                            <!--        , formula = ~ Sex + Eye + Hair # mosaic layout -->
                    <!--        , residuals_type = "rstandard") -->
        <!-- ``` -->
        
        <!-- ## HEC GLM -->
        
        <!-- ```{r hec_glm, eval=TRUE} -->
        <!-- ``` -->
        
        <!-- ## HEC GLM 2 -->
        
        <!-- ```{r glm2, echo=TRUE, eval=FALSE} -->
        <!-- # convert to frequency form data frame -->
        <!-- HEC_df$Blbl <- factor(ifelse(HEC_df$Hair == "Blond" & -->
                                                  <!--                                HEC_df$Eye == "Blue" -->
                                                  <!--                              , TRUE -->
                                                  <!--                              , FALSE)) -->
        
        <!-- # explanatory model, hair eye interaction -->
        <!-- hec.glm2 <- glm(Freq~Hair*Eye+Sex*Blbl -->
                                     <!--                , data=HEC_df -->
                                     <!--                , family = poisson) -->
        
        <!-- mosaic(hec.glm2 # invokes the glm plotting method -->
                    <!--        , main="Hair*Eye | Blond-Blue*Sex" -->
                            <!--        , gp = shading_Friendly -->
                            <!--        , formula = ~ Sex + Eye + Hair # mosaic layout -->
                    <!--        , residuals_type = "rstandard") -->
        <!-- ``` -->
        
        <!-- ## HEC GLM 2 -->
        
        <!-- ```{r glm2, eval=TRUE} -->
        <!-- ``` -->
        
        <!-- ## HEC GLM tests -->
        
        <!-- ```{r glmtests, echo=TRUE} -->
        <!-- anova(hec.glm1, hec.glm2) -->
        <!-- LRstats(hec.glm2) # compare to saturated -->

<!-- ``` -->
        
        # End
