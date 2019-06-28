nrow(students)
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

mosaic(with(students, table(grad, faculty, hqual, year))
       , shade = TRUE
       , rot_labels = c(0, -60, -30, 90))

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

mosaic(glm3
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


glm3 <- glm(Freq~(faculty + hqual + grad)^2 + year
            , grad_stats_df
            , family = poisson)

mosaic(glm3
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

LRstats(glm3)

glm4 <- glm(Freq~(faculty + hqual + grad + year)^2
            , grad_stats_df
            , family = poisson)

mosaic(glm4
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))

LRstats(glm4)
anova(glm1, glm2, glm3, glm4)

grad_stats_df$dipfin <- factor(ifelse(grad_stats_df$faculty == "fin" &
                                     grad_stats_df$hqual == "dip" 
                                   , TRUE, FALSE))
grad_stats_df$dipmgmt <- factor(ifelse(grad_stats_df$faculty == "mgmt" &
                                     grad_stats_df$hqual == "dip" 
                                   , TRUE, FALSE))

glm5 <- glm(Freq~(faculty + hqual + grad)^2 + (faculty + hqual + grad)^3 + (faculty + hqual + year)^2
            , grad_stats_df
            , family = poisson)

mosaic(glm5
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))


glm6 <- glm(Freq~(faculty + hqual + grad + year)^2 + (faculty + hqual + grad)^3
            , grad_stats_df
            , family = poisson)

mosaic(glm6
       , shade = TRUE
       , formula = ~ faculty + grad + hqual + year
       , residuals_type = "rstandard"
       , rot_labels = c(0, -60, -30, 90))


anova(glm1, glm2, glm3, glm4, glm5, glm6)
LRstats(glm5)
LRstats(glm6)


