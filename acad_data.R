library(dplyr)
library(vcd)
library(vcdExtra)
# library(gam)
library(car)
library(effects)

expit <- function(x) exp(x)/ (1 + exp(x))
expit_prob <- function(x) c(expit(x), 1 - expit(x))
random_binary_from_logits <- function(lgt) {
  factor(sapply(lgt, function(x) {
    sample(c(TRUE, FALSE)
           , size = 1
           , prob = expit_prob(x))
  }))}

my_mosaic <- function(x) {
  rtype <- if(class(x) == "table") {"deviance" }
            else {"rstandard"}
  mosaic(x
         , residuals_type = rtype
         , formula = ~ faculty + hqual + grad + year
         , gp = shading_Friendly2
         , rot_labels = c(0, -30, -45, 90)
         , rot_varnames = c(0, -90, 0, 90)
         , offset_labels = c(0, 0.5, 0, 0)
         , offset_varnames = c(0, 1, 0, 0.5))
}

year_intake <- c(2014, 2015)
faculty_business <- c("fin", "law"
                      , "mgmt", "hsp"
                      , "hr")
nat <- c("dom", "int")
fin <- c("self", "spons")
genders <- c("F", "M")
quals <- c("dip", "dip-other", "hs", "hs-equiv")

acad_data <- function() {
  set.seed(2024)
  
  pois_parms <- expand.grid(year = year_intake
                            , faculty = faculty_business
                            , natmix = nat
                            , finance = fin
                            , gender = genders
                            , hqual = quals)
  
  reps <- 2 # 2 per year
  pois_parms <- pois_parms %>%
    # main effects
    mutate(lamb = ifelse(year == year_intake[1], 250, 300)
           , lamb = ifelse(gender == genders[1]
                           , lamb * 0.92
                           , lamb * 0.90)
           , lamb = ifelse(natmix == nat[1]
                           , lamb
                           , lamb * 0.15)
           , lamb = ifelse(finance == fin[1]
                           , lamb
                           , lamb * 0.1)
           # joint effects nat fin
           , lamb = ifelse(natmix == nat[2] & finance == fin[2]
                           , lamb
                           , lamb * 0.7)
           # joint effects nat year
           , lamb = ifelse(natmix == nat[2] & year == year_intake[2]
                           , lamb * 0.7
                           , lamb)
           
           # main effects
           , lamb = ifelse(faculty == faculty_business[2]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[3]
                           , lamb * 0.9
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[4]
                           , lamb * 0.3
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[5]
                           , lamb * 0.25
                           , lamb)
           
           # joint year faculty
           , lamb = ifelse(faculty == faculty_business[1] &
                             year == year_intake[1]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[2] &
                             year == year_intake[1]
                           , lamb * 0.75
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[3] &
                             year == year_intake[1]
                           , lamb * 1.2
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[4] &
                             year == 1.1
                           , lamb * 1.1
                           , lamb)
           # main effect
           , lamb = ifelse(hqual == quals[2]
                           , lamb * 0.25
                           , lamb)
           , lamb = ifelse(hqual == quals[3]
                           , lamb * 0.7
                           , lamb)
           , lamb = ifelse(hqual == quals[4]
                           , lamb * 0.4
                           , lamb)
           
           # joint effects quals and gender
           # gender qual
           , lamb = ifelse(gender == genders[1] &
                             hqual == quals[1]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(gender == genders[1] &
                             hqual == quals[2]
                           , lamb * 0.7
                           , lamb)
           , lamb = ifelse(gender == genders[2] &
                             faculty == faculty_business[1]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(gender == genders[1] &
                             faculty == faculty_business[3]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(gender == genders[2] &
                             faculty == faculty_business[5]
                           , lamb * 0.7
                           , lamb)
           
           # joint effects quals and faculty
           , lamb = ifelse(faculty == faculty_business[1] &
                             hqual == quals[1]
                           , lamb * 0.75
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[1] &
                             hqual == quals[2]
                           , lamb * 0.5
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[1] &
                             hqual == quals[3]
                           , lamb * 1.2
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[2] &
                             hqual == quals[3]
                           , lamb
                           , lamb * 0.9)
           , lamb = ifelse(faculty == faculty_business[2] &
                             hqual == quals[4]
                           , lamb
                           , lamb * 0.8)
           , lamb = ifelse(faculty == faculty_business[3] &
                             hqual == quals[1]
                           , lamb * 1.2
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[3] &
                             hqual == quals[2]
                           , lamb * 1.1
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[3] &
                             hqual == quals[3]
                           , lamb * 0.7
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[3] &
                             hqual == quals[4]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[4] &
                             hqual == quals[1]
                           , lamb * 1.2
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[4] &
                             hqual == quals[2]
                           , lamb * 1.1
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[4] &
                             hqual == quals[3]
                           , lamb * 0.7
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[4] &
                             hqual == quals[4]
                           , lamb * 0.5
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[5] &
                             hqual == quals[1]
                           , lamb * 0.95
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[5] &
                             hqual == quals[2]
                           , lamb * 0.9
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[5] &
                             hqual == quals[3]
                           , lamb * 0.75
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[5] &
                             hqual == quals[4]
                           , lamb * 0.7
                           , lamb)

           # joint effects year and quals and faculty
           , lamb = ifelse(faculty == faculty_business[1] &
                             hqual == quals[1] &
                             year == year_intake[1]
                           , lamb * 0.8
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[1] &
                             hqual == quals[2] &
                             year == year_intake[1]
                           , lamb * 0.85
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[1] &
                             hqual == quals[3] &
                             year == year_intake[1]
                           , lamb * 1.2
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[2] &
                             hqual == quals[1] &
                             year == year_intake[1]
                           , lamb * 0.95
                           , lamb)
           , lamb = ifelse(faculty == faculty_business[2] &
                             hqual == quals[2] &
                             year == year_intake[1]
                           , lamb * 0.9
                           , lamb)
           # joint effects natmix, finance and qual
           , lamb = ifelse(natmix == nat[2] &
                             hqual == quals[1]
                           , lamb * 0.6
                           , lamb)
           , lamb = ifelse(natmix == nat[2] &
                             hqual == quals[2]
                           , lamb * 0.2
                           , lamb)
           , lamb = ifelse(natmix == nat[2] &
                             hqual == quals[4]
                           , lamb * 1.1
                           , lamb)
           , lamb = ifelse(finance == fin[2] &
                             hqual == quals[1]
                           , lamb * 0.3
                           , lamb)
           , lamb = ifelse(finance == fin[2] &
                             hqual == quals[2]
                           , lamb * 0.1
                           , lamb)
           , lamb = ifelse(finance == fin[2] &
                             hqual == quals[4]
                           , lamb * 1.2
                           , lamb)
           , lamb = ifelse(natmix == nat[2] &
                             faculty == faculty_business[1]
                           , lamb * 0.7
                           , lamb)
           , lamb = ifelse(natmix == nat[2] &
                             faculty == faculty_business[2]
                           , lamb * 0.75
                           , lamb)
           , lamb = ifelse(natmix == nat[2] &
                             faculty == faculty_business[3]
                           , lamb * 1.3
                           , lamb)
           , lamb = ifelse(finance == fin[2] &
                             hqual %in% quals[3:4] &
                             faculty %in% faculty_business[1:2]
                           , lamb * 1.3
                           , lamb)
           , lamb = ifelse(finance == fin[1] &
                             hqual %in% quals[3:4] &
                             faculty %in% faculty_business[1:2]
                           , lamb * 1.1
                           , lamb)
           , lamb = ifelse(finance == fin[2] &
                             hqual %in% quals[3:4] &
                             faculty %in% faculty_business[3]
                           , lamb * 1.4
                           , lamb)
           , lamb = ifelse(natmix == nat[2] &
                             hqual %in% quals[2] &
                             faculty %in% faculty_business[4:5]
                           , lamb * 0.7
                           , lamb)
           , lamb = ifelse(natmix == nat[1] &
                             hqual %in% quals[1:2] &
                             faculty %in% faculty_business[3:4]
                           , lamb * 1.4
                           , lamb)
    )
  
  students <- 1:nrow(pois_parms) # minimum 1
  for (i in 1:nrow(pois_parms)) {
    students <- c(students
                  , rep(i, sum(rpois(reps
                                     , pois_parms$lamb[i]))))
  }
  
  Freq <- table(students)
  students <- cbind(pois_parms, Freq)
  students <- expand.dft(students)
  
  n <- nrow(students)
  
  students <- students %>%
    mutate(year = factor(year)
           , logit_abs_rate = -3.2 +
             0.1 * (finance == fin[1]) +
             -0.2 * (finance == fin[2]) +
             0.1 * (natmix == nat[1]) +
             -0.2 * (natmix == nat[2]) +
             0.2 * (hqual == quals[1]) +
             0.25 * (hqual == quals[2]) +
             0.075 * (hqual == quals[4]) +
             -0.05 * (gender == genders[1]) +
             0.15 * (gender == genders[2]) +
             0.15 * (faculty == faculty_business[3]) +
             0.2 * (faculty == faculty_business[4]) +
             0.05 * (faculty == faculty_business[5]) +
             0.2 * (finance == fin[1] & natmix == nat[1]) +
             0.1 * (hqual %in% quals[1:2] & gender == genders[2]) +
             0.05 * (faculty == faculty_business[3] & gender == genders[2]) +
             0.075 * (faculty == faculty_business[4] & gender == genders[2]) +
             0.05 * (faculty == faculty_business[2] & gender == genders[2] & finance == fin[1]) +
             -0.05 * (faculty == faculty_business[5] & gender == genders[1] & finance == fin[1]) +
             rnorm(n, sd = 0.15) +
             sample(c(TRUE, FALSE) # create some outliers
                    , size = n
                    , prob = expit_prob(-5.5) # 0.005, 0.995
                    , replace = TRUE) * runif(n, 0, 2.5) +
             (sample(c(TRUE, FALSE) # create some outliers, based on gender (more females had caring at home)
                     , size = n
                     , prob = expit_prob(-4.5) # 0.01, 0.99
                     , replace = TRUE) & gender == genders[1]) * runif(n, 0, 1)
           , abs_rate = expit(logit_abs_rate)
           , logit_t1_success = 2 +
             (logit_abs_rate + abs(min(logit_abs_rate))) * -0.2 +
             0.05 * (gender == genders[1]) +
             0.22 * (faculty == faculty_business[3]) +
             0.15 * (faculty == faculty_business[5]) +
             0.05 * (faculty == faculty_business[5] & gender == genders[1]) +
             0.05 * (hqual == quals[3]) +
             -0.05 * (hqual == quals[2]) +
             0.05 * (hqual == quals[4]) +
             0.1 * (finance == fin[1] & natmix == nat[2]) +
             0.1 * (finance == fin[2] & natmix == nat[1]) +
             rnorm(n, sd = 0.15) +
             sample(c(TRUE, FALSE) # create some outliers
                    , size = n
                    , prob = c(0.01, 0.99)
                    , replace = TRUE) * runif(n, 0, 0.5)
           , t1_success = random_binary_from_logits(logit_t1_success)
           , logit_withdraw = -3.5 +
             (logit_abs_rate + abs(min(logit_abs_rate))) * 0.25 +
             0.3 * (t1_success == FALSE) +
             0.15 * (natmix == nat[2]) +
             0.2 * (gender == genders[2]) +
             0.05 * (faculty == faculty_business[2]) +
             0.1 * (faculty == faculty_business[3]) +
             0.15 * (faculty == faculty_business[1]) +
             0.05 * (faculty == faculty_business[5] & gender == genders[2]) +
             0.05 * (hqual == quals[1]) +
             0.1 * (hqual == quals[2]) +
             0.1 * (finance == fin[1] & natmix == nat[2]) +
             rnorm(n, sd = 0.15) +
             sample(c(TRUE, FALSE) # create some outliers
                    , size = n
                    , prob = c(0.01, 0.99)
                    , replace = TRUE) * runif(n, 0, 2)
           , withdraw = random_binary_from_logits(logit_withdraw)
           , logit_defer = -3 +
             (logit_abs_rate + abs(min(logit_abs_rate))) * 0.25 +
             0.1 * (t1_success == FALSE) +
             0.2 * (natmix == nat[2]) +
             -0.15 * (gender == genders[1]) +
             -0.1 * (faculty == faculty_business[2]) +
             -0.15 * (faculty == faculty_business[1]) +
             -0.1 * (faculty == faculty_business[5] & gender == genders[1]) +
             -0.05 * (hqual == quals[1]) +
             -0.075 * (hqual == quals[2]) +
             -0.1 * (finance == fin[1] & natmix == nat[2]) +
             -2 * (year == year_intake[1]) + # most of the earlier year defferals have passed through by now
             rnorm(n, sd = 0.15) +
             sample(c(TRUE, FALSE) # create some outliers
                    , size = n
                    , prob = expit_prob(-3.5) # 0.02, 0.98
                    , replace = TRUE) * runif(n, -1, 1) +
             (sample(c(TRUE, FALSE) # create some outliers, based on gender (more females had caring at home)
                     , size = n
                     , prob = expit_prob(-3.5) # 0.02, 0.98
                     , replace = TRUE) & gender == genders[1]) * runif(n, 0, 1)
           , defer = random_binary_from_logits(logit_defer)
           , logit_outcome = expit(1.1) + # 65% baseline
             (logit_abs_rate + abs(min(logit_abs_rate))) * -0.25 +
             0.125 * (t1_success == TRUE) +
             -0.125 * (t1_success == FALSE) +
             0.1 * (natmix == nat[2]) +
             -0.05 * (natmix == nat[1]) +
             0.025 * (gender == genders[1]) +
             -0.025 * (gender == genders[2]) +
             0.15 * (faculty == faculty_business[4]) +
             0.1 * (faculty == faculty_business[3]) +
             -0.05 * (faculty == faculty_business[2]) +
             -0.1 * (faculty == faculty_business[1]) +
             0.05 * (faculty == faculty_business[5] & gender == genders[2]) +
             -0.1 * (faculty == faculty_business[1] & gender == genders[2]) +
             0.075 * (hqual == quals[3]) +
             0.05 * (hqual == quals[4]) +
             -0.05 * (hqual == quals[2]) +
             0.1 * (finance == fin[2]) +
             0.05 * (natmix == nat[2]) +
             -0.1 * (finance == fin[1]) +
             -0.1 * (natmix == nat[1]) +
             0.05 * (finance == fin[2] & natmix == nat[2]) +
             0.05 * ((faculty == faculty_business[4] | faculty == faculty_business[3]) & gender == genders[1]) +
             -0.075 * (faculty == faculty_business[2] & hqual == quals[1]) +
             -0.1 * (faculty == faculty_business[2] & hqual == quals[2]) +
             -0.1 * (faculty == faculty_business[1] & hqual %in% quals[1:2]) +
             0.15 * (faculty == faculty_business[1] & hqual == quals[3]) +
             0.1 * (faculty == faculty_business[1] & hqual == quals[4]) +
             0.025 * (faculty == faculty_business[5] & gender == genders[1] & finance == fin[1]) +
             rnorm(n, sd = 0.15) +
             sample(c(TRUE, FALSE) # create some outliers
                    , size = n
                    , prob = c(0.01, 0.99)
                    , replace = TRUE) * runif(n, -2, 2)
            , outcome = factor(case_when(withdraw == TRUE ~ "wthdr"
                                        # , defer == TRUE ~ "defer"
                                        , expit(logit_outcome) > 0.70 ~ "dist"
                                        , expit(logit_outcome) > 0.63 ~ "merit"
                                        , expit(logit_outcome) > 0.50 ~ "pass"
                                        , TRUE ~ "fail"
           ))
           , grad = factor(ifelse(outcome %in% c("pass", "merit", "dist")
                                  , TRUE, FALSE))
           
    )
  return(students)
}

students <- acad_data()

sts <- students[students$faculty %in% faculty_business[1:3], ]
sts$faculty <- factor(sts$faculty)
sts <- sts[,  c("year", "faculty", "hqual"
                     , "natmix", "finance", "gender"
                     , "t1_success", "abs_rate"
                     , "outcome", "grad")]