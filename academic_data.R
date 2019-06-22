library(gtools)
library(vcd)
set.seed(5043)
n <- rpois(1, 8400)
grad_year <- c(2017, 2018)
prob_year <- rbeta(1, 480, 500)
faculty_business <- c("mgmt", "law"
                      , "fin", "hsp"
                      , "hr")
prob_faculty <- rdirichlet(1, c(80, 70, 80, 20, 30))
prob_faculty_year <- c(rbeta(1, 550, 450)
                            , rbeta(4, 480, 520))
joint_prob_faculty_year <- t(rbind(prob_faculty * prob_faculty_year
                                   , prob_faculty * (1- prob_faculty_year)))

year <- factor(sort(sample(grad_year
                      , size = n
                      , prob = c(prob_year, 1 - prob_year)
                      , replace = TRUE)))
faculty_2017 <- sample(faculty_business
                       , size = length(year[year==2017])
                       , prob = joint_prob_faculty_year[,1]
                       , replace = TRUE)
faculty_2018 <- sample(faculty_business
                       , size = length(year[year==2018])
                       , prob = joint_prob_faculty_year[,2]
                       , replace = TRUE)
faculty <- factor(c(faculty_2017, faculty_2018))

# these things won't differ significantly by year
nat <- c("dom", "int")
prob_nat <- rbeta(1, 25, 2)
fin <- c("self", "spons")
prob_fin <- rbeta(1, 10, 1)
prob_nat_fin <- rbeta(1, 100, 20)
genders <- c("F", "M")

natmix <- character(length(year))
finance <- character(length(year))
gender <- character(length(year))
for(yr in unique(year)) {
  gn <- sort(sample(genders
                    , size = length(year[year == yr])
                    , prob = c(0.51, 0.49)
                    , replace = TRUE))
  nm <- sort(sample(nat
                 , size = length(year[year == yr])
                 , prob = c(prob_nat, 1 - prob_nat)
                 , replace = TRUE))
  sponsmix_dom <- sample(fin
                         , size = length(nm[nm=="dom"])
                         , prob = c(prob_fin, 1 - prob_fin)
                         , replace = TRUE)
  sponsmix_int <- sample(fin
                         , size = length(nm[nm=="int"])
                         , prob = c(prob_nat_fin, 1 - prob_nat_fin)
                         , replace = TRUE)
  
  nat_fin_shuffle <- sample(length(nm))
  nm <- nm[nat_fin_shuffle]
  fn <- c(sponsmix_dom, sponsmix_int)[nat_fin_shuffle]
  
  natmix[year == yr] <- nm
  finance[year == yr] <- fn
  gender[year == yr] <- gn
}

natmix <- factor(natmix)
finance <- factor(finance)
gender <- factor(gender)

quals <- c("dip", "dip-other", "hs", "hs-equiv")
prob_quals <- as.vector(rdirichlet(1, c(8, 6, 4, 1)))
prob_quals_int <- as.vector(rdirichlet(1, c(10, 6, 10, 5)))
prob_quals_fin <- as.vector(rdirichlet(1, c(8, 2, 10, 1)))
prob_quals_int_fin <- as.vector(rdirichlet(1, c(10, 1, 12, 1)))

hqual <- character(length(year))
hqual[natmix == "dom" & finance == "self"] <- 
  sample(quals
         , size = length(hqual[natmix == "dom" & finance == "self"])
         , prob = prob_quals
         , replace = TRUE)
hqual[natmix == "int" & finance == "self"] <- 
  sample(quals
         , size = length(hqual[natmix == "int" & finance == "self"])
         , prob = prob_quals_int
         , replace = TRUE)
hqual[natmix == "dom" & finance == "spons"] <- 
  sample(quals
         , size = length(hqual[natmix == "dom" & finance == "spons"])
         , prob = prob_quals_fin
         , replace = TRUE)
hqual[natmix == "int" & finance == "spons"] <- 
  sample(quals
         , size = length(hqual[natmix == "int" & finance == "spons"])
         , prob = prob_quals_int_fin
         , replace = TRUE)
for(i in seq_along(hqual)) {
  if((hqual[i] == "dip" | hqual[i] == "dip-other") &
     gender[i] == "F" & runif(1) > 0.95) {
    hqual[i] <- sample(quals[3:4]
                       , size = 1
                       , prob = c(0.8, 0.2))
  }
}
hqual <- factor(hqual)

logit_abs_rate <- -3.178054 +
  0.1 * (finance == "self") +
  0.2 * (natmix == "dom") +
  0.05 * (hqual == "dip") +
  0.1 * (hqual == "dip-other") +
  0.075 * (hqual == "hs-equiv") +
  0.05 * (gender == "M") +
  0.2 * (faculty_business == "hsp") +
  0.15 * (faculty_business == "mgmt") +
  0.05 * (faculty_business == "fin") +
  0.1 * (finance == "self" & natmix == "dom") +
  0.05 * ((hqual == "dip" | hqual == "dip-other") & gender == "M") +
  0.05 * (faculty_business == "hsp" & gender == "M") +
  0.1 * (faculty_business == "mgmt" & gender == "M") +
  0.05 * (faculty_business == "law" & gender == "M") +
  -0.25 * (faculty_business == "hr" & gender == "F" & finance == "self") +
  rnorm(length(year), sd = 0.1) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = length(hqual)
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(length(hqual), 0, 2)

logit_t1_success <- 2.2 +
  (logit_abs_rate + abs(min(logit_abs_rate))) * -0.3 +
  0.05 * (gender == "M") +
  0.2 * (faculty_business == "law") +
  0.15 * (faculty_business == "mgmt") +
  0.25 * (faculty_business == "fin") +
  0.15 * (faculty_business == "hr" & gender == "F") +
  0.2 * (hqual == "hs") +
  0.05 * (hqual == "dip-other") +
  0.15 * (hqual == "hs-equiv") +
  0.1 * (finance == "self" & natmix == "int") +
  rnorm(length(year), sd = 0.3) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = length(hqual)
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(length(hqual), 0, 1)

abs_rate <- expit(logit_abs_rate)
t1_success <- logical(length(hqual))
for(i in seq_along(t1_success)) {
  t1_success[i] <- sample(c(TRUE, FALSE)
                              , size = 1
                              , prob = c(expit(logit_t1_success[i])
                                         , 1 - expit(logit_t1_success[i])))
}

t1_success <- factor(t1_success)

logit_withdraw <- -3.9 +
  (logit_abs_rate + abs(min(logit_abs_rate))) * 0.5 +
  1 * (t1_success == FALSE) +
  0.15 * (natmix == "int") +
  0.2 * (gender == "M") +
  0.05 * (faculty_business == "law") +
  0.1 * (faculty_business == "mgmt") +
  0.2 * (faculty_business == "fin") +
  0.2 * (faculty_business == "hr" & gender == "M") +
  0.1 * (hqual == "dip") +
  0.15 * (hqual == "dip-other") +
  0.1 * (finance == "self" & natmix == "int") +
  rnorm(length(year), sd = 0.1) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = length(hqual)
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(length(hqual), 0, 2)

withdraw <- logical(length(hqual))
for(i in seq_along(hqual)) {
  withdraw[i] <- sample(c(TRUE, FALSE)
                          , size = 1
                          , prob = c(expit(logit_withdraw[i])
                                     , 1 - expit(logit_withdraw[i])))
}
withdraw <- factor(withdraw)

logit_defer <- -3.5 +
  (logit_abs_rate + abs(min(logit_abs_rate))) * 0.75 +
  0.75 * (t1_success == FALSE) +
  0.2 * (natmix == "int") +
  -0.3 * (gender == "F") +
  -0.1 * (faculty_business == "law") +
  -0.25 * (faculty_business == "fin") +
  -0.2 * (faculty_business == "hr" & gender == "F") +
  -0.1 * (hqual == "dip") +
  -0.15 * (hqual == "dip-other") +
  -0.1 * (finance == "self" & natmix == "int") +
  -2 * (year == 2017) +
  rnorm(length(year), sd = 0.2) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = length(hqual)
         , prob = c(0.02, 0.98)
         , replace = TRUE) * runif(length(hqual), 0, 1)

withdraw <- logical(length(hqual))
for(i in seq_along(hqual)) {
  withdraw[i] <- sample(c(TRUE, FALSE)
                        , size = 1
                        , prob = c(expit(logit_withdraw[i])
                                   , 1 - expit(logit_withdraw[i])))
}
withdraw <- factor(withdraw)

defer <- logical(length(hqual))
for(i in seq_along(hqual)) {
  defer[i] <- sample(c(TRUE, FALSE)
                        , size = 1
                        , prob = c(expit(logit_defer[i])
                                   , 1 - expit(logit_defer[i])))
}

defer <- factor(ifelse(withdraw == TRUE, FALSE, defer))



logit_grad <- 0 +
  (logit_abs_rate + abs(min(logit_abs_rate))) * -0.5 +
  1 * (t1_success == TRUE) +
  0.1 * (natmix == "int") +
  0.1 * (gender == "F") +
  0.2 * (faculty_business == "hsp") +
  0.1 * (faculty_business == "mgmt") +
  -0.05 * (faculty_business == "fin") +
  0.1 * (faculty_business == "hr" & gender == "M") +
  0.2 * (hqual == "hs") +
  0.15 * (hqual == "hs-equiv") +
  0.15 * (finance == "spons") +
  0.15 * (natmix == "int") +
  0.05 * (finance == "self" & natmix == "dom") +
  0.2 * (faculty_business == "hsp" & gender == "F") +
  0.1 * (faculty_business == "mgmt" & gender == "F") +
  0.05 * (faculty_business == "law" & gender == "F") +
  0.25 * (faculty_business == "hr" & gender == "F" & finance == "self") +
  rnorm(length(year), sd = 0.1) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = length(hqual)
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(length(hqual), 0, 2)
grad <- ifelse(withdraw == TRUE, "withdrawn"
               , ifelse(defer == TRUE, "defer"
                        , ifelse(expit(logit_grad) > 0.8
                                 , "dist"
                                 , ifelse(expit(logit_grad) > 0.7
                                          , "merit"
                                          , ifelse(expit(logit_grad) > 0.5
                                                   , "pass", "fail")))))

grad <- factor(grad)

mosaic(table(faculty, year), shade = TRUE)
mosaic(table(natmix, finance), shade = TRUE)
mosaic(table(natmix, finance, year), shade = TRUE)
mosaic(table(gender, year), shade = TRUE)
mosaic(table(natmix, finance, hqual), shade = TRUE)
mosaic(table(gender, hqual), shade = TRUE)
mosaic(table(gender, t1_success, hqual), shade = TRUE)
mosaic(table(gender, t1_success, faculty), shade = TRUE)
mosaic(table(t1_success, natmix), shade = TRUE)
mosaic(table(t1_success, year), shade = TRUE)
mosaic(table(faculty, hqual), shade = TRUE)
mosaic(table(t1_success, withdraw), shade = TRUE)
mosaic(table(faculty, withdraw), shade = TRUE)
mosaic(table(faculty, grad), shade = TRUE)
mosaic(table(year, grad), shade = TRUE)

