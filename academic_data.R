library(gtools)
library(vcd)

expit <- function(x) exp(x)/ (1 + exp(x))

set.seed(5043)
n <- rpois(1, 8400)
grad_year <- c(2017, 2018)
prob_year <- rbeta(1, 480, 500)
faculty_business <- c("fin", "law"
                      , "mgmt", "hsp"
                      , "hr")
prob_faculty <- rdirichlet(1, c(90, 70, 80, 20, 35))
prob_faculty_year <- c(rbeta(1, 590, 410)
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
prob_nat <- rbeta(1, 30, 2)
fin <- c("self", "spons")
prob_fin <- rbeta(1, 20, 1)
prob_nat_fin <- rbeta(1, 100, 20)
genders <- c("F", "M")

natmix <- character(n)
finance <- character(n)
gender <- character(n)
for(yr in unique(year)) {
  prob_faculty_fin <- prob_fin * rbeta(5, 50, 1)
  prob_faculty_nat_fin <- prob_nat_fin * rbeta(5, 50, 1)
  names(prob_faculty_fin) <- faculty_business
  names(prob_faculty_nat_fin) <- faculty_business
  for(fc in faculty_business) {
    gn <- sample(genders
                 , size = length(year[year == yr & faculty == fc])
                 , prob = c(0.51, 0.49)
                 , replace = TRUE)
    nm <- sort(sample(nat
                      , size = length(year[year == yr & faculty == fc])
                      , prob = c(prob_nat, 1 - prob_nat)
                      , replace = TRUE))
    fin_dom <- sample(fin
                      , size = length(nm[nm=="dom"])
                      , prob = c(prob_faculty_fin[fc], 1 - prob_faculty_fin[fc])
                      , replace = TRUE)
    fin_int <- sample(fin
                      , size = length(nm[nm=="int"])
                      , prob = c(prob_faculty_nat_fin[fc], 1 - prob_faculty_nat_fin[fc])
                      , replace = TRUE)
    
    nat_fin_shuffle <- sample(length(nm))
    nm <- nm[nat_fin_shuffle]
    fn <- c(fin_dom, fin_int)[nat_fin_shuffle]
    
    natmix[year == yr & faculty == fc] <- nm
    finance[year == yr & faculty == fc] <- fn
    gender[year == yr & faculty == fc] <- gn    
  }
}

natmix <- factor(natmix)
finance <- factor(finance)
gender <- factor(gender)

quals <- c("dip", "dip-other", "hs", "hs-equiv")
prob_quals <- as.vector(rdirichlet(1, c(8, 6, 4, 1)))
prob_quals_int <- as.vector(rdirichlet(1, c(10, 6, 10, 5)))
prob_quals_fin <- as.vector(rdirichlet(1, c(8, 2, 10, 1)))
prob_quals_int_fin <- as.vector(rdirichlet(1, c(10, 1, 12, 1)))

hqual <- character(n)

for (fc in faculty_business) {
  selector <- natmix == "dom" & finance == "self" & faculty == fc
  hqual[selector] <- 
    sample(quals
           , size = length(hqual[selector])
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
  for(i in 1:n) {
    if((hqual[i] == "dip" | hqual[i] == "dip-other") &
       gender[i] == "F" & runif(1) > 0.95) {
      hqual[i] <- sample(quals[3:4]
                         , size = 1
                         , prob = c(0.8, 0.2))
    }
  }  
}

hqual <- factor(hqual)

logit_abs_rate <- -3.178054 +
  0.1 * (finance == "self") +
  0.2 * (natmix == "dom") +
  0.15 * (hqual == "dip") +
  0.1 * (hqual == "dip-other") +
  0.075 * (hqual == "hs-equiv") +
  0.075 * (gender == "M") +
  0.2 * (faculty == "hsp") +
  0.15 * (faculty == "mgmt") +
  0.05 * (faculty == "hr") +
  0.1 * (finance == "self" & natmix == "dom") +
  0.1 * ((hqual == "dip" | hqual == "dip-other") & gender == "M") +
  0.1 * (faculty == "hsp" & gender == "M") +
  0.05 * (faculty == "mgmt" & gender == "M") +
  0.05 * (faculty == "law" & gender == "M" & finance == "self") +
  -0.25 * (faculty == "hr" & gender == "F" & finance == "self") +
  rnorm(n, sd = 0.05) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = n
         , prob = c(0.005, 0.995)
         , replace = TRUE) * runif(n, 0, 2.5) +
  (sample(c(TRUE, FALSE) # create some outliers, based on gender (more females had caring at home)
         , size = n
         , prob = c(0.01, 0.99)
         , replace = TRUE) & gender == "F") * runif(n, 0, 1)

logit_t1_success <- 2 +
  (logit_abs_rate + abs(min(logit_abs_rate))) * -0.4 +
  0.05 * (gender == "M") +
  0.2 * (faculty == "mgmt") +
  0.1 * (faculty == "hr") +
  0.15 * (faculty == "hr" & gender == "F") +
  0.2 * (hqual == "hs") +
  -0.05 * (hqual == "dip-other") +
  0.1 * (hqual == "hs-equiv") +
  0.1 * (finance == "self" & natmix == "int") +
  rnorm(n, sd = 0.1) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = n
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(n, 0, 1)

abs_rate <- expit(logit_abs_rate)
t1_success <- logical(n)
for(i in 1:n) {
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
  0.05 * (faculty == "law") +
  0.1 * (faculty == "mgmt") +
  0.2 * (faculty == "fin") +
  0.2 * (faculty == "hr" & gender == "M") +
  0.1 * (hqual == "dip") +
  0.15 * (hqual == "dip-other") +
  0.1 * (finance == "self" & natmix == "int") +
  rnorm(n, sd = 0.1) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = n
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(n, 0, 2)

withdraw <- logical(n)
for(i in 1:n) {
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
  -0.1 * (faculty == "law") +
  -0.25 * (faculty == "fin") +
  -0.2 * (faculty == "hr" & gender == "F") +
  -0.1 * (hqual == "dip") +
  -0.15 * (hqual == "dip-other") +
  -0.1 * (finance == "self" & natmix == "int") +
  -2 * (year == 2017) +
  rnorm(n, sd = 0.2) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = n
         , prob = c(0.02, 0.98)
         , replace = TRUE) * runif(n, 0, 1)

withdraw <- logical(n)
for(i in 1:n) {
  withdraw[i] <- sample(c(TRUE, FALSE)
                        , size = 1
                        , prob = c(expit(logit_withdraw[i])
                                   , 1 - expit(logit_withdraw[i])))
}
withdraw <- factor(withdraw)

defer <- logical(n)
for(i in 1:n) {
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
  0.1 * (faculty == "hsp") +
  0.2 * (faculty == "mgmt") +
  -0.05 * (faculty == "fin") +
  0.1 * (faculty == "hr" & gender == "M") +
  0.2 * (hqual == "hs") +
  0.15 * (hqual == "hs-equiv") +
  0.15 * (finance == "spons") +
  0.15 * (natmix == "int") +
  0.05 * (finance == "self" & natmix == "dom") +
  0.2 * ((faculty == "hsp" | faculty == "mgmt") & gender == "F") +
  -0.1 * (faculty == "law" & hqual == "dip") +
  -0.15 * (faculty == "law" & hqual == "dip-other") +
  -0.2 * (faculty == "fin" & hqual == "dip") +
  -0.25 * (faculty == "fin" & hqual == "dip-other") +
  0.05 * (faculty == "hr" & gender == "F" & finance == "self") +
  rnorm(n, sd = 0.1) +
  sample(c(TRUE, FALSE) # create some outliers
         , size = n
         , prob = c(0.01, 0.99)
         , replace = TRUE) * runif(n, 0, 2)
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
mosaic(table(faculty, natmix), shade = TRUE)
mosaic(table(faculty, finance), shade = TRUE)
mosaic(table(faculty, natmix, year), shade = TRUE)
mosaic(table(faculty, finance, year), shade = TRUE)

mosaic(table(natmix, hqual), shade = TRUE)
mosaic(table(finance, hqual), shade = TRUE)
mosaic(table(gender, hqual), shade = TRUE)
mosaic(table(faculty, hqual), shade = TRUE)

mosaic(table(t1_success, hqual, gender), shade = TRUE)
mosaic(table(t1_success, faculty, gender), shade = TRUE)
mosaic(table(t1_success, natmix), shade = TRUE)
mosaic(table(t1_success, year), shade = TRUE)
mosaic(table(t1_success, withdraw), shade = TRUE)
mosaic(table(faculty, withdraw), shade = TRUE)
mosaic(table(faculty, grad)[, c(5, 4, 2, 3, 1, 6)], shade = TRUE)
mosaic(table(year, grad)[, c(5, 4, 2, 3, 1, 6)], shade = TRUE)
mosaic(table(grad, year, natmix)[c(5, 4, 2, 3, 1, 6), ,], shade = TRUE)
mosaic(table(grad, year)[c(5, 4, 2, 3, 1, 6) ,], shade = TRUE)
