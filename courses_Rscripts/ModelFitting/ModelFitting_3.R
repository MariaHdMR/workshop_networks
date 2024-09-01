# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting models f1-f3 to a randomly-generated visitation rate dataset -------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set.seed(1)

n <- 40 # Sample size
P <- runif(n, 0, 100) # Plant densities

a <- 3.333 # True value of 'a'
h <- 0.025 # True value of 'h'

k <- NULL # Visitation counts
for(i in 1:n){
  # Generate a 'k' for each 'P' assuming the truth is Type 2 (model f2)
  k[i] <- rpois(1, a * P[i] / (1 + a * h * P[i]) )
}


plot(P, k,
     xlim = c(0, max(P)),
     ylim = c(0, max(k)),
     xlab = 'Plant density (P)',
     ylab = 'Visitation count (k)')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Type I (model f1) ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nlL.pois.f1 <- function(par){
  a <- par['a']
  -sum(dpois(k, a * P, log = TRUE))
}

fit.f1 <- optim(par = list(a = 1),
                fn = nlL.pois.f1)
print(fit.f1)

plot.f1 <- function(x, fit){
  a <- fit$par['a']
  return(a * x)
}
curve(plot.f1(x, fit.f1), 
      add = TRUE,
      lwd = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Continue yourself ------------------------------------------------------------
# Now fit the Type II (f2) and Type III (f3) models and 
# compare their 1) parameter MLEs and 2) likelihoods.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# type II ----

nlL.pois.f2 <- function(par){
  a <- par['a']
  h <- par['h']
  -sum(dpois(k, (a * P)/(1+ a* h* P), log = TRUE))
}

fit.f2 <- optim(par = list(a = 1, h= 1),
                fn = nlL.pois.f2)


print(fit.f2)

plot.f2 <- function(x, fit){
  a <- fit$par['a']
  h <- fit$par['h']
  return((a * x) / (1+a*h*x))
}
curve(plot.f2(x, fit.f2), 
      add = TRUE,
      lwd = 2)

# type III



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comparisons -----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parameter estimates ---------
print(c(a = a, h = h, theta = 1))


# Negative log-Likelihoods -----


# Define and apply functions for AIC and BIC ------



##############################################################################
##############################################################################
# The following is provided simply for you to have the final code of the 
# lesson plan.  You don't need to use it for the above in-class exercise.
##############################################################################
##############################################################################

# library(bbmle)
# 
# # bbmle needs the negative log likelihood function formulated differently
# nlL.pois.f1.bbmle <- function(a){
#   -sum(dpois(k, a * P, log = TRUE))
# }
# 
# fit <- mle2(nlL.pois.f1.bbmle, 
#             start = list(a = 0.2))
# print(fit)
# confint(fit)

##############################################################################
##############################################################################
##############################################################################