# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use maximum likelihood to estimate 'lambda' 
# of a Poisson given hypothetical count data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define negative log likelihood function
nlL.pois <- function(lambda){
  -sum(dpois(k, lambda, log = TRUE))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# True value of 'lambda' to simulate counts
lambda.true <- 30.333 

# Sample size 'n' of simulated counts
n <- 2

# Generate 'n' random visitation counts 'k'
k <- rpois(n, lambda.true )

# Visualize the counts
hist(k, breaks = 0:max(k))

abline(v = lambda.true,
       lwd = 2,
       col = 'red')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Provide initial guess for 'lambda'
init.par <- list(lambda = 2)

# Fit the model (suppressing the optim() 1D warning)
suppressWarnings(
  fit <- optim(init.par, nlL.pois)
)

print(fit)
# en el fit, el value lo que significa es el conjunto de negative log likelihood de todas
# las observaciones
# counts means la de veces que ha estado contando. El gradiente nos dice como cerca de cero estamos
# converge de cero dice que ha podido converger perfectamente. lo que queremos es encontrar un cero 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add estimate to visualization
abline(v = fit$par,
       lwd = 2,
       col = 'blue')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


