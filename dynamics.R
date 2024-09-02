# dynamics

#exersice 1: 
#install.packages("deSolve")
library(deSolve)


# set initial conditions of response variable
popn <- c(N = 12)

# Parameters
parameters <- c(r = 0.2)

# Time sequence
times <- seq(0, 100, by = 1)

# "exp.model" stands for "exponential growth model" 
exp.model <- function(time, popn, parameters) {
  # exponential growth model, dNdt = r*N:
  dNdt <- parameters['r'] * popn['N']
  # the function must return a list (even if it is just one element):
  return(list(dNdt))
}

# Solve the ODE
out <- ode(y = popn, times = times, func = exp.model, parms = parameters)

# Plot the output
plot(out, ylab = "population size, N", col = "blue")

