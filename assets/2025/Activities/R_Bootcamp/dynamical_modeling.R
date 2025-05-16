install.packages('deSolve')
library(deSolve)

# SIR model definition
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Parameters and initial state definition
params <- c(beta = 0.3, gamma = 0.1)
state <- c(S = 999, I = 1, R = 0)

# Time span for simulation:
times <- seq(0, 100, by = 1)

# Solving the model
output <- ode(y = state, times = times, func = sir_model, parms = params)

# Converting output to a Dataframe
output_df <- as.data.frame(output)

#Plotting
library(ggplot2)

ggplot(output_df, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), size = 1) +
  geom_line(aes(y = I, color = "Infected"), size = 1) +
  geom_line(aes(y = R, color = "Recovered"), size = 1) +
  labs(title = "SIR Model Simulation",
       x = "Time (Days)",
       y = "Number of Individuals",
       color = "Compartment") +
  theme_minimal()



