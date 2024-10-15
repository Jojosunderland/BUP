## Predator-Prey interactions
install.packages("deSolve")
library(deSolve)
library(ggplot2)

#LV logistic growth function

LG <- function(t,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
    return(list(dP)) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}

# LV Predator-Prey model

LV <- function(t,state,parameters){ ##lotka voltera function function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dx <- alpha * x - beta * x* y ## prey population equation 
    dy <- delta * x * y - gamma * y ## predator population
    return(list(c(dx, dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}

state <- c(x=10, y = 10) ## the initial population values of prey and predator populations
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.02, gamma = 0.4) ## the equation parameters
times <-  seq(from = 1, to = 500, by = 0.1) ##a sequence of time steps – uses function seq()
out <- ode(y= state, times = times, func = LV, parms = parameters)
out.df <- data.frame(out)

#plot the output
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

