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

#plot the output, this gives us the change in the two populations through time
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

# plot the change in populations through phase space 
# plot one predator population as a function of the prey population
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

## TASK: Change the parameters and initial conditions
## Q: What happens when you increase/decrease them?
## A: When you increase initial pop size, the circle increases, when you increase the parameters it decreases


## Prey growth rate: exponential vs logistic
# in the original LV model, prey growth is exponential, lets make it logistic

LV_log <- function(t,state,parameters){ ##lotka voltera function function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dx <- alpha * (1 - x/K) - beta * x* y ## prey population equation 
    dy <- delta * x * y - gamma * y ## predator population
    return(list(c(dx, dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}

# set K to 30
state <- c(x=10, y = 10) ## the initial population values of prey and predator populations
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.02, gamma = 0.4, K = 30) ## the equation parameters
times <-  seq(from = 1, to = 500, by = 0.1) ##a sequence of time steps – uses function seq()
out <- ode(y= state, times = times, func = LV_log, parms = parameters)
out.df <- data.frame(out)

## Q: How does it change  the results for the same set of parameter values listed above? Discuss (i.e. try to figure out why)



## Incorporating function response
# The rate at which predators can consume preys is called a functional response

# Type 2 functional response
x <- seq(0,50,0.1)
A <- 0.005  ## A is the hunting efficiency of the predator (high A = poor hunter)
y <- x/(1+A*x)
ggplot()+
  geom_line(mapping=aes(x=x,y=x/(1+A*x)),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed")

# if predator is a bad hunter, prey reach carrying capacity much quicker as their not being killed too often

## Type 2 Functional response LV model

LV_FR2 <- function(t,state,parameters){ ##lotka voltera function function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dx <- alpha * x - (beta * x* y)/(1+A*x) ## prey population equation 
    dy <- (delta * x * y)/(1+A*x) - gamma * y ## predator population
    return(list(c(dx, dy))) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}

# Add A to parameters
state <- c(x=10, y = 10) ## the initial population values of prey and predator populations
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.02, gamma = 0.4, A = 0.02) ## the equation parameters
times <-  seq(from = 1, to = 500, by = 0.1) ##a sequence of time steps – uses function seq()
out <- ode(y= state, times = times, func = LV_FR2, parms = parameters)
out.df <- data.frame(out)

ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

# plot the change in populations through phase space 
# plot one predator population as a function of the prey population
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,270) +
  ylim(0,150) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")


### Three-species competition LV model: Limiting similarity




