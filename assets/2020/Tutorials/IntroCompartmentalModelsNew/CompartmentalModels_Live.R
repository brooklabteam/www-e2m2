#######################################
## Introduction 
#######################################
## Compartmental models and differential equation R tutorial
## Live coding version - fill in as you go
## E2M2 - 2020
## written by: Jessica Metcalf, Amy Wesolowski, Cara Brook

## In this tutorial, we will construct simple compartmental models and explore different structures of these models
##############################################################################
## Contents: 
# Modeling Population Growth
# Investigating continuous vs discrete time
# Structured population models
# Lotka-Volterra models of predator-prey dynamics
# Susceptible-Infected-Recovered models
##############################################################################
## load your libraries first

library(deSolve)

##############################################################################
## Modeling Population Growth
##############################################################################

## The simplest formulation for population growth in discrete time is N_t+1=lambda N_t
## where N_t is the size of the population at time t. The World Bank provides time-series 
## of population size for all countries in the world, including for Madagascar; this is in 
## the file "WorldBankPop.csv". Keep in mind that this is not data from direct observations 
## - censuses have not occurred in all the years for which data is available! 
  
## First, bring in this data by identifying the right path to the folder you are working in (use the command 'getwd' to find out where you are, and then use 'setwd' to get R to look in the right place) and have a look at its structure in R (just the first 10 columns): 

##Load your data from the world bank and peek at the first few rows



## Make a sub-vector called 'mada.pop' that is the row of your dataset which 
## corresponds to Madagascar.


## Make a second sub-vector called 'france.pop' that corresponds to France.


## Now, we can calculate the growth rate, N_t+1/N_t, by taking a vector that goes from the
## 2nd until the last value in our population vectors (this is years 1961 until 2016, and 
## thus we call it N_t+1). We will divide this by the the vector that goes from the first 
## until the before-last column, which will correspond to 1960 to 2015. Relative to the 
## first vector, this is N_t - i.e., the first number in the N_t+1 vector is 1961; and the 
## first number in the N_t vector is 1960, and so on.  


## First make a vector of Nt and Nt+1 for Mada

## And do the same for France

## Now estimate population growth rate for Madagascar and France as lambda = Nt+1/Nt

## Is the population growing or declining?

## Now plot (a) the population sizes for Madagascar and France from 1960-2016,
## side-by-side with the (b) population growth rates




## The growth rates for Madagascar and France never drop below 1 (although France comes close).
## What does this tell us?



##############################################################################
## Investigating continuous vs discrete time
##############################################################################

## Is the model above structured like a continuous or a discrete time model?

## Let's make a discrete time population model. We will use a for-loop to project a 
## population 100 time-steps, starting with population size of 10, and with a population 
## growth rate equal to 1.1

## Set up your (a) vector "N" of future population sizes, (b) fill in your initial population size,
## and (c) define "lambda", the population growth rate.



## Now make a discrete time model using a for-loop to iterate this population 100 years
## into the future.


## Does anyone remember the equation to shorten this discrete time model? 
## If we do this, do we get the same result?



## Plot the results together.


## What if we want to make a continuous time model? How dowe write that?
## (Hint: First, write the differential equation dN(t)/dt = rN in a function called 'ngrow')



## To solve this, we're going to need to use the R library called "deSolve".
## We also need to define the starting variables and parameters: 

library(deSolve)              # Load libary to be used for numerical integration
Pop <- c(N=10)                # Define starting variables (P(0)=10, as above)
values <- c(r=log(lambda))    # Define parameters - use an approx of the lambda used in the discrete time ex above
time.out <- seq(0,100,by=0.1) # Set up time-steps for variables - here go for 100 years
ngrow(t=0,y=Pop,parms=values)  #try out the function for t=0

## The function outputs the time derivative of N at the time t. 
# This means that in order to get the (approximate) values of N at some time 
# delta.t in the future, we need to calculate the equivalent of N(t+delta.t)=N(t)+rN(t)*delta.t. 
# We can do this by hand: 

delta.t <- 0.1                    # Set a small value for delta.t (0.1 day)
pop.next <- Pop + unlist(ngrow(t=time,y=Pop,parms=values)) * delta.t
pop.next

## We could iterate this process, updating our values of Pop 
## and time with each iteration to get a discrete-time approximation 
## of the values of $P$ through time. However, differential equations describe 
## the rates of change. It turns out that using the discrete time approximation 
## of this process leads to rapid acummulation of error in our estimate of the state 
## variables through time, even if we set our value of delta.t to be very small. 
## Fortunately,  a number of algorithms have been developed to come up with better 
## (both more accurate and more computationally efficient) approximations to the values of the 
## state variables through time. One such is "lsoda" which we loaded above, and which we can run, 
## here storing the result in a variable called 'Ncontinuous': 


Ncontinuous <- lsoda(
y = Pop,               # Initial conditions for population
times = time.out,      # Timepoints for evaluation
func = ngrow,          # Function to evaluate
parms = values         # Vector of parameters
)

## We can check the first few lines of the output:
head(Ncontinuous)

## The data frame has 2 columns showing the values of time (labeled "time") 
## and the state variable ("N", which is population size) through time. Let's see what 
## has happened after 2 years, by printing it out:
subset(Ncontinuous,time==2)

## Now we can also plot the full time-series: 
plot(Ncontinuous[,"time"],               # Time on the x axis
     Ncontinuous[,"N"],                  # Number people on the y axis
     xlab = "Time in days",     # Label the x axis
     ylab = "Pop size",         # Label the y axis
     main = " ",                # Plot title
     xlim = c(0,100),           # 
     type = "l",                # Use a line plot
     bty = "n")                 # Remove the box around the plot
points(1:100,N, pch=19,col="red",cex=0.2)  #compare with our discrete time

# Note that we overlaid our discrete time model predictions (the variable N) for comparison.
## This is close, but not an exact match to our discrete time version, which makes sense, 
## since these are different processes.  
                           
##############################################################################
## Structured population models 
##############################################################################

## So far, we have ignored structure in the population, and treated all individuals as if they were identical. We know this is not true. The simplest extension is to assume some form of stage structure, i.e., different states within the population. Let us assume that the population has two stage classes, 'juveniles' and 'adults'. We can describe the population using a vector of numbers of the two types of individuals n_t=(n_j,n_a) where the j index indicates juveniles, and the a index indicates adults, n indicates 'numbers'. In discrete time, projection from one time-step to another requires defining juvenile survival, juvenile probability of aging into the adult class in each time-step, adult survival and adult fertility.

sj=0.2  #juvenile survival
aj=0.8  #juvenile aging 
sa=0.8  #adult survival
f=1     #adult fertility

## These parameters can be estimated by tracking individuals in the field and
## counting the number that survive between censuses (here, distinguishing 'juvenile' 
## and 'adult' stages), what number moved between stages, and the number of offspring 
## produced at each stage. From this, we obtain survival and aging probabilities, and 
## fertility. We place these parameters in a matrix which has as many rows and columns as
## we have stages. The columns define the stage that individuals start in, and the rows the 
## stage that individuals end up in: 

A <- matrix(0,2,2)
A[1,1] <- sj*(1-aj)  #juvenile to juvenile = not aging and surving as a juvenile
A[2,1] <- sj*aj      #juvenile to adult =  aging and surving as a juvenile
A[2,2] <- sa         #adult to adult =  surviving as an adult
A[1,2] <- f          #adult to juvenile = survivng and reproducing  

## Take a look at the matrix 'A' in your R console:
A

# We can project the population forward by one time-step:  
n.start <- c(5,10)    #start with pop of 5 juveniles and 10 adults
n.next <- A%*%n.start #get pop one time-step latter using matrix multiplication (%*%)
## And look at it in your R console
n.next                #more juveniles, but fewer adults...

## We could write a loop, and project the population into the future (by applying matrix multipicaton to A and n.next, and so on), and from that, figure out the growth rate of this population. There is  also a shortcut, using some core results from matrix population model theory (Caswell, 2001): the population growth rate is defined by the dominant eigenvalue of the matrix. We can extract this with: 
eigen(A)$value[1]      #extract the pop growth rate using the eigenvalue; <1 indicates pop shrinking. this eigenvalue is equivalent to lambda, the intrinsic population growth rate

A[1,2] <- f*2          #double the fertility 
## See how this changes the matrix A
A                      #fertility 1 is now replaced by fertility 2
eigen(A)$value[1]      #extract the pop growth rate using the eigenvalue; now pop growing with lamba > 1

# There are many more powerful results that yield estimates of the stable population structure, the reproductive value, life expectancy, sensitivies, elasticities, etc, that have been used extensively in fields from conservation biology to life history evolution. You can also implement density dependence, etc. There are some nice R packages that can help you do this (e.g., 'popbio').

## TO DO: 
#### 1) Iterate the population forwards in a loop and compare the growth rate obtained with the eigenvalue estimate of lambda. You may find that the population gets unmanageably large very quickly (R can't handle very large numbers) - so one trick is to reset the popuation to sum to one after each iteration - it doesn't matter, since you're only interested in the change / increase from t to t+1.
#### 2) For some species, e.g., mosquitoes, the juvenile phase (or larval phase) could be sensitive to environmental drivers, like temperature, humidity, etc. Create a loop that changes survival of juveniles (or aging, which equates to development rate) in the matrix at each time-step based on such time-varying drivers (which you could build using a sine wave to capture seasonality, etc - note that this means that the time-step is < 1 year). Explore the consequences for total population size if temperature increases. 
### 3)  You can download a large set of pre-digitised matrix population models from: http://www.compadre-db.org and then explore population growth, life expectancy, etc. for 100s and 100s of species, etc. (see Salgeruo-Gomez et al. 2015)


##############################################################################
## Lotka-Volterra models of predator-prey dynamics
##############################################################################

## Moving beyond considering just one species, a classic dynamic is that of predator-prey cycles. To caricature the process, we can imagine that fossa eat lemurs, until there aren't enough lemurs. Then the fossa population crashes, which allows the lemur population to grow again, etc. We can frame this in continuous time with the following equation:

LotVmod <- function (Time, State, Pars) {
with(as.list(c(State, Pars)), {
dx = x*(alpha - beta*y)
dy = -y*(gamma - delta*x)  #reverse to just echo the line above :)
return(list(c(dx, dy)))
})
}

## where x reflects the prey population (hares, rabbits, lemurs, etc), 
## and y reflects the predator population (wolves, foxes, fossa, etc). 
## The prey population grows at a linear rate (alpha) and gets eaten by the predator 
## population at the rate of (beta) per predator. The predator gains a certain amount 
## vitality by eating the prey at a rate (delta), while dying off at another rate (gamma). 
## Let's set some initial parameter values, and a starting population structure, and 
## time-vector, as previously: 

Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 150, by = 1)

## We can then run the model and plot the results: 
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time)) #run model

## plot results
matplot(out[,1],out[,-1], type = "l",    #separate 1st col (time), plot against other 2
        xlab = "time", ylab = "population", lty=1)  
legend("topright", c("Lemurs", "Fossa"), lty = c(1,1), col = c(1,2), box.lwd = 0)

## We can start with different starting numbers of rabbits and fosa, and overlay the two plots (using the command 'add=TRUE' in matplot). To distinguish the two runs, we use dashed lines for the second set of starting values using lty=2 (colours still represent predator and prey, as in the legend):  

State2 <- c(x = 5, y = 10)
out2 <- as.data.frame(ode(func = LotVmod, y = State2, parms = Pars, times = Time))
matplot(out[,1],out[,-1], type = "l", 
        xlab = "time", ylab = "population", lty=1)  #plot out our previous results
matplot(out2[,1],out2[,-1], type = "l", lty=2,add=TRUE)  #add the new runs
legend("topright", c("Lemurs", "Fossa"), lty = c(1,1), col = c(1,2), box.lwd = 0)

## This shows the same pattern, just slightly lagged in time. 
## After the initial transient (detectable by slightly higher or lower peaks for the 
## first few cycles), the two simulations start repeating the same pattern over and over. 

## We can try the same with different parameters, to understand what the parameters do, 
## again overlaying the new results for comparison. Here, we double the parameter alpha, 
## which determines prey reproduction: 
  
Pars2 <- c(alpha = 4, beta = .5, gamma = .2, delta = .6)
out3 <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars2, times = Time))
matplot(out[,1],out[,-1], type = "l", 
        xlab = "time", ylab = "population", lty=1)  
matplot(out3[,1],out3[,-1], type = "l", lty=2,add=TRUE)  
legend("topright", c("Lemurs", "Fossa"), lty = c(1,1), col = c(1,2), box.lwd = 0)

## The two time-series settle down to different patterns. Interestingly, if more lemurs
## are being produced, you end up with less lemurs in the end, because you are getting 
## more fossa in the system. Sometimes, this is referred to as the 'paradox of enrichment'. 


##############################################################################
## Susceptible-Infected-Recovered models  
##############################################################################

## Let's think about another two species model: dynamics for a directly 
## transmitted immunizing infection, with a short generation time (e.g., ~2 weeks), 
## like measles. We need to keep track of 3 states, or types of individuals, i.e., 
## susceptible individuals ('S'), infected individuals ('I') and recovered individuals ('R').
## We can write the function that defines this process, assuming that the total population 
## size is constant, which means we don't need to keep track of the ('R') compartment. 
## This is:  

sir <- function(t,y,parms){
  with(c(as.list(y),parms),{
    dSdt <- -beta*S*I/N
    dIdt <- beta*S*I/N - gamma*I
    list(c(dSdt,dIdt))
  })
}

## We define a starting population with 500,000 susceptible individuals and 20 infected 
## individuals; and a list of parameters that define the processes, beta, the transmission 
## rate, and gamma, the recovery rate (defined as 1/infectious period): 
  
pop.SI <- c(S = 500000,I = 20)  
values <- c(beta = 3.6,          # Transmission coefficient
            gamma = 1/5,         #recovery rate
            N=800000)            # population size (constant)

## We can calculate the value of the basic reproduction number R_0 = beta / gamma, 
## as follows:
  
R0 <- values["beta"]/values["gamma"] # value of R0
R0                                  # Remember that the infection will spread (grow) 
                                    #  when R0 > 1, just like the population grows when 
                                    # lambda > 1 in the structured population models above.

## As in the population example, we can use the function lsoda, here taking as our time-unit
# 1 day, setting a small time-step, and running it out across 1 year (365 days): 
ts.sir <- data.frame(lsoda(
  y = pop.SI,                   # Initial conditions for population
  times = seq(0,365,by=0.1),   # Timepoints for evaluation
  func = sir,                   # Function to evaluate
  parms = values                # Vector of parameters
))

## Look at the resulting data frame from this run of the model pop.SI
head(ts.sir)

## and then we can plot this: 

plot(ts.sir[,"time"],ts.sir[,"S"], xlab="Time", ylab="Susceptible", type="l", col = "green")
lines(ts.sir[,"time"],ts.sir[,"I"], xlab="Time", ylab="Infected", type="l", col="red")
legend("topright", c("S", "I"), col = c("green", "red"), lty=c(1,1),  box.lwd = 0)

## The epidemic burns itself out (i.e., at the end, the number of infected individuals 
## is zero), but, interestingly, not all susceptible individuals are infected (i.e., the 
## number of susceptibles at the end is > 0). This is the phenomenon of herd immunity. 

## Now we can re-run the analysis for a less infectious pathogen, i.e. beta is lower, but
## with the same population parameters:

lower.values <- c(beta = 0.9,   # Transmission coefficient decreases from 3.6 to .9
            gamma = 1/5,         #recovery rate
            N=800000)            # population size (constant)
lower.ts.sir <- data.frame(lsoda(
  y = pop.SI,                   # Initial conditions for population
  times = seq(0,365,by=0.1),   # Timepoints for evaluation
  func = sir,                   # Function to evaluate
  parms = lower.values                # Vector of parameters
))

## and then we can plot this: 

plot(lower.ts.sir[,"time"],lower.ts.sir[,"S"], xlab="Time", ylab="Susceptible", type="l", col="green")
lines(lower.ts.sir[,"time"],lower.ts.sir[,"I"], xlab="Time", ylab="Infected", type="l", col="red")

## How are these results different? 

## TO DO: 
#### 1) Change the parameter values to see what happens if people recover slower (lower gamma). 
#### 2) Change the number of initial infected individuals to 1 -- will the outbreak start earlier? Later? 
### 3) This model reflects a 'closed' population. There are \textbf{no new births} entering the population. Can you imagine how you might change the model to include this? What are the main dynamical consequences? 
### 4) You can download time-series for a number of infections including measles from https://www.tycho.pitt.edu; you could explore seasonality for different pathogens, or patterns through space and time, and across different city sizes  

## Final notes
# Some of this material is extended from DAIDD and MMED courses. 


## References 

# Cawell, H. Matrix Population Models. 2001. Sinauer

# Salguero-Gomez, R., et al. The COMPADRE Plant Matrix Database: an open online repository for plant demography. Journal of Ecology, 2015. 103(1): p. 202-218.

