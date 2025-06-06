## Epidemic Cards Tutorial: computer exercise
## E2M2: Ecological and Epidemiological Modeling in Madagascar
## November 27-December 1, 2016

## Cara Brook, 2016

## By now, you've been exposed to some of the variety of model
## types used to understand infectious disease data. We'll now work
## with a few of those forms to explain our own data.

## By the end of this tutorial, you should be able to:
##
##  * Make a simple SIR model in discrete time.
##  * Visualize data from a time series of Infected and Susceptible case counts
##  * Fit a simple SIR model (i.e. estimate the transmission parameter) with 
##    logistic regression techniques.

library(plyr)

rm(list=ls())
######################################################################
## Part 1: Visualizing your data

## Earlier, you played an epidemic card game and generated several time
## series of infecteds and susceptibles. Let's take a look at these data.

## Import .csv file 
dat <-  read.csv("Epidemic_Cards.csv", header=T)

## (1) Take a look at the form of the dataset


## (2) Select a subset of these data, choosing just those with R0 value = 2 and name that object "dat.R0"


## (3) Next,  define the length of our time series. Make a vector titled "time" that is the length of 
## the longest timestep in your dataset.



## (4) Now, define a variable called "N" that equals your population size (Angelo's pile)


## (5) Using your data, plot Infecteds over time, with a different line for each trial.
## Color all the Infecteds lines as red.


## (6) Now plot all the Susceptible trials as individual lines.
## Color them as green.


## (7) Now add a legend to the plot in the top right-hand corner of your plotting frame.


## (8) How do things change when R0 = 3? Go back above, and take a new subset of the
## data with R0 = 3, and repeat!

######################################################################
##Part 2: Modeling the epidemic

## Now let's build a model to recapture these data!

## We learned this week that models can take a variety of
## forms. The primary axes of model differentiation are: (1) discrete vs. 
## continuous treatment of time and (2) deterministic vs. stochastic model
## forms. Here, we model our epidemic from Epidemic Cards the game in discrete time.

## The version of the model we'll work with here has 3 state variables (susceptible,
## infected, and recovered), but since our population size is constant, we only 
## need to keep track of two of these, since we could always subtract to get the third.
## This is reason why we only kept track of susceptibles and infecteds
## in our game while counting cards.

## Our state variables are given as:

## S - the number of susceptibles in the population
## I - the number of infected/infectious individuals in the population

## and we have parameter, the basic reproduction number (R0). As we have already learned,
## R0 for a pathogen gives the expected number of new infectious individuals engendered by
## one infectious individual in a completely susceptible population.


## Recall that cards are placed in the 'infection pile' and stay there for exactly 1 round.
## In disease modeling, this assumption (that infection lasts for only 1 timestep) is the equivalent
## of saying that R0 = beta, your transmission rate.

## We would thus represent a simple discrete time model in the following form:

## S[t+1] = S[t] - (R0 * S[t]/N)*I[t]
## I[t+1]= (R0 * S[t]/N)*I[t]

## where S[t] indicates the population susceptible at time t and S[t+1]
## and  I[t] indicates the population infectious at time t and I[t+1].

## Now we have all the information we need to make our discrete time model.

## (9) First, let's print our time vector from above to make sure we still have all the 
## important information at hand. Do this here:


## (10) Then, we make two empty vectors called  model.I and model.S for the full duration of the
## time series. (Hint, make two vectors called 'model.I' and 'model.S' that are filled with the value
## 'NA' but are the same length as your vector 'time')



## (11) Now,  "seed" these vectors with our initial conditions of the numbers infected and susceptible.
## (Hint, write over index 1 of model.I and model.S with with one infected individual 25 susceptible individuals.



## (12) Now make an object called "R0" that specifies a value for R0. Use the same value of R0 from the first two rounds
## of the game.


## (13) Now, write a for-loop that iterates your discrete time model across the full length of the time series
## Essentially, write the R language that says the following:

##       for (all variables, t, in the length of our time vector){
##              run my discrete time model (hint, use the equations from above, lines 88 and 89)
##             }


## (14) Now plot the infected output from your discrete time model in #13 and color it red.

## (15) And add a line to the graph with the susceptible output from your discrete time model in #13 and
## color it green. Add a legend to the plot in the top right-hand corner.

## You can see that the Susceptible population declines across the course of 
## the epidemic. What would happen if births were added to this population?


## How do things change when R0 = 3? 
## Go back above, change R0 = 3, and repeat!

######################################################################
##Part 3: How likely are we to recover the observed data, assuming
## our model is true?

## We've now visualized our data and modeled our data, but how well does the 
## model recapture the data? Let's look. First re-run your model and select
## the data subset above, for which R0=2. Label that dataset as  "dat.R0"

## Then, let's plot our model and data together.
## (16) First plot your model lines for infected and susceptible, as you did above under #14 above (line 123).

## (17) Then, add your trials of infected time series to the plot as dashed red lines (hint: lty=2)

## (18) Next, do the same for susceptibles as dashed green lines.


## (19) And, add a legend to the plot in the top righ-hand corner.


## We see that there is variation from trial-to-trial in our data and that,
## sometimes, our model fits the data better than other times. Why might this
## be? Did we get the same time series of S and I every time we played a new trial?
## What does this tell us about the stochastic vs. deterministic nature of 
## the real world? 

## Usually we would not be so lucky as to know our model parameters (in this case, there is just
## one: R0) ahead of time. So let's fit a model with flexible parameters to our
## data and see how well it does.

######################################################################
##Part 4: Optimize parameters to 'fit' your model to your data

## Now, let's assume that we had only our data and wanted to produce a model that 
## 'fit' these data well without knowing R0 in advance. There are many methods used to
## fit models to data, and while this diversity can at first be daunting - like many
## things, this becomes easier with experience. We've already talked a lot about different
## model fitting techniques, and here, we'll just demonstrate one simple method that will just 
## minimize the sum of squared errors.


## (20) First, wrap your discrete time model above into a function called "discrete.mod"
## with the following 4 input arguments: R0, I.start, S.start, time. Have it "return" a time series of infecteds.

discrete.mod <- function(R0, S.start, I.start, time){
  
  model.S <- rep(NA, length(time))
  model.I <- rep(NA, length(time))
  model.S[1] <- S.start
  model.I[1] <- I.start
  
  N = S.start + I.start
  
  for (t in 1:(length(time)-1)){
    model.S[t+1] = model.S[t] - (R0 * model.S[t]/N) * model.I[t]
    model.I[t+1] = (R0 * model.S[t]/N) * model.I[t]
  }
  

  
  return(model.I)
}


## (21) Test that your function is working by running the following line of script:
#out <- discrete.mod(R0=2, S.start = 25, I.start = 1, time=seq(1,10,1))
  
## (22) Now, write a function called "get.diffs" that calculates the difference for each timestep between
## the model output in 21 and one run of the data.

## Here it is:
get.diffs <- function(real.dat1,mod.dat){
  diff <- real.dat1$infecteds - mod.dat
  return(diff)
}


## (23) Write a function called "sum_sq" that calculates the sum of squared difference between your model
## and the data across all trials.



## Here it is:
sum_sq <- function(par, real.dat, time){
  #run your function at the guess par
  
  out.test <- discrete.mod(R0=par, S.start=1, I.start=26, time=time)
  #divide real dat by trials
  trial.list <- dlply(real.dat, .(trial))
  
  #and apply your difference function to get observed - modeled
  trial.list.diffs <- lapply(trial.list,get.diffs, mod.dat=out.test)
  
  #take the sum of these squared differences and return
  sum.of.sqs = sum(unlist(c(trial.list.diffs))^2)
  
  return(sum.of.sqs)
  
  
    
  
    
    
    

  
  for (i in 1:length(unique(real.dat$trial))){
    
  }
}

## (24) Use optim() to minimize the difference between your model and the data. Guess R0 as a value of 2.

out.optim <- optim(par = 2, fn=sum_sq, real.dat=dat.R0, time=time)

## (25) Save out.optim$value as R0.fit


## (25) Run your model with the new value R0.fit.

## (26) Plot and compare with the data.


## How well did we do at recapturing our data, as compared with our 'true model', plotted above?


## What does this tell us about the model fit?

## Try again, this time fitting the subset of the data for which R0 = 3. How well do you do at fitting the
## correct value here? Is it closer or not as close to your true R0 value as when you fit the subset for 
## R0 = 2? Why might this be?






