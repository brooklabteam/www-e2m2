# one other package!
install.packages("RPostgreSQL")

# load the packages
library(ENMeval)
library(predicts)
library(terra)
library(RPostgreSQL)
library(dplyr)

# Load in raster data in the "Predictors" subfolder
rainfall <- rast("rainfall_madagascar.tif")  
pop.dens <- rast("pop_dens_madagascar.tif")  
elevation <- rast("elevation_madagascar.tif") 

# Lets plot each raster
plot(rainfall)
plot(pop.dens)
plot(elevation)

# Load in the spatial data from malaria_coords.csv
# These data are malaria occurrence data use in Arambepola et al. 2020 (in the e2m2_Spatial folder)
# They are the locations of health facilities with clinical cases of malaria confirmed by an RDT
# Gathered from HMIS data reports from between January 2013 and December 2016
disease.data <- read.csv("malaria_coords.csv")

# malaria_coords.csv is an comma separated value table, usually opened in excel, let's look at it
# it only includes x and y coordinates
head(disease.data)

# We need to combine the presence (occurrence) and background data into one dataframe
# First, lets remove duplicate disease locations
disease.data <- disease.data[duplicated(disease.data)==FALSE,]

# Now we convert disease.data into spatial file using the x and y coordinates
disease.data.sp <- terra::vect(disease.data, geom=c("x", "y"), keepgeom=TRUE, crs = crs(rainfall))
plot(disease.data.sp)

# Now we extract the values from our raster predictors at each point in our malaria dataset
disease.data.ext <- terra::extract(rainfall, disease.data.sp, method='simple', fun = mean, xy = FALSE, bind = TRUE)
disease.data.ext <- terra::extract(pop.dens, disease.data.ext, method='simple', fun = mean, xy = FALSE, bind = TRUE)
disease.data.ext <- terra::extract(elevation, disease.data.ext, method='simple', fun = mean, xy = FALSE, bind = TRUE)

# Plot the rainfall data and add the malaria data on top
plot(rainfall)
points(disease.data.ext, col = "yellow")

## What patterns do you notice?
## What type of spatial analysis is this? 
## Exploratory Data Analysis, Spatial Statistics, or Spatial Modeling?


# We need to group our raster layers together but first we need to check that they have the same
# Coordinate reference system (crs), extent (ext), and resolution (res), as each other
# We are using the "pop.dens" object as a template because it has the largest resolution (cell size)
crs(pop.dens) == crs(rainfall)
crs(pop.dens) == crs(elevation)

ext(pop.dens) == ext(rainfall)
ext(pop.dens) == ext(elevation)

res(pop.dens) == res(rainfall)
res(pop.dens) == res(elevation)


# The resolutions of our layers are different, which means we cannot group them together
# Try to run this code and see what happens
predictors <- c(rainfall, pop.dens, elevation)

# Let's resample them so they are all the same
# Here we are using the pop.dens raster as a template for the resampled rainfall and elevation rasters.
rainfall <- terra::resample(rainfall, pop.dens, method = "average")
elevation <- terra::resample(elevation, pop.dens, method = "average")

# Check to make sure the resampling worked, all of these should now be TRUE
res(pop.dens) == res(rainfall)
res(pop.dens) == res(elevation)

# Lets group the rasters together
# Now the rasters are aligned, we can rerun this line of code with no errors
predictors <- c(rainfall, pop.dens, elevation)

# and plot them
plot(predictors)


## In spatial analysis, we often use a random locations in the study area to act as our null hypothesis
## In habitat analysis this is the "Use vs Availability" approach but since we will go on to model the distribution
## of malaria, this is a "presence/background" approach

# Lets get our sample of random background points
# Use spatSample to get 5,000 random points
background.pts <- terra::spatSample(predictors, 5000, xy = TRUE, na.rm = TRUE)

# Now we need to convert our disease data back into a dataframe
# This new dataframe still includes the values we extracted from our predictor rasters
occor <- data.frame(disease.data.ext)
head(occor)

# Compare the names of the occor and background.pts dataframes
# The names must match before we can combine our two dataframes
names(occor)
names(background.pts)

# Add a column called "malaria" to denote whether this is a presence or absence point
# Where 1 = malaria and 0 = background
occor$malaria <- 1
background.pts$malaria <- 0

# Combine the malaria occurrences with the background points into one dataframe using a row bind (rbind)
data.all <- rbind(occor, background.pts)

# Remove rows in the dataframe with no data using complete.cases
data.all <- data.all[complete.cases(data.all), ]

### basic stats
## We can integrate spatial data into standard statistical methods
# Lets plot malaria presence/background by mean yearly rainfall
boxplot(data.all$rainfall ~ data.all$malaria, xlab = "", ylab = "Mean Annual Rainfall",
        col = c("light blue", "light green"), names = c("Malaria Background", "Malaria Occurrences"))

# And run a t-test to see if there is a difference in the mean yearly 
# rainfall between the malaria and background groups.
t.test(data.all$rainfall ~ data.all$malaria)

# Is there a significant difference?
# Does malalaria occor in areas with higher or lower rainfall than the background data?


# What about a correlation?
# Is elevation correlated with rainfall?
# plot the correlation of rainfall and elevation
plot(data.all$rainfall ~ data.all$elevation, xlab = "Elevation", ylab = "Mean Annual Rainfall")

# and test for statistical significance
cor.test(data.all$rainfall, data.all$elevation)

## What is the relationship between elevation and rainfall?
## Is it statistically significant?

## What type of spatial analysis is this? 
## Exploratory Data Analysis, Spatial Statistics, or Spatial Modeling?

## We can also use the extracted spatial data in a linear mode
glm1 <- glm(malaria ~ rainfall + elevation + pop.dens, data = data.all, family=binomial(link=logit))
glm1

### But liner models work best with presence and true absence data
### We do not have absence data, only background
### So lets use a spatial model that is designed for presence/background data: MaxEnt, which is also called MaxNet

## We will make three MaxEnt models, each made with different combinations of predictor rasters

# For model 1, we use all three predictors: rainfall, pop.dens, elevation
# What might be a hypothesis behind these predictors
predictors1 <- c(rainfall, pop.dens, elevation)


# For model 2, we only use rainfall and elevation
# What might be a hypothesis behind these predictors?
predictors2 <- c(rainfall, elevation)


### We are using the ENMevaluate function in the ENMeval package
## For each model we specify the occurrence data (occs = disease.data.sp)
## The predictors we are testing, these change with each model (predictors1, predictors2, etc.)

## tune.args = list(fc = c("L", "LQH"), rm = 1), partitions='randomkfold', n.bg = 5000, algorithm = "maxnet",  overlap = FALSE, doClamp = TRUE)
## These are all model type and fit data, we will not be changing them today

## If you would like to learn more about MaxEnt, read Elith et al. 2011 (in the "Papers" folder)
## And look Here: https://jamiemkass.github.io/ENMeval
## And here: https://rsh249.github.io/bioinformatics/ENMeval.html#:~:text=fc%20.,”%2C%20“LQH”).
## For R tutorials


# Each model can take 3-5 minutes to run
# I have changed some recommended settings to save time fitting models (yes this is the quick version)
# if you run these models using your own data, change the code to match the recommendations below.
maxnet1 <- ENMevaluate(occs = disease.data.sp, predictors1, # Here we use the predictors1 object
                        tune.args = list(
                          fc = c("L", "LQH"), ## recommended settings: c(“L”, “LQ”, “LQH”), see Elith et al. 2011 for more information
                          rm = 1), ## recommended setting: seq(0.5, 2, 0.5), we have just put 1 to save time
                        partitions='randomkfold',
                        n.bg = 5000, ## recommended setting: 10000
                        algorithm = "maxnet",  overlap = FALSE, doClamp = TRUE)

# Look at the model object
maxnet1

# You can access different parts of the model object using @
maxnet1@results
maxnet1@models

# See that there are actually two models and two entries in the results?
# We actually fit two models, one using just linear (L) and the other using liner, quadratic, and hinge (LQH)

# Now lets fit a model using the predictors2 (just rainfall and elevation)
maxnet2 <- ENMevaluate(occs = disease.data.sp, predictors2, # Here we use the predictors2 object
                       tune.args = list(
                         fc = c("L", "LQH"), ## recommended settings: c(“L”, “LQ”, “LQH”), see Elith et al. 2011 for more information
                         rm = 1), ## recommended setting: seq(0.5, 2, 0.5), we have just put 1 to save time
                       partitions='randomkfold',
                       n.bg = 5000, ## recommended setting: 10000
                       algorithm = "maxnet",  overlap = FALSE, doClamp = TRUE)

# And look at the outputs
maxnet2
maxnet2@results
maxnet2@models


## But which of the models best reflects the data?
## We can use AICc model selection to find out
## We will talk about AIC and model selection later in the course
## For now, know that the smaller the AICc the better the model fits the data

# Make a table to put our values AICc and model fit values into
models.aicc <- data.frame(names = c("m1", "m2"))

# and save the model results into their own models
maxnet1.results <- maxnet1@results
maxnet2.results <- maxnet2@results

# Add a column with the name of the maxnet model
maxnet1.results$model <- "maxnet1"
maxnet2.results$model <- "maxnet2"

# Combine the results into one dataframe
maxnet.results <- rbind(maxnet1.results, maxnet2.results)

# and select the most important columns for model fit statistics
maxnet.results <- maxnet.results %>% select(model, fc, rm, tune.args, ncoef, auc.train, AICc)
  
# Lets calculate the difference in AICc for each model (deltaAICc)
# We subtract each model's AICc from the smallest AICc of any model
maxnet.results$deltaAICc <- maxnet.results$AICc - min(maxnet.results$AICc)

# Let's look at the table and sort it by the lowist AICc
sort_by(maxnet.results, ~AICc)
## Which is our top model?


## We often use Area Under the Curve (AUC) to see how well our model predicts the data
## The closer the AUC is to 1 the better it fits the data
# an AUC of 0.5 means the model does no better than random

## What is the Area Under the Curve (AUC) for our top model?
## Using the Area Under the Curve (AUC), does this model predict malaria presence well?


## Lets look at the response curves for each variable in our top model
## In MaxEnt (and MaxNet) respose curves tell us the relationship between the predictor and the
## Since our top model was linear, quadratic, and hinge, our curves can be complex

## We create an object of the top model
mod <- maxnet1@models$fc.LQH_rm.1 ## this is the name of the top model in our table

## Lets see how malaria presence responds to rainfall
respcurve<-maxnet::response.plot(mod, "rainfall", type="cloglog",
                                 mm=mod$samplemeans, min=mod$varmin["rainfall"], 
                                 max=mod$varmax["rainfall"],
                                 levels=unlist(mod$levels["rainfall"]), plot=T,
                                 xlab = "Rainfall (mm)",
                                 ylab = "Malaria presence (0-1)")
# How would you incorporate this figure?

# Now lets look at the population density
respcurve<-maxnet::response.plot(mod, "pop.dens", type="cloglog",
                                 mm=mod$samplemeans, min=mod$varmin["pop.dens"], 
                                 max=mod$varmax["pop.dens"],
                                 levels=unlist(mod$levels["pop.dens"]),plot=T,
                                 xlab = "Population density (km^2)",
                                 ylab = "Malaria presence (0-1)")

# and look at the elevation
respcurve<-maxnet::response.plot(mod, "elevation", type="cloglog",
                                 mm=mod$samplemeans, min=mod$varmin["elevation"], 
                                 max=mod$varmax["elevation"],
                                 levels=unlist(mod$levels["elevation"]),plot=T,
                                 xlab = "Elevation (m)",
                                 ylab = "Malaria presence (0-1)")




## Let's make a map predicting whether malaria is present in an area
## We have scaled all of the values to be between 0-1, where 1 is most likly and 0 is least
## Save the prediction of the top model to a new object
top.model.pred <- maxnet1@predictions$fc.LQH_rm.1 #Again we need to specify the name of our top model here
plot(top.model.pred, col = map.pal("viridis", 100))

# And add the malaria locations we used to make the model on top
points(disease.data.sp)


# Let's save our predictions file so we can look at it again later
writeRaster(top.model.pred,"malaria_model.tif")

## Do the model's predictions make sense?
## What other predictors of malaria would you add to the model?
## What type of spatial analysis is this? 
## Exploratory Data Analysis, Spatial Statistics, or Spatial Modeling?

## Interested in more malaria models?
## Malaria Atlas has spatial models and data you can explore and download
## https://data.malariaatlas.org/maps


## We do not have time today to run point pattern, clustering, or habitat analyses but here are some links to R tutorials
## Point Pattern Analysis: https://paezha.github.io/spatial-analysis-r/point-pattern-analysis-i.html 
##                         https://rpubs.com/hughes/295880
##                         https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html
##                         https://www3.nd.edu/~mhaenggi/ee87021/Dixon-K-Function.pdf
##
## Habitat use analysis: https://cran.r-project.org/web/packages/adehabitatHS/adehabitatHS.pdf
##                       https://bookdown.org/c_w_beirne/wildCo-Data-Analysis/habitat-use.html
##                       https://www.youtube.com/watch?v=dsPsRPZiOC0






