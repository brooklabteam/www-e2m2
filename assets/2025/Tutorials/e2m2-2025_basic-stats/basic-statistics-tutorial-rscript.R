# Basic Statistics Workshop
# Projet: E2M2 2025
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated March 2024

# Introduction ############################

#' This is a tutorial that was developed as part of the E2M2 workshop held in 
#' March 2024. It is based on code developed by Michelle Evans. 
#' This tutorial introduces several basic statistical methods used to assess 
#' the relationship between variables and compare values between groups.

#' By the end of the tutorial, you should be able to:
  
#' - Visualize your data and determine if it is parametric or non-parametric
#' - Conduct a correlation analysis
#' - Conduct two-sample t-tests
#' - Conduct an ANOVA analysis

# Load the packages for this tutorial #########################

#' Here are the packages we'll need for this tutorial. I also choose to set 
#' some base configurations that I like, such as never reading in strings as 
#' factors, setting a limit for the number of digits to include when printing 
#' to the console, and setting a base theme for `ggplot`.

#' Remember that you may need to install packages that you don't already 
#' have installed on your computer using `install.packages("package-name")`. 
#' There is an example commented out below.

options(stringsAsFactors = F, scipen = 999, digits = 4)

#to find files within the Rproject
#install.packages("here")
library(here)

#visualization and exploration
library(ggplot2); theme_set(theme_bw())
library(skimr)

#statistics
library(car)

#data manipulation
library(tidyr)
library(dplyr)

# Load the Data ###################################

penguins <- read.csv(here("data/penguins.csv"))

flowers <- read.csv(here("data/flowers.csv"))

# Data Exploration ##############################

## Penguins #################################

#' First, let's look at the datasets structure and variables. 
#' We can do that using `head` in base R and `skim` from the `skimr` package.

head(penguins)
skimr::skim(penguins)

#' During this class, we are going to try and explore if there is a 
#' difference in penguin body size between penguin species and island.
#'  We can make a boxplot of this to start understanding what our data 
#'  looks like.

ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot()

#' We also want to know what the distribution of this data is, so that we can 
#' start to get an idea of if it is parametric or non-parametric. To do that,
#'  we use a **histogram**. 

ggplot(penguins, aes(x= body_mass_g, fill = species,  group = species)) +
  geom_histogram() +
  facet_wrap(~species, nrow = 3)

#' We also will later look at the correlation between different measures of 
#' body size. Let's look at the relationship between two of them now 
#' with a **scatterplot**.

ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point()

# Verifying Model Assumptions ###############################

#' For parametric tests, there are four model assumptions that must be met:
  
#'  - **Linearity** : The relation between two variables is linear

#' - **Independence** : Each observation is independent of all other observations

#' - **Normality**: The distribution of the data must be normal

#' - **Homogeneity of variance**: The variance of subsets or groups of data should be equal

## Linearity ########################################

#' Linearity is most easily visualized via a scatter plot. Let's compare 
#' `Sepal.Length` and `Petal.Length` in the `flowers` dataset in a scatter 
#' plot using `geom_point`.

#this relationship isn't very linear
ggplot(data = flowers, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()

#this is a better example of a linear relationship
ggplot(data = flowers, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

## Independence #######################################

#'Independence is determined by the study-design, and there 
#'isn't a statistical test to do for it. 


## Normality ##########################################

#' Normality refers to the distribution of the data. Normal data resembles 
#' a bell-curve, which is symmetrical on both sides.

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 40)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

#this data is right-skewed, however it seems to be normal within each group of species
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram() +
  facet_wrap(~species)

#' The Shapiro-Wilk test is used to test for normality. 
#' This is a hypothesis test. The null hypothesis is that the data is normally 
#' distributed, and the alternative hypothesis is that the data is not normally
#'  distributed. Because the p-value is less than 0.05, we have enough 
#'  evidence to infer that the `body_mass_g` variable is not normally distributed. 
shapiro.test(penguins$body_mass_g)
shapiro.test(penguins$body_mass_g[penguins$species=="Chinstrap"])

## Homegeneity of Variance ###############################
#' Homogeniety of variance means the variance of each group is the same.

#' We can visualize this using a boxplot or violin plot.

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot(aes(fill = species))

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = species)) 

#' We can test for homogeneity of variance using Levene's Test

car::leveneTest(body_mass_g ~ species, data = penguins)

#' It is returning the results of a null hypothesis test. The null hypothesis 
#' is that all variances are equal across groups. The alternative hypothesis 
#' that we are testing is that the variances are not equal across groups. 
#' Because the p-value (`Pr(>F)`) is below 0.05, we would infer that the 
#' assumption of homogeneity of variance is violated (i.e. that the variances 
#' are different across the groups `species`).

# Condcuting Correlations ##########################################
#' Correlations are used to compare the strength of an association between 
#' two continuous variables. They range between -1 (perfectly negatively 
#' correlated) and +1 (perfectly positively correlated).

#' Before applying a test of correlation, we should first visualize the data 
#' using a scatterplot. One variable is plotted on the x-axis and the other 
#' on the y-axis.

ggplot(data = flowers, aes(x = Sepal.Length, y = Petal.Width)) +
  geom_point()
#' This isn't a purely linear relationship, and may be better assessed via a 
#' Spearman's correlation.

ggplot(data = flowers, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() 

#' This relationship looks more linear, and could be assessed via a Pearson's 
#' correlation. However, we must also verify that the data is normally 
#' distributed. We do this individually by variable.

shapiro.test(flowers$Sepal.Length)
shapiro.test(flowers$Sepal.Width)

## Pearson's Correlation #########################################

cor.test(flowers$Sepal.Length, flowers$Sepal.Width, method = "pearson")

#' This is a null hypothesis test, where the null hypothesis is that the 
#' true correlation is equal to zero and the alternative hypothesis is that 
#' the true correlation is not equal to zero. We assess the strength of the
#'  correlation via the `cor` parameter and the statistical significance of 
#'  the correlation via the `p-value`.

#' Because the `cor` value is positive and relatively high in the range of 
#' -1 to +1, we can infer that the association between these variables is 
#' positive and moderately strong. The p-value being low further supports 
#' this and suggests that the association is statistically significantly 
#' different from no association.

## Spearman's Correlation ######################################
#' In the data visualization above, Sepal Length and Petal Width seemed 
#' to have a non-linear relationship. They would be better assessed via a 
#' Spearman's correlation, called rho.

cor.test(flowers$Sepal.Length, flowers$Sepal.Width, method = "spearman")

# Comparisons Between Groups ####################################
#' There are several statistical tests for comparing continuous variables 
#' (`numeric`, `integer`) between groups (`factors`). Which one to use depends
#'  on if the data is parameteric or non-parametric and on how many groups 
#'  you have.

## Comparison between two groups ###############################

### Parametric #################################################

#' If your data is parametric (normally distributed, independent, 
#' equal variance), you can use a t-test to compare a variable between 
#' groups. Both types of t-tests for two groups (two-sample and Welch's) 
#' use the `t.test` function, but differ in their assumption of equal variance 
#' via the `var.equal` argument. Use `?t.test` to access the help file for 
#' `t.test` to see what values this argument takes. 

 ?t.test

#' We can apply this to test for differences in `bill_length_mm` across 
#' `sex` in the `penguins` data. This variable is normally distributed and 
#' has equal variance among groups.

ggplot(penguins, aes(x = sex, y = bill_length_mm)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_boxplot(fill = NA)

t.test(bill_length_mm ~ sex, data = penguins, var.equal = TRUE)

#' This test returns information on the alternative hypothesis that the 
#' mean bill length differs between penguin sex. It provides the mean of 
#' each group (male and female), as well as the test statistics (t, df, 
#' p-value). All of these would be reported in a manuscript.


### Non-Parametric ################################

#' If your data is not normally distributed, you would use a Mann Whitney U 
#' (or Wilcoxon rank-sum Test), to compare measurements between two groups. 
#' This is done via the `wilcox.test` function.

ggplot(data = penguins, 
       aes(x = island, y = bill_length_mm)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_boxplot(fill = NA)

ggplot(data = penguins, aes(x = bill_length_mm)) +
  geom_histogram() +
  facet_wrap(~island)

wilcox.test(bill_length_mm~island, data = penguins,
            subset = island %in% c("Biscoe", "Dream"))

#' As we may have predicted from the boxplot, the variable `bill_length_mm` 
#' does not differ between the two islands.

#calculate the median and IQR for each group
penguins |>
  group_by(island) |>
  summarise(median = median(bill_length_mm),
            #estimate IQR range too
            low_IQR = quantile(bill_length_mm, 0.25),
            upp_IQR = quantile(bill_length_mm, 0.75))

## Comparison between more than 2 groups ################

### Parametric ##########################################

#' An ANOVA (Analysis of Variance) Test is used to compare the means between 
#' 3 or more groups if the data is parametric. It uses the function `aov`. 
#' The test is provided using the formula format with a tilde (~). Use 
#' `?aov` to see how to apply an ANOVA to explore if `body_mass_g` differs 
#' across `species` in the penguin dataset. The output of `aov` is not very 
#' informative, so it is better to save as an object and use `summary`.

anova_test <- aov(body_mass_g~species, data = penguins)

summary(anova_test)

#' In this output, we are particularly concerned with a couple of values. 
#' `Df` is the degrees of freedom, equal to the number of groups (k) - 1. 
#' `Pr(>F)` is the p-value, representing the probability that the difference 
#' among groups is due to chance.

#' To get pairwise differences,  we need to perform a *post-hoc* or *pairwise* 
#' comparison. There are multiple types of these tests, but we will use a 
#' Bonferroni for now. Read more about the other options
#'  [here](https://www.statology.org/tukey-vs-bonferroni-vs-scheffe/).

pairwise.t.test(penguins$body_mass_g, penguins$species, 
                p.adjust.method = "bonferroni")

#' Based on this, we can see that the difference between Adelie and Chinstrap
#'  penguins is not significant (p-value = 1), but the differences between 
#'  Gentoo and the other two species have very low p-values, suggesting those 
#'  differences are statistically significant. These pairwise differences are 
#'  often illustrated on a plot using letters to denote groups with 
#'  significant differences, like below:

ggplot(penguins, aes(x = species, y = body_mass_g, color = species)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  geom_boxplot(fill = NA) +
  guides(color = "none") +
  geom_text(data = data.frame(label = c("a","a", "b"),
                              x = levels(factor(penguins$species)),
                              y = c(5000,5000,6450)), 
            aes(x = x, y = y, label = label), color = "black")

### Non-Parametric ###########################################

#' ANOVA tests are more robust than t-tests to data with unequal variance, 
#' but still require your data to be normally distributed. If your data is 
#' not normally distributed, you can use a Kruskal-Wallis test to compare 
#' the rank of values between groups (similar to the Spearman's correlation).
#' In our dataset, the variable `flipper_length_mm` seems to not be normally 
#' distributed, especially on Biscoe island. 

ggplot(penguins, aes(x  = flipper_length_mm)) +
  geom_histogram()+
  facet_wrap(~island, nrow = 3)

kruskal.test(flipper_length_mm ~ island, data = penguins)

#'This result confirms that the flipper lengths are different across the 
#'islands, but like the ANOVA, does not tell us which islands have 
#'significantly different flipper lengths. We will need to conduct a post-hoc test.

pairwise.wilcox.test(penguins$flipper_length_mm, penguins$island,
                     p.adjust.method = "bonferroni")















