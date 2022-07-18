
## --------------------- Clear-all ---------------------
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console


## --------------------- Upload libraries ---------------------
library(bnlearn)
library(data.table) ## Here, dataframes are datatables
library(corrplot)
library(ggplot2)
library(viridis)


## --------------------- Load functions ---------------------
source("Functions.R")


## --------------------- Read data ---------------------
## Load dataframe

## Select relevant variables from data

## Keep just rows with complete data

## Normalize variables

## --------------------- Build correlation matrix ---------------------


## --------------------- Build network ---------------------
## Make whitelist (list of must links)


## Make blacklist (list of links to avoid)
#Black_list <- as.data.table(matrix(c()))

## Obtain initializing network with mmhc algorithm

## Build network with hc algorithm
## Note: perturb is an integer, the number of attempts to randomly 
## ...insert/remove/reverse an arc on every random restart.
BN.hc <- BN.initial.mmhc

## Calculate Log-likelihood Loss of network

## Plot network
#graphviz.plot(BN.hc)
#bnlearn::plot(BN.hc)

## Calculate strength of links (the more negative, the more strength)

## Plot strength-network
strength.plot(BN.hc,BN.hc.str,shape = "ellipse",layout = "dot")

## --------------------- Find parameters of Bayesian Network ---------------------
## Fit the parameters of a Bayesian network (conditional probabilities of the variables)

## --------------------- Probability distributions ---------------------
## Define a variable (Root variable) for which I have a known distribution (that scientists can change)
Root_variable <- "TOTAL"

## Get distribution of Root_variable (Root_variable.prob is a data.table) from the data and assign it in the column probs.old

## Create a new distribution 
Root_variable.prob$probs <- Root_variable.prob$probs.old[c(seq(nrow(Root_variable.prob)-2,nrow(Root_variable.prob)),seq(1,nrow(Root_variable.prob)-3))]

## Variables for which distributions will be visualized
Variables_to_visualize <- c

## Calculate the old and new distributions of the variables to visualize
## Note: the new distributions are the result of the forward or backward causality of the root variable
Variables_to_visualize.prob <- propagate(Student_data,BN.hc.parameters,Variables_to_visualize,Root_variable.prob)


## --------------------- Visualizations ---------------------
## Visualize probability distributions
png("Output/Probability_distributions_old_and_new.png",width = 1200, height = 800, units = "px", res = 120)
par(mfrow=c(2,length(Variables_to_visualize)+1))
# First row old probs
barplot(names.arg = as.data.frame(Root_variable.prob)[,Root_variable],height = Root_variable.prob$probs.old, ylim = c(0,1),main = Root_variable)
for (i in names(Variables_to_visualize.prob)){
  barplot(names.arg = as.data.frame(Variables_to_visualize.prob[[i]])[,1],height = Variables_to_visualize.prob[[i]]$probs.old, ylim = c(0,1),main = i)
}
# Second row new probs
barplot(names.arg = as.data.frame(Root_variable.prob)[,Root_variable],height = Root_variable.prob$probs, ylim = c(0,1),main = Root_variable)
for (i in names(Variables_to_visualize.prob)){
  barplot(names.arg = as.data.frame(Variables_to_visualize.prob[[i]])[,1],height = Variables_to_visualize.prob[[i]]$probs, ylim = c(0,1),main = i)
}
dev.off()

## Visualize differences in probability distributions
png("Output/Differences_in_probability_distributions.png",width = 1200, height = 800, units = "px", res = 120)
par(mfrow=c(1,length(Variables_to_visualize)+1))
barplot(names.arg = as.data.frame(Root_variable.prob)[,Root_variable],height = Root_variable.prob$probs-Root_variable.prob$probs.old, ylim = c(-0.5,0.5),main = Root_variable)
for (i in names(Variables_to_visualize.prob)){
  barplot(names.arg = as.data.frame(Variables_to_visualize.prob[[i]])[,1],height = Variables_to_visualize.prob[[i]]$probs-Variables_to_visualize.prob[[i]]$probs.old, ylim = c(-0.5,0.5),main = i)
}
dev.off()

