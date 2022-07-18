
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
#Student_data <- readRDS("Input/datos.icfes.rds") ## This is a datatable (and therefore a dataframe too!)
Student_data <- fread("Input/datos.icfes.csv") ## This is a datatable (and therefore a dataframe too!)


## Select relevant variables from data
Student_data <- Student_data[,c(
             "PUNTAJE",
             # "BIOLOGIA_PUNT",
             # "MATEMATICAS_PUNT",
             # "FILOSOFIA_PUNT",
             # "FISICA_PUNT",
             # "QUIMICA_PUNT",
             # "LENGUAJE_PUNT",
             # "INTERDISCIPLINAR_PUNT" # NO
             #"ECON_PERSONAS_HOGAR",
             #"FAMI_ING_FMILIAR_MENSUAL",
             # "FAMI_COD_EDUCA_PADRE",
             # "FAMI_COD_EDUCA_MADRE",
             # "FAMI_COD_OCUP_PADRE",
             # "FAMI_COD_OCUP_MADRE",
             "ESTU_ESTRATO",
             # "ECON_MATERIAL_PISOS",
             # "ECON_CUARTOS",
             # "ECON_SN_COMPUTADOR",
             "ESTU_TRABAJA",
             # "CIENCIAS_SOCIALES_PUNT",
             # "INGLES_DESEM",
             # "ECON_SN_CELULAR",
             # "ECON_SN_INTERNET",
             # "ECON_SN_SERVICIO_TV",
             # "ECON_SN_TELEFONIA",
             "TOTAL",
             # "SES",
             "DESERCION_MEDIA",
             "EXTRAEDAD",
             #"APROBACION_MEDIA",
             "REPROBACION_MEDIA",
             "AMBIENTE.ESCOLAR"
             ),
             with = F] ## with = F is for returning a data frame column, not a vector

## Keep just rows with complete data
Student_data <- Student_data[complete.cases(Student_data)]

## Normalize variables
for (i in 1:ncol(Student_data)) { # Run through columns of dataframe
  Student_data[,i] <- Student_data[,i,with=FALSE]/max(Student_data[,i,with=FALSE])
}


## --------------------- Build correlation matrix ---------------------
corrplot(cor(na.omit(Student_data[,.SD,.SDcols = sapply(Student_data, is.numeric)])),order="FPC", col = viridis(100),tl.col="black")


## --------------------- Build network ---------------------
## Make whitelist (list of must links)
White_list <- as.data.table(matrix(c(
               # "TOTAL","PUNTAJE",
               "ESTU_ESTRATO","PUNTAJE"
               # "ECON_PERSONAS_HOGAR","ESTU_ESTRATO"
               # "ECON_PERSONAS_HOGAR","EXTRAEDAD",
               # "ECON_PERSONAS_HOGAR","ESTU_TRABAJA"
               # "AMBIENTE.ESCOLAR","PUNTAJE"
               ), ncol = 2, byrow = T))[,.(from = V1, to = V2)] # 'From' and 'to' are the names of the columns

## Make blacklist (list of links to avoid)
#Black_list <- as.data.table(matrix(c()))

## Obtain initializing network with mmhc algorithm
BN.initial.mmhc <- mmhc(Student_data,whitelist = White_list)



## Build network with hc algorithm
## Note: perturb is an integer, the number of attempts to randomly 
## ...insert/remove/reverse an arc on every random restart.
#BN.hc <- hc(Student_data, start = BN.initial.mmhc, perturb = 1000, whitelist = White_list)
BN.hc <- BN.initial.mmhc

## Calculate Log-likelihood Loss of network
#plot(bn.cv(Student_data,BN.hc,runs = 20))

## Plot network
#graphviz.plot(BN.hc)
#bnlearn::plot(BN.hc)

## Calculate strength of links (the more negative, the more strength)
BN.hc.str <- arc.strength(BN.hc,Student_data)

## Plot strength-network
strength.plot(BN.hc,BN.hc.str,shape = "ellipse",layout = "dot")


## --------------------- Discretize Student_data ---------------------
## Classify variable values of Student_data into new numerical classes (similar to discretization)
## This is a reduction of the discrete sample space of each variable
for (ii in seq(1,length(Student_data))) {
  vect <- Student_data[,ii,with = F][[1]]
  if (length(unique(vect)) > nclass.Sturges(vect)) {
    breaks <- nclass.Sturges(vect)
    r <- range(vect)  
    b <- seq(r[1],r[2],length = 2*breaks+1)  
    brk <- b[0:breaks*2+1]
    mid <- b[1:breaks*2]
    brk[1] <- brk[1]-0.01 # because open left
    k <- cut(vect, breaks=brk, labels=FALSE) 
    vect <- format(mid[k],digits = 2)
  }
  Student_data[[ii]] <- vect
}

## Convert Student_data columns to factors
Student_data <- Student_data[,lapply(.SD,as.factor)]


## --------------------- Find parameters of Bayesian Network ---------------------
## Fit the parameters of a Bayesian network (conditional probabilities of the variables)
BN.hc.parameters <- bn.fit(BN.hc,Student_data,method = "bayes")


## --------------------- Probability distributions ---------------------
## Define a variable (Root variable) for which I have a known distribution (that Policymakers can change)
Root_variable <- "TOTAL"

## Get distribution of Root_variable (Root_variable.prob is a data.table) from the data and assign it in the column probs.old
Root_variable.prob <- prob(Student_data,Root_variable) ## Calculates probabilities of different bins/classes of the root variable, which add to 1
setnames(Root_variable.prob,"probs","probs.old") ## Change column name from 'probs' to 'probs.old'

## Create a new distribution that is the outcome of a new policy
Root_variable.prob$probs <- Root_variable.prob$probs.old[c(seq(nrow(Root_variable.prob)-2,nrow(Root_variable.prob)),seq(1,nrow(Root_variable.prob)-3))]

## Variables for which distributions will be visualized
Variables_to_visualize <- c("PUNTAJE","AMBIENTE.ESCOLAR","ESTU_TRABAJA","ESTU_ESTRATO",
          "REPROBACION_MEDIA","EXTRAEDAD","DESERCION_MEDIA")

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

