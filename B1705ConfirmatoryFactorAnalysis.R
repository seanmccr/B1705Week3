# ----- B1705 Week 3 | Confirmatory Factor Analysis (CFA) | 24.01.2024 -----

# ----- 1. Confirmatory Factor Analysis (CFA) -----

##### 1.1. Loading Libraries #####
# library recommended for CFA
library(lavaan)

##### 1.2. Specifying our model #####
# This represents our findings in the EFA; Factor '1' indicates which variables load on this factor.
model <- '
  F1 =~ x4 + x5 + x6
  F2 =~ x1 + x2 + x3
  F3 =~ x7 + x8 + x9
'
##### 1.3. Fitting the model #####
# Code to run the process on the model that we just created
fit <- cfa(model, data=efa_data)

##### 1.4. Visualisation and inspection of the model #####
# Loading libraries
library(semPlot)
# Code to create a path diagram of this model
semPaths(fit, whatLabels = "est", layout = "tree", edge.label.cex = 2.5, 
         nCharNodes = 0, nCharEdges = 0, sizeMan = 10, sizeLat = 15)

##### 1.5. Assessing model fit #####
# Summarize the results
summary(fit, fit.measures=TRUE)

###### COMPLETE #####


