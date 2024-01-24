# ----- B1705 Week 3 | Factor Analysis | 24.01.2024 -----

# ----- 1. Exploratory Factor Analysis -----

##### 1.1. Loading libraries #####
library(psych)

##### 1.2. Loading in our dataset and checking headings #####
efa_data <- read.csv('https://www.dropbox.com/scl/fi/e5qri07n3ng2jl3nxl47j/efa_data.csv?rlkey=qm6xyngabaz1iibxzuew57y0k&dl=1')
head(efa_data,6) # display the first six rows

##### 1.3. Code to create a scree plot; this determines the number of factors #####
fa.parallel(efa_data, fa="both")
# Run the parallel analysis without the default legend
fa.parallel(efa_data, fa="both", main="Parallel Analysis Scree Plot", show.legend=FALSE)

##### 1.4. Code to run the EFA #####
efa_result <- fa(efa_data, nfactors=3, rotate="varimax")

##### 1.5. Code to print and interpreting results #####
print(efa_result, cut=0.3)
# Code to provide a diagram of our results
fa.diagram(efa_result)

##### 1.6. What to do with the EFA? #####

# Show the factor loadings for existing variables
print(efa_result$loadings)
# Calculate factor scores
factor_scores <- efa_result[["scores"]]

##### 1.7. Integrating factor loadings into the dataset #####
# Combine original data with factor scores; these factor scores can be used instead of our original variables
# for subsequent analysis
combined_efa_data <- cbind(efa_data, factor_scores)
# Print combined EFA headings
head(combined_efa_data)

# ----- 2. Exploratory Factor Analysis (EFA): Practice -----

                            