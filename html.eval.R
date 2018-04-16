html.eval <- function(path, file.name) {
  
  if(!require(rmarkdown)){
    install.packages("rmarkdown")
    library(rmarkdown)
  }
  
  sink(paste(path, paste(file.name, ".Rmd", sep = ""), sep = "/"))
  cat(
"---
title: \"ku.enm: evaluation results\"
output:
  html_document:
      toc: true
      toc_depth: 4
---
      
\```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
\```

<br>
      
### Breif description of the models evaluation and selection process
      
\```{r, echo=FALSE}
st4 <- read.csv(\"evaluation_results.csv\")
sett <- as.character(st4[,1])
setts <- strsplit(sett, split = \"_\")
rm <- vector()
for (i in 1:length(setts)) {
rm[i] <- setts[[i]][2]
}
f.clas <- vector()
for (i in 1:length(setts)) {
f.clas[i] <- setts[[i]][4]
}
var.di <- vector()
for (i in 1:length(setts)) {
var.di[i] <- paste(setts[[i]][5:length(setts[[i]])], collapse = \"_\")
}
rm1 <- paste(unique(rm), collapse = \", \")
f.clas1 <- paste(unique(f.clas), collapse = \", \")
var.di1 <- paste(unique(var.di), collapse = \", \")
par <- rbind(rm1, f.clas1, var.di1)
\```
      
This is the final report of the ku.enm.eval function implemented in the ku.enm R package. 

A total of \`r length(st4[,1])\` calibration models with parameters resulted from the combination of \`r length(unique(rm))\` regularization multipliers, \`r length(unique(f.clas))\` feature classes, and \`r length(unique(var.di))\` distinct sets of environmental variables, have been evaluated. Models peformance was evaluated based on statistical significance (Partial_ROC), omission rates, and the Akaike information criterion corrected for small sample sizes (AICc).
      
\```{r par, echo=FALSE}
colnames(par) <- \"Parameters\"
row.names(par) <- c(\"Regularization multipliers\", \"Feature classes\", \"Sets of predictors\")
knitr::kable(par, digits=c(0,0), row.names = TRUE, caption = \"Table 1. Parameters of the evaluated models.\")
\```
      
<br>
All the results presented below can be found in the evaluation output folder for further analyses.

<br>
<br>
      
### Models evaluation statistics

In the following table you are going to find information about how many models meet the four criteria of selection that this function uses.
      
\```{r, echo=FALSE}
st <- read.csv(\"evaluation_stats.csv\")
colnames(st) <- c(\"Criteria\",	\"Number_of_models\")
knitr::kable(st, digits=c(0,0), caption = \"Table 2. General statistics of models that meet distinct criteria.\")
\```
      
<br>
<br>

### Best models according to pre-defined criteria
      
The following table contains the best models selected by your pre-defined criteria.

Notice that if the selection criterion was, models with the best Omission rates and among them those with lower AICc values, the delta_AICc values were recalculated among the new candidate models.

\```{r, echo=FALSE}
best <- list.files(pattern = \"best\")
st1 <- read.csv(best)
colnames(st1) <- c(\"Model\",	\"Mean_AUC_ratio\",	\"Partial_ROC\", \"Ommission_rate_5%\", \"AICc\",	\"delta_AICc\",	\"W_AICc\",	\"num_parameters\")
knitr::kable(st1, digits=c(0,3,3,3,3,3,3,0), caption = \"Table 3. Performance statistics of the best models selected based on the pre-defined critera.\")
\```

<br>
<br>

### Models preformance plot

The figure below shows the position of your selected models in the distribution of all the calibration models according to omission rates and AICc values.

![Figure 1. Distribution of all, non-statistically significant, and selected models according to their omission rates and AICc values.](evaluation_figure.png){width=60%}

<br>
<br>

### Performance evaluation statistics for all models

Following you will find the performance statistics for all the calibration models.

\```{r, echo=FALSE}
st4 <- read.csv(\"evaluation_results.csv\")
colnames(st4) <-  c(\"Model\",	\"Mean_AUC_ratio\",	\"Partial_ROC\", \"Ommission_rate_5%\", \"AICc\",	\"delta_AICc\",	\"W_AICc\",	\"num_parameters\")
knitr::kable(st4, digits=c(0,3,3,3,3,3,3,0), caption = \"Table 4. Performance statistics of all the calibration models.\")
\```"
      )
  sink()
  render(paste(path, paste(file.name, ".Rmd", sep = ""), sep = "/"), "html_document", quiet = TRUE)
  unlink(paste(path, paste(file.name, ".Rmd", sep = ""), sep = "/"))
  
}
  
  
  