#Evaluation is an important step in model calibration. This step centers on selecting candidate 
#models and their associated parameters to identify the very best models for the purposes of the study. 
#The ku.enm.eval function evaluates candidate models based on three distinct criteria: statistical significance 
#(based on partial ROC), prediction ability (we use omission rates, but other metrics, such as overall correct 
#classification rate, can also be used), and complexity (here evaluated using AICc). 

#Arguments explanation for ku.enm.eval:
#***DAN: these are very nice, detailed, and precise specs on inputs! Well done.
#- path is the directory in wich the folders containig calibration models are being or were created 
#- occ.all (character) is the name of the csv file with the calibration occurrences, columns must be: species, longitud, latitud
#- occ.tra (character) is the name of the csv file with the calibration occurrences, columns must be: species, longitud, latitud
#- occ.test (character) is the name of the csv file with the evaluation occurrences, columns must be: species, longitud, latitud
#- batch (character) the name of the .bat file created with the ku.enm.cal function
#- out.eval (character) name of the folder in wich the results of the evaluation will be written
#- omi.val (numeric) is the % of omission error allowed (5%)
#- rand.perc (numeric) is the percentage of data to be used for the bootstraping process, default (50%) 
#- no.inter (numeric) is the number of times that the bootstrap is going to be recalculated, default (100)
#- kept (logical) if TRUE all calibration models will be kept after evaluation, default TRUE
#- selection (character) model selection criterion, can be "OR_AICc", "AICc", or "OR"; OR = omission rates

#Outputs 
#A folder containing the following:
#A csv file notifying the amount of models meeting the different criteria of selection
#A csv file containing all the statistics of model performance (pROC, AICc, and Omission rates) 
#Another csv file with only the best models selected based on the chosen criterion
#A png file containing a scatterplot od all models based on the AICc values and Omission rates
#An html file sumarizing all the information produced for helping with interporetations

#Returns
#Notifications of diferent stages of the model evaluation process, and other notifications when the process is finished

ku.enm.eval <- function(path, occ.all, occ.tra, occ.test, batch, out.eval, omi.val = 5, 
                        rand.perc = 50, no.inter = 1000, kept = TRUE, selection = "OR_AICc"){
  
  #####
  #Packages
  if(!require(yaml)){
    install.packages("yaml")
    library(yaml)
  }
  if(!require(ENMeval)){
    install.packages("ENMeval")
    library(ENMeval)
  }
  if(!require(sqldf)){
    install.packages("sqldf")
    library(sqldf)
  }
  if(!require(rmarkdown)){
    install.packages("rmarkdown")
    library(rmarkdown)
  }
  if(!require(knitr)){
    install.packages("knitr")
    library(knitr)
  }
  
  rasterOptions(maxmemory = 1e+15) #giving more power to the raster package
  options(digits = 16) #number of digits that r can recognize
  
  #####
  #Functions
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/n.par.R")
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/proc.R")
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/om_rate.R")
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/html.eval.R")
  
  #####
  #Data
  ##Source of initial information for model evaluation order
  bat <- readLines(paste(batch, ".bat", sep = "")) #reading the batch file written to create the calibration models
  
  ###Recognizing the folders names and separating them for different procedures
  fol <- gregexpr("outputd.\\S*", bat)
  fold <- regmatches(bat, fol)
  folde <- unlist(fold)
  extract <- paste("outputdirectory=", path, "\\", sep = "")
  folder <- gsub(extract, "", folde, fixed = T) #names of all the calibration models folders
  
  folder_a <- gregexpr("\\S*all", folder)
  folder_al <- regmatches(folder, folder_a)
  folder_all <- unlist(folder_al) #folders with the models for calculating AICcs
  
  folder_c <- gregexpr("\\S*cal", folder)
  folder_ca <- regmatches(folder, folder_c)
  folder_cal <- unlist(folder_ca) #folder with the models for calculating pROCs and omission rates
  
  ##Models
  ###For AICc
  dir_names <- as.vector(paste(getwd(), "/", path, "/", folder_all, sep = "")) #vector of the subdirectories with the models
  
  ###For pROC and omission rates
  dir_names1 <- as.vector(paste(getwd(), "/", path, "/", folder_cal, sep = "")) #vector of the subdirectories with the models
  
  ###Names of the models to be evaluated
  mod_nam <- as.vector(gsub("_all", "", folder_all, fixed = TRUE)) #names of the models (taken from the folders names)
  
  ##Complete set and calibration and evaluation occurrences
  oc <- read.csv(occ.all) #read all occurrences
  oc <- oc[,-1] #erase species name column
  
  occ <- read.csv(occ.tra) #read calibration occurrences
  occ <- occ[,-1] #erase species name column
  
  occ1 <- read.csv(occ.test) #read test occurrences
  occ1 <- occ1[,-1] #erase species name column
  
  ##Omission value
  omissionval <- omi.val / 100
  
  ###Place of the occ.tra with the value to be considered the omi.val
  val <- ceiling(length(occ[,1]) * omi.val / 100) + 1 
  
  #####
  #pROCs, omission rates, and AICcs calculation
  cat("\nPartial ROCs, omission rates, and AICcs calculation, please wait...\n")
  
  aiccs <- list() #empty list of AICc results
  proc_res_med <- list() #empty list of mean AUC values
  proc_res_lt1 <- vector() #empty vector of significance values of the AUCs
  om_rates <- vector() #empty vector of omision rates 
  
  pb <- winProgressBar(title = "Progress bar", min = 0, max = length(dir_names), width = 300) #progress bar
  
  for(i in 1:length(dir_names)){ #calculating AUC rations in a loop
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, title = paste( round(i / length(dir_names) * 100, 2), "% of the evaluation process has finished"))
    
    #AICc calculation
    suppressWarnings(while (!file.exists(as.vector(list.files(dir_names[i], pattern = ".lambdas", full.names = TRUE)))) {
      Sys.sleep(1)
    })
    lbds <- as.vector(list.files(dir_names[i], pattern = ".lambdas", full.names = TRUE)) #lambdas file
    lambdas <- readLines(lbds)
    par_num <- n.par(lambdas) #getting the number of parameters for each model
    
    suppressWarnings(while (!file.exists(list.files(dir_names[i], pattern = "asc", full.names = TRUE))) {
      Sys.sleep(1)
    })
    mods <- list.files(dir_names[i], pattern = "asc", full.names = TRUE) #name of ascii model
    mod <- raster(mods) #reading each ascii model created with the complete set of occurrences
    aiccs[[i]] <- suppressWarnings(calc.aicc(nparam = par_num, occ = oc, predictive.maps = mod)) #calculating AICc for each model
    
    #pROCs calculation
    suppressWarnings(while (!file.exists(list.files(dir_names1[i], pattern = "asc", full.names = TRUE))) {
      Sys.sleep(1)
    })
    mods1 <- list.files(dir_names1[i], pattern = "asc", full.names = TRUE) #list of the ascii models
    mod1 <- raster(mods1) #reading each ascii model created with the calibration occurrences
    
    if(min(na.omit(getValues(mod1))) == max(na.omit(getValues(mod1)))){
      warning("\nMaxent model produced an output with an only probability value, pROC and 
              omission rate will return a value of 2 for recognition of these cases.\n")
      proc_res_med[[i]] <- data.frame(2, 2, 2, 2) #mean of AUC ratios interations for each model
      proc_res_lt1[i] <- 2
      om_rates[i] <- 2  
    }else{
      proc_res <- ku.enm.proc(presencefile = occ.test, predictionfile = mod1,
                              omissionval = omissionval, randompercent = rand.perc, 
                              noofiteration = no.inter) #Partial ROC analyses for each model
      proc_res <- as.data.frame(do.call(rbind, proc_res)) #converting each list of AUC ratios interations in a table for each model
      proc_res_med[[i]] <- apply(proc_res, 2, mean) #mean of AUC ratios interations for each model
      proc_res_lt1[i] <- sum(proc_res[,4] <= 1)/length(proc_res[,4]) #proportion of AUC ratios <= 1 for each model  
      
      #Omission rates calculation
      om_rates[i] <- ku.enm.omrat(model = mod1, threshold = omi.val, occ.tra = occ, occ.test = occ1)
    }
    
    #Erasing calibration models after evaluating them if kept = FALSE
    if(kept == FALSE){
      unlink(dir_names[i], recursive = T)
      unlink(dir_names1[i], recursive = T)
    }
  }
  suppressMessages(close(pb))
  n.mod <- i
  
  ##Erasing main folder of calibration models if kept = FALSE
  if(kept == FALSE){
    unlink(path, recursive = T)
    cat("\nAll calibration models were deleted\n")
  }else{
    cat("\nAll calibration models were kept\n")
  }
  
  ##Creating the final tables
  ###From AICc analyses
  aiccs <- do.call(rbind, aiccs) #joining tables
  for (i in 1:length(aiccs[,1])) {
    aiccs[i,2] <- (aiccs[i,1] - min(aiccs[,1], na.rm = TRUE))
    aiccs[i,3] <- (exp(-0.5 * aiccs[i,2])) / (sum(exp(-0.5 * aiccs[,2]), na.rm = TRUE))
  }
  
  ###From pROC analyses
  proc_res_m <- do.call(rbind, proc_res_med)[,-(2:3)] #joining tables of the mean AUC ratios
  proc_res_m[,1] <- mod_nam #declaring model names
  proc_res_m <- data.frame(proc_res_m, proc_res_lt1) #adding a new column with the number of AUC ratios interations < 1
  
  ###Omission rates
  om_rates <- unlist(om_rates)
  
  #####
  #Joining the results
  ku_enm_eval <- as.data.frame(cbind(proc_res_m, om_rates, aiccs))
  ku_enm_eval <- data.frame(ku_enm_eval[,1], as.numeric(levels(ku_enm_eval[,2]))[ku_enm_eval[,2]], ku_enm_eval[,3], 
                            ku_enm_eval[,4], ku_enm_eval[,5], ku_enm_eval[,6], ku_enm_eval[,7], ku_enm_eval[,8])
  colnames(ku_enm_eval) <- c("Model", "Mean_AUC_ratio", "Partial_ROC",#changing column names in the final table
                             paste("Omission_rate_at_", omi.val, "%", sep = ""), "AICc",
                             "delta_AICc", "W_AICc", "num_parameters")
  
  
  #####
  #Choosing the best models
  if(selection == "OR_AICc" | selection == "AICc" | selection == "OR"){
    if(selection == "OR_AICc"){
      ku_enm_bes <- ku_enm_eval[ku_enm_eval[,3] <= 0.05,]
      ku_enm_best <- ku_enm_bes[ku_enm_bes[,4] <= omissionval,]
      if(length(ku_enm_best[,4]) != 0){
        for (i in 1:length(ku_enm_best[,1])) {
          ku_enm_best[i,6] <- (ku_enm_best[i,5] - min(ku_enm_best[,5], na.rm = TRUE))
          ku_enm_best[i,7] <- (exp(-0.5 * ku_enm_best[i,6])) / (sum(exp(-0.5 * ku_enm_best[,6]), na.rm = TRUE))
        }
        ku_enm_best <- na.omit(ku_enm_best[ku_enm_best[,6] <= 2,])
        if(length(ku_enm_best[,6]) > 10){
          ku_enm_best <- ku_enm_best[order(ku_enm_best[,6]),][1:10,] 
        }else{
          ku_enm_best <- ku_enm_best[order(ku_enm_best[,6]),]
        }
      }else{
        mesKU <- "\nNone of your models meets the omission rate criterion,\n
        models with the smallest omission rates will be presented\n"
        ku_enm_best <- ku_enm_bes[order(ku_enm_bes[,4]),][1:100,]
        for (i in 1:length(ku_enm_best[,1])) {
          ku_enm_best[i,6] <- (ku_enm_best[i,5] - min(ku_enm_best[,5], na.rm = TRUE))
          ku_enm_best[i,7] <- (exp(-0.5 * ku_enm_best[i,6])) / (sum(exp(-0.5 * ku_enm_best[i,6]), na.rm = TRUE))
        }
        ku_enm_best <- na.omit(ku_enm_best[ku_enm_best[,6] <= 2,])
        if(length(ku_enm_best[,6]) > 10){
          ku_enm_best <- ku_enm_best[order(ku_enm_best[,6]),][1:10,] 
        }else{
          ku_enm_best <- ku_enm_best[order(ku_enm_best[,6]),]
        }
      }
    }
    
    if(selection == "AICc"){
      ku_enm_best1 <- ku_enm_eval[ku_enm_eval[,3] <= 0.05,]
      ku_enm_best1 <- na.omit(ku_enm_best1[ku_enm_best1[,6] <= 2,])
      if(length(ku_enm_best1[,6]) > 10){
        ku_enm_best1 <- ku_enm_best1[order(ku_enm_best1[,6]),][1:10,] 
      }else{
        ku_enm_best1 <- ku_enm_best1[order(ku_enm_best1[,6]),]
      }
    }
    
    if(selection == "OR"){
      ku_enm_bes <- ku_enm_eval[ku_enm_eval[,3] <= 0.05,]
      ku_enm_best2 <- ku_enm_bes[ku_enm_bes[,4] <= omissionval,]
      if(length(ku_enm_best2[,4]) != 0){
        if(length(ku_enm_best2[,4]) > 10){
          ku_enm_best2 <- ku_enm_best2[order(ku_enm_best2[,4]),][1:10,] 
        }else{
          ku_enm_best2 <- ku_enm_best2[order(ku_enm_best2[,4]),]
        } 
      }else{
        mesKU <- "\nNone of your models meets the omission rates criterion,\n
        models with the smallest omission rates will be presented\n"
        ku_enm_best2 <- ku_enm_bes[order(ku_enm_bes[,4]),][1:10,]
      }
    }
    }else{
      cat("\nNo valid model selection criterion has been defined,\n
          no file containing the best models will be created.\n
          Select your best models from the complete list.\n")
    }
  
  #####
  #Statistics of the process
  ##Counting
  ku_enm_best_OR_AICc <- ku_enm_bes[ku_enm_bes[,4] <= omissionval,]
  if(length(ku_enm_best_OR_AICc[,4]) != 0){
    for (i in 1:length(ku_enm_best_OR_AICc[,1])) {
      ku_enm_best_OR_AICc[i,6] <- (ku_enm_best_OR_AICc[i,5] - min(ku_enm_best_OR_AICc[,5], na.rm = TRUE))
      ku_enm_best_OR_AICc[i,7] <- (exp(-0.5 * ku_enm_best_OR_AICc[i,6])) / (sum(exp(-0.5 * ku_enm_best_OR_AICc[,6]), na.rm = TRUE))
    }
    ku_enm_best_OR_AICc <- na.omit(ku_enm_best_OR_AICc[ku_enm_best_OR_AICc[,6] <= 2,])
  }
  
  ku_enm_best_AICc <- na.omit(ku_enm_bes[ku_enm_bes[,6] <= 2,])
  
  ku_enm_best_OR <- ku_enm_bes[ku_enm_bes[,4] <= omissionval,]
  
  ##Preparing the table
  r_names <- c("Statistically significant models", "Models meeting OR criteria", 
               "Models meeting AICc critera", "Models meeting OR and AICc criteria")
  statis <- c(length(ku_enm_bes[,3]),
              length(ku_enm_best_OR[,4]),
              length(ku_enm_best_AICc[,6]),
              length(ku_enm_best_OR_AICc[,2]))
  ku_enm_stats <- cbind(r_names, statis)
  colnames(ku_enm_stats) <- c("Criteria", "Number of models")
  
  #####
  #Writing the results
  ##csv files
  cat("\nWriting ku.enm.eval results...\n")
  dnam <- out.eval
  dir.create(dnam)
  
  name <- paste(dnam, "evaluation_results.csv", sep = "/")
  name0 <- paste(dnam, "evaluation_stats.csv", sep = "/")
  name1 <- paste(dnam, "best_models_OR_AICc.csv", sep = "/")
  name2 <- paste(dnam, "best_models_AICc.csv", sep = "/")
  name3 <- paste(dnam, "best_models_OR.csv", sep = "/")
  
  
  write.csv(ku_enm_eval, file = name, eol = "\n", na = "NA", row.names = FALSE)
  write.csv(ku_enm_stats, file = name0, eol = "\n", na = "NA", row.names = FALSE)
  
  if(selection == "OR_AICc" | selection == "AICc" | selection == "OR"){
    if(selection == "OR_AICc"){
      write.csv(ku_enm_best, file = name1, eol = "\n", na = "NA", row.names = FALSE)
    }
    if(selection == "AICc"){
      write.csv(ku_enm_best1, file = name2, eol = "\n", na = "NA", row.names = FALSE)
    }
    if(selection == "OR"){
      write.csv(ku_enm_best2, file = name3, eol = "\n", na = "NA", row.names = FALSE)
    }
  }
  
  ##Plot
  png(paste(dnam, "evaluation_figure.png", sep = "/"), width = 80, height = 80, units = "mm", res = 600)
  par(mar = c(4.5, 4, 0.5, 0.5), cex = 0.5)
  plot(na.omit(ku_enm_eval[,4])~log(na.omit(ku_enm_eval[,5])),
       xlab = "Natural logarithm of AICc", ylab = paste("Omission rates at", 
                                                        paste(omi.val, "%", sep = ""), "threshold value", sep = " "),
       las = 1, col = "gray35")
  points(na.omit(ku_enm_eval[!ku_enm_eval[,1] %in% ku_enm_bes[,1],])[,4]~log(na.omit(ku_enm_eval[!ku_enm_eval[,1] %in% ku_enm_bes[,1],])[,5]),
         col = "red1", pch = 19, cex = 1.2)
  
  if(selection == "OR_AICc" | selection == "AICc" | selection == "OR"){
    if(selection == "OR_AICc"){
      points(na.omit(ku_enm_best[,4])~log(na.omit(ku_enm_best[,5])),
             col = "dodgerblue1", pch = 19, cex = 1.5)
      legend("bottomright", legend = c("Selected models", "Non significant models", "All models"),
             pt.cex = c(1.8, 1.5, 1), pch = c(19, 19, 1), col = c("dodgerblue1", "red1", "gray35"), bty = "n",
             inset = c(0.01, 0))
    }
    if(selection == "AICc"){
      points(na.omit(ku_enm_best1[,4])~log(na.omit(ku_enm_best1[,5])),
             col = "darkorchid1", pch = 19, cex = 1.5)
      legend("bottomright", legend = c("Selected models", "Non significant models", "All models"), 
             pt.cex = c(1.8, 1.5, 1), pch = c(19, 19, 1), col = c("darkorchid1", "red1", "gray35"), bty = "n",
             inset = c(0.01, 0))
    }
    if(selection == "OR"){
      points(na.omit(ku_enm_best2[,4])~log(na.omit(ku_enm_best2[,5])),
             col = "orange2", pch = 19, cex = 1.5)
      legend("bottomright", legend = c("Selected models", "Non significant models", "All models"),
             pt.cex = c(1.8, 1.5, 1), pch = c(19, 19, 1), col = c("orange2", "red1", "gray35"), bty = "n",
             inset = c(0.01, 0))
    }
  }
  dev.off()
  
  ##html file
  ###Writing the html file
  html.eval(path = dnam, file.name = "evaluation_results")
  
  #####
  #Finalizing the function
  cat("\nProcess finished\n")
  cat(paste("A folder containing the results of the evaluation of", n.mod,
            "\ncalibration models has been written\n", sep = " "))
  
  cat(paste("\nThe folder", dnam, "contains:\n", sep = " "))
  cat("   -A html file and its dependencies that sum all the results, check\n")
  cat(paste("    ", "evaluation_results.html\n", sep = ""))
  
  cat("   -Two csv files with the results and stats of the evaluation,\n")
  
  if(selection == "OR_AICc" | selection == "AICc" | selection == "OR"){
    if(selection == "OR_AICc"){
      cat("    and an aditional csv file containing the best models selected by OR and AICc.\n")
    }
    if(selection == "AICc"){
      cat("    and an aditional csv file containing the best models selected by AICc.\n")
    }
    if(selection == "OR"){
      cat("    and an aditional csv file containing the best models selected by OR.\n")
    }
  }
  
  if(exists("mesKU") == TRUE){
    warning(mesKU)
  }
  
  cat(paste("\nCheck your working directory!!!", getwd(), sep = "    "))
}