ku.enm.eval <- function(path, occ.all, occ.tra, occ.test, batch, out.eval, omi.val = 5, 
                        rand.perc = 50, no.inter = 1000, kept = TRUE, selection = "OR_AICc")
  
  #Evaluation is an important step in model calibration. This step centers on selecting candidate 
  #models and their associated parameters to identify the very best models for the purposes of the study. 
  #The ku.enm.eval function evaluates candidate models based on three distinct criteria: statistical significance 
  #(based on partial ROC), prediction ability (we use omission rates, but other metrics, such as overall correct 
  #classification rate, can also be used), and complexity (here evaluated using AICc). 

  #Arguments explanation for ku.enm.eval:
  
  #- path is the directory in wich the folders containig calibration models are being or were created 
  #- occ.all (character) is the name of the csv file with the calibration occurrences, columns must be: species, longitud, latitud
  #- occ.tra (character) is the name of the csv file with the calibration occurrences, columns must be: species, longitud, latitud
  #- occ.test (character) is the name of the csv file with the evaluation occurrences, columns must be: species, longitud, latitud
  #- batch (character) the name of the .bat file created with the KU.ENM.cal function
  #- out.eval (ccaracter) name of the folder in wich the results of the evaluation will be written
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