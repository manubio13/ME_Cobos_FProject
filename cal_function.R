ku.enm.cal <- function(occ.all, occ.tra, M.var.dir, batch, out.dir, reg.mult, f.clas = "all", run = TRUE)

  #Calibration models are a large set of candidate models created to respond to the need to test multiple 
  #parameter combinations, for example, distinct regularization multiplier values, various feature classes, 
  #and different sets of environmental variables. 
  
  #Arguments explanation ku.enm.cal:
  
  #- occ.all (character) is the name of the csv file with all the occurrences, columns must be: species, longitud, latitud
  #- occ.cal (character) is the name of the csv file with the calibration occurrences, columns equal to occ.all
  #- M.var.dir (character) is the name of the folder containing other folders with different sets of environmental variables
  #- batch (character) name of the batch file with the code to create all calibration maxent models
  #- out.dir (character) name of the folder which will contain all calibration models subfolders
  #- reg.mult (numeric or numeric vector) regularization multiplier(s) to be evaluated
  #- Feature clases can be selected from  four different combination sets or manually:
    #Combination sets are: "all", "basic", "no.t.h", "no.h", and "no.t". default = "all". basic = "l", "lq", "lqp", "lqpt", "lqpth". 
    #Manually you can select from the following list:
    #"l", "q", "p", "t", "h", "lq", "lp", "lt", "lh", "qp", "qt", "qh",
    #"pt", "ph", "th", "lqp", "lqt", "lqh", "lpt", "lph", "qpt", "qph",
    #"qth", "pth", "lqpt", "lqph", "lqth", "lpth", "lqpth"
  #-run (logical) if true the batch runs after its creation, if false it will only be created and runnig would be manually

  #Outputs 
  #A folder named as out.dir with all the subfolders to save maxent results when running the .bat file
  #A .bat file containing the java codes to run all the maxent calibration models, it will run auotmatically or in some
  #computers it will be asked if running is allowed

  #Returns
  #Notification of allowing runing the java batch file if asked, creation of a batch file in working directory, and
  #how many models will be created