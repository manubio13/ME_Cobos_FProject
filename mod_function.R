ku.enm.mod <- function(occ.all, M.var.dir, out.eval, rep.n = 10, rep.type = "Bootstrap", 
                       out.dir, out.format = "logistic", project = FALSE, G.var.dir,  
                       ext.type = "all", write.mess = FALSE, write.clamp = FALSE)
  
  #After selecting parametrizations that produce the best models, the next step is to create the final models, 
  #and if needed transfer them to other environmental data sets (e.g., to other periods or geographic regions).
  
  #Arguments explanation for ku.enm.mod:
  
  #- occ.all (character) is the  csv file with all the occurrences, columns must be: species, longitud, latitud
  #- M.var.dir (character) name of the forlder containing folders in wich calibration (M) variables are
  #- out.eval (character) name of the folder were evaluation results were written
  #- rep.n (numeric) number of model replicates
  #- rep.type (character) is the replicate type, can be: "Crossvalidate", "Bootstrap", "Subsample"
  #- out.dir (character) name of the output directory to be created and in which all models subdirectories will be created
  #- out.format (character) is the models output format, can be: "raw", "logistic", "cloglog", "cumulative"
  #- project (logical) if TRUE your models will be projected to the scenarios in G.var.dir, default = FALSE
  #- G.var.dir (character) if project is TRUE, name of the forlder containing folders in wich variables of your projection scenarios are
  #- ext.type (character) if project is TRUE, is the extrapolation type of projections, can be: "all", "ext_clam", "ext", and "no_ext", 
    #default = "all". ext = free extrapolation, ext_clam = extrapolation and clamping, no_ext = no extrapolation, and all = all the three previous options
  #- write.mess (logical) if TRUE, grids of MESS analysis results will be written, default = FALSE
  #- write.clamp (logical) if TRUE, a grid of the spatial distribution of clamping will be written, default = FALSE

  #Outputs 
  #A folder named as out.dir with all the subfolders to save maxent final modelsmresults when running the .bat file
  #A batch file for creating all the final maxent models with their projections if project is TRUE

  #Returns
  #Notifications on allowing runing if asked and creation of the java batch file created, and
  #how many models will be created