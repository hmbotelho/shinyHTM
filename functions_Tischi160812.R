
#
# Math functions
#

sem <- function(values) {
  return (sd(values,na.rm=T)/sqrt(sum(!is.na(values))))
}

sem_above <- function(values) {
  m = mean(values,na.rm=T)
  a = which(values > m) # ignores NA
  s = sqrt(sum((values[a]-m)^2)/length(a))
  sem = s/sqrt(length(a))
  return(sem)
}

sem_below <- function(values) {
  m = mean(values,na.rm=T)
  a = which(values < m) # ignores NA
  s = sqrt(sum((values[a]-m)^2)/length(a))
  sem = s/sqrt(length(a))
  return(sem)
}




#
# Spatial position related
#

htm_convert_wellNum_posNum_to_xy <- function(wellID, posID) {  
    
  
  ### GET INFO FROM HTM
  plate.nrow = htm@settings@visualisation$number_positions_y
  plate.ncol = htm@settings@visualisation$number_positions_x
  plate.nwells = plate.nrow * plate.ncol
  plate.nposrow = htm@settings@visualisation$number_subpositions_y
  plate.nposcol = htm@settings@visualisation$number_subpositions_x
  plate.npos = plate.nposrow * plate.nposcol
  
  
  #### Prepare the LUTs
  
  ### WELLS  
  plate.wellNumToRow = vector(length=plate.nwells);
  plate.wellNumToCol = vector(length=plate.nwells); 
  iw = 1;
  for(ir in 1:plate.nrow) {
    for(ic in 1:plate.ncol) {
      plate.wellNumToRow[iw] = ir;
      plate.wellNumToCol[iw] = ic;
      iw=iw+1;
    }  
  }
  
  ### SUBPOSITIONS
  plate.image.distance = 0.4/max(plate.nposcol,plate.nposrow);
  #plate.image.size.cex <<- 2.5/plate.nposcol
  plate.posNumToRow = vector(length=plate.npos);
  plate.posNumToCol = vector(length=plate.npos);
  
  ip = 1;
  dd = plate.image.distance ;
  
  dc = -(plate.nposcol-1)*dd/2;
  for(ic in 1:plate.nposcol) {
    dr = -(plate.nposrow-1)*dd/2; 
    for(ir in 1:plate.nposrow) {
      plate.posNumToCol[ip] = dc;
      plate.posNumToRow[ip] = dr;
      ip = ip + 1;
      dr = dr + dd;
    }
    dc = dc + dd; 
  }
  
  ### Deal with alphanumeric well IDs
  
  if(any(grepl("(?i)[A-Z]",wellID))) {
    wellNum = vector(length=length(wellID))
    for(i in 1:length(wellID)) {  
      ir <- which(LETTERS == substr(wellID[i],1,1))
      ic <- as.numeric(substr(wellID[i],2,1000000000))
      wellNum[i] = (ir - 1) * plate.ncol + ic
      #print(paste(wellID[i],wellNum[i]))
    }
  } else {
    wellNum = wellID
  }

  
  ## return
  
  posNum = posID
  
  list(y = plate.wellNumToRow[wellNum]+plate.posNumToRow[posNum],
       x = plate.wellNumToCol[wellNum]+plate.posNumToCol[posNum])


}

htm_convert_wellNum_to_xy <- function(wellNum) {
  
  ### GET INFO FROM HTM
  plate.nrow = htm@settings@visualisation$number_positions_y
  plate.ncol = htm@settings@visualisation$number_positions_x
  plate.nwells = plate.nrow * plate.ncol

  ### intialise
  xx = vector(length=length(wellNum))
  yy = xx
    
  ### WELLS  
  plate.wellNumToRow = vector(length=plate.nwells);
  plate.wellNumToCol = vector(length=plate.nwells);
  iw = 1;
  for(ir in 1:plate.nrow) {
    for(ic in 1:plate.ncol) {
      plate.wellNumToRow[iw] = ir;
      plate.wellNumToCol[iw] = ic;
      iw=iw+1;
    }  
  }
 
  
  ## return
  
  list(y = plate.wellNumToRow[wellNum],
       x = plate.wellNumToCol[wellNum])
  
  
}

convert_wellA01_to_wellNum <- function(wellA01, nc=12) {
  
  ## Example:
  # convert_wellA01_to_wellNum(c("A01","B02","C02"), nc=12)
  
  
  ### GET INFO FROM HTM
  #plate.nrow = htm@settings@visualisation$number_positions_y
  #plate.ncol = htm@settings@visualisation$number_positions_x
  
  n = length(wellA01)
  
  ### intialise
  wellNum = vector(length=n)
  
  ### 
  for(i in 1:n) {  
    ir <- which(LETTERS == substr(wellA01[i],1,1))
    ic <- as.numeric(substr(wellA01[i],2,1000000000))
    wellNum[i] = (ir - 1) * nc + ic
    print(paste(wellA01[i],wellNum[i]))
  }
  
  
  ## return
  return(wellNum)  
#  list(y = plate.wellNumToRow[wellNum],
#       x = plate.wellNumToCol[wellNum])
  
  
}

#
# Data in- and out-put
#

htmLoadDataFromFile <- function(htm, tablename, path) {
  
  print(paste("reading",path,"..."))
  .table = read.table(file=path, header=T, sep=",", stringsAsFactors=F, check.names=T)
  
  if(is.null(htm)) {
    htm <- htmMake()
  }
  
  cmd <- paste("htm@",tablename," <- .table",sep="")
  print(cmd)
  eval(parse(text=cmd))
  
  return(htm) 
  }

htmSaveDataTable <- function(htm, tablename, path) {
  
  print(paste("writing",path,"..."))
  .table = eval(parse(text=paste("htm@",tablename,sep="")))
  write.csv(.table, file=path)

}

saveTable <- function(data) {
  
  path = gfile("Save as...", type="save")
  write.csv(data, file=path)
  
  }

htmLoadSetttings <- function(htm, path) {
  
  print(paste("loading",path))
  load(path)
  
  if(is.null(htm)) {
    print("constructed new htm object with loaded settings")
    htm <- htmMake()
    htm@settings <- .settings
  } else {
    print("replacing settings in existing htm object.")
    htm@settings <- .settings 
  } 
  
  print(.settings)
  return(htm) 
}

htmSaveSetttings <- function(htm, path) {
  
  print(paste("saving",path))
  .settings = htm@settings
  save(.settings,file=path)
  
}

htmGetColumnNumber <- function(htm,colname) {
  icol = which(colnames(htm@data)==colname)
  if(length(icol)) {
    return(icol)    
  } else {
    return(0)
  }
}


#
# QC
#

htmAddQC <- function(htm,.colname,.min,.max) {
  
  qc = htm@settings@qc
  
  if(qc[1,1]=="None selected") {
    htm@settings@qc <- data.frame(colname=.colname, min=.min, max=.max)
  } else {
    htm@settings@qc <- rbind(htm@settings@qc,data.frame(colname=.colname, min=.min, max=.max))
  }
  
  #print(paste("added image QC: colname =",.colname,"; min =",.min,"; max =",.max))
  return(htm)
  
}

htmGetQCs <- function(htm) {
  
  nQCs = nrow(htm@settings@qc)
  QCs = vector()
  for(i in 1:nQCs) {
    .colname = htm@settings@qc[i,1]
    .min =     htm@settings@qc[i,2]
    .max =     htm@settings@qc[i,3]
    QCs[length(QCs)+1]=paste(.colname,"  min=",.min,"  max=",.max,sep="")
  }
  return(QCs)
  
}

htmRemoveQCs <- function(htm, indices) { 
  
  htm@settings@qc <- htm@settings@qc[-indices,]
  if(nrow(htm@settings@qc)==0) {
    htm@settings@qc <- data.frame(colname="None selected", min=NA, max=NA)
  }
  
  #print(htm@settings@qcImages)
  return(htm)
  
}

htmApplyQCs <- function(htm) {
    
    print("Performing QCs:")
    
    # get QC dataframe from htm object
    data = htm@data
    qc = htm@settings@qc
    
    if(qc[1,1]=="None selected") {
        print("  No QCs selected. Setting all data to valid.")
        htm@data$HTM_qc <- rep(1, nrow(htm@data)) # at this point something happens to the memory of htm... 
    } else {
        # compute QC and put results into htm
        passedQC = dataframeQC(data,qc)  
        # return the modified htm
        htm@data$HTM_qc <- passedQC # at this point something happens to the memory of htm...
    }  
    
    print("")
    print("Excluding experiments (settings QC of all rows to 0):")
    
    experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    
    for(experiment in experiments) {
        
        if(experiment %in% experiments_to_exclude) {
            print(paste(experiment,"setting QC to failed"))
            indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
            htm@data$HTM_qc[indices_all] <- 0
        } 
    }
    
    print("  (The column HTM_qc has been updated or generated.)")
    print("")
    return(htm)
    
}

dataframeQC <- function(data=data.frame(),qc=data.frame()) {
    
    passedallqc = rep(TRUE, nrow(data) )
    
    print(paste("QC:"))
    
    for(i in 1:nrow(qc)) {
        
        values = data[[as.character(qc$colname[i])]]
        
        #passedthisqc = rep(NA, nrow(htm@data) )
        passedthisqc <- ( (values >= qc$min[i]) & (values <= qc$max[i]) & !(is.na(values)))
        passedallqc[ !passedthisqc ] <- FALSE
        
        print(paste("Measurement:", qc$colname[i]))
        print(paste("  Allowed range:", qc$min[i], "..",qc$max[i], "and not NA."))
        print(paste("  Total:", length(passedthisqc)))
        print(paste("  Failed:", sum(!passedthisqc)))        
        
    }
    
    print(paste(" "))
    print(paste("Summary of all QCs:"))
    print(paste("  Total (all QCs):", length(passedallqc)))
    print(paste("  Failed (all Qcs):", sum(!passedallqc)))        
    
    return(passedallqc)    
    
}


#
# Get and set paramters
#

htmSetListSetting <- function(htm, setting, key, value, gui = F) {
  if(gui==T) {
    htm <- get("htm", envir = globalenv()) 
  }
  #print(setting)
  #print(key)
  #print(value)
  
  #print(paste0("htmSetListSetting(htm" , ",'" , setting, "','" , key , "','" , value , "',gui=T)"))
  .list = eval(parse(text=paste("htm@settings@",setting,sep="")))
  .list[[key]]=value
  eval(parse(text=paste("htm@settings@",setting," <- .list",sep=""))) 
  if(gui==T) {
    assign("htm", htm, envir = globalenv())   
  }
  return(htm)
}
 
htmGetListSetting <- function(htm, .setting, .key, gui = F) {  
  if(gui==T) {
    htm <- get("htm", envir = globalenv()) 
  }
  .list = eval(parse(text=paste("htm@settings@",.setting,sep="")))
 
  if(is.null(.list[[.key]]) || is.na(.list[[.key]])) {
    return("None selected")
  } else {
    return(.list[[.key]])
  }
  
}

htmGetVectorSettings <- function(.setting) {
  htm <- get("htm", envir = globalenv()) 
  .out = eval(parse(text=paste("htm@settings@",.setting,sep="")))
  if( is.null(eval(parse(text=paste("htm@settings@",.setting,sep="")))) ) {
    .out = c("None selected")
  }  
  return(.out)
}

htmAddVectorSetting <- function(.setting, .entry) { 
  htm <- get("htm", envir = globalenv())
  
  if( is.null(eval(parse(text=paste("htm@settings@",.setting,sep="")))) ) {
    .vector = c("None selected")
    eval(parse(text=paste("htm@settings@",.setting,"<- .vector",sep="")))  
  }
  
  
  .vector = eval(parse(text=paste("htm@settings@",.setting,sep="")))
  if(.vector[1]=="None selected") {
    .vector <- c(.entry)
  } else if (.entry %in% .vector) {
    print("This was already selected.")
    .vector <- .vector
  } else {
    .vector <- c(.vector,.entry)
  }
  eval(parse(text=paste("htm@settings@",.setting,"<- .vector",sep=""))) 
  print(paste("New setting of",.setting))
  print(.vector)
  
  assign("htm", htm, envir = globalenv())          

}

htmRemoveVectorSetting <- function(.setting, .index) {
  htm <- get("htm", envir = globalenv()) 
  .vector = eval(parse(text=paste("htm@settings@",.setting,sep="")))
  .vector <- .vector[-.index]
  if(length(.vector)==0) {
    .vector=c("None selected")
  }

  eval(parse(text=paste("htm@settings@",.setting,"<- .vector",sep=""))) 
  print("New settings:")
  print(.vector)
  assign("htm", htm, envir = globalenv())          
}


#
# Data normalisation
#

htmNormalization <- function(htm) {
  
  print("*")
  print("* Data normalization")
  print("*" )
  print("")
  
  # get data
  data <- htm@data
  
  # remove previously computed columns
  drops = names(data)[which(grepl("HTM_norm", names(data)))]
  data <- data[ ,!(names(data) %in% drops)]
  
  
  # get all necessary information
  measurements = htmGetVectorSettings("statistics$measurements") 
  experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
  transformation <- htmGetListSetting(htm,"statistics","transformation")
  gradient_correction <- htmGetListSetting(htm,"statistics","gradient_correction")
  normalisation <- htmGetListSetting(htm,"statistics","normalisation")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))

  
  # compute
  for (measurement in measurements) {
      
    cat("\nMeasurement:\n")
    print(measurement)
    cat("\nNegative Control:\n")
    print(negcontrols)
  
    #
    # Check           
    #
    if( ! (measurement %in% names(data)) ) {
      cat(names(data))
      cat("\nError: selected measurement does exist in data columns!\n")
      return(htm)
    }
  
    #
    # Analyze
    #
    
    manipulation <- "__"
    input <- measurement
  
    # Log2
    if(transformation == "log2") {
    
        print("")
        print("Log2:")
        print(paste("  Input:", input))
        
        # compute log transformation
        # create new column name
        manipulation <- paste0(manipulation,"log2__")
        
        output = paste0("HTM_norm",manipulation,measurement)
        
        idsGtZero <- which(data[[input]]>0)
        idsSmEqZero <- which(data[[input]]<=0)
        data[idsGtZero,output] <- log2(data[idsGtZero,input]) 
        data[idsSmEqZero,output] <- NaN
        print(paste("  Output:", output))
        print(paste("  Number of data points:",length(data[[input]])))
        print(paste("  NaN's due to <=0:",length(idsSmEqZero)))
        
        # todo: this should be at a more prominent position
        #print("Replacing -Inf in log scores ******************************")
        #logScores = data[[output]]
        #finiteLogScores = subset(logScores,is.finite(logScores))
        #minimum = min(finiteLogScores)
        #idsInf = which(is.infinite(logScores))
        #logScores[idsInf] <- minimum
        #data[[output]] <- logScores
        
        #htmAddLog("Replacing Infinities in Log2 Score by")
        #htmAddLog(minimum)
        #htmAddLog("Affected Wells:")
        #for(id in idsInf) {
        #  htmAddLog(htm@wellSummary$treatment[id])
        #  htmAddLog(htm@wellSummary$wellQC[id])
        #  htmAddLog(htm@wellSummary[id,logScoreName])
        #  htmAddLog("")
        #}
        
        input <- output
    
      } # if log transformation
  
  
      if(gradient_correction == "median_polish") {
        
        print(paste("  median polish of", input))
        
        # also store the background
        gradient = paste0("HTM_norm",paste0(manipulation,"__medpolish_gradient__"),measurement)
        
        manipulation <- paste0(manipulation,"__medpolish_residuals__")
        output = paste0("HTM_norm",manipulation,measurement)
        
        data[[output]] = rep(NA,nrow(data))
        
        for(experiment in experiments) {
            
          print("")
          print(paste("  Experiment:",experiment))
            
          indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
          #indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qc) & !is.na(data[[input]]))
            
          # extract values
          xy = htm_convert_wellNum_to_xy(data[indices_all, htm@settings@columns$wellnum]) 
          mp = htmMedpolish(x=xy$x, y=xy$y, val=data[indices_all, input])
          
          data[indices_all, output] = mp$residuals
          data[indices_all, gradient] = mp$gradient
          
        } # experiment loop
        
        input <- output
        
      } #medpolish
      
      
      
      if( gradient_correction %in% c("median_7x7","median_5x5","median_3x3")) {
        
        print(paste("  median filter of", input))
        
        gradient = paste0("HTM_norm",paste0(manipulation,"__",gradient_correction,"__gradient__"),measurement)
        manipulation <- paste0(manipulation,"__",gradient_correction,"__residuals__")
        output = paste0("HTM_norm",manipulation,measurement)
        
        data[[output]] = rep(NA,nrow(data))
        
        for(experiment in experiments) {
          
          print(paste("  Experiment:",experiment))
          
          indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
          indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qc) & !is.na(data[[input]]))
          
          xy = htm_convert_wellNum_to_xy(data[indices_ok, htm@settings@columns$wellnum]) 
        
          if(gradient_correction == "median_7x7") {
            mp = htmLocalMedian(x=xy$x, y=xy$y, val=data[indices_ok, input], size=7)
          }
          if(gradient_correction == "median_5x5") {
            mp = htmLocalMedian(x=xy$x, y=xy$y, val=data[indices_ok, input], size=5)
          }
          if(gradient_correction == "median_3x3") {
            mp = htmLocalMedian(x=xy$x, y=xy$y, val=data[indices_ok, input], size=3)
          }
          
          data[indices_ok, output] = mp$residuals
          data[indices_ok, gradient] = mp$gradient
          
        } # experiment loop
        
        input <- output
        
      } #median filter
    
      
      if( gradient_correction %in% c("z_score_5x5")) {
        # Mean = E(X)
        # Variance = E(X^2)-E(X)^2
        # Z-Score = (Xi - E(X)) / Sqrt(E(X^2)-E(X)^2)
        
        print(paste("  5x5 z-score filter of", input))
        
        # also store the background
        standard_deviation = paste0("HTM_norm",paste0(manipulation,"__5x5_standard_deviation__"),measurement)
        mean_value = paste0("HTM_norm",paste0(manipulation,"__5x5_mean__"),measurement)
        manipulation <- paste0(manipulation,"__5x5_z_score__")
        output = paste0("HTM_norm",manipulation,measurement)
        
        data[[output]] = rep(NA,nrow(data))
        data[[standard_deviation]] = rep(NA,nrow(data))
        data[[mean_value]] = rep(NA,nrow(data))
        
        for(experiment in experiments) {
          
          print(paste("  Experiment:",experiment))
          
          indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
          xy = htm_convert_wellNum_to_xy(data[indices_all, htm@settings@columns$wellnum]) 
      
          mp = htmLocalZScore(x=xy$x, y=xy$y, val=data[indices_all, input], size=5)
          
          data[indices_all, mean_value] = mp$avg
          data[indices_all, standard_deviation] = mp$sd
          data[indices_all, output] = mp$z
          
          
        } # experiment loop
        
        input <- output
        
      } #median filter
      
      
      
      if(normalisation != "None selected") {
        
        print("")
        print("Per batch normalisation:")
        print(paste("  Method:",normalisation))
        print(paste("  Input:",input))
        
        # init columns
        manipulation <- paste0(manipulation,normalisation,"__")
        output = paste0("HTM_norm",manipulation,measurement)
        data[[output]] = NA
        print(paste("  Output:",output))
        
        # computation
        #cat("\nComputing normalisations...\n")
        
        for(experiment in experiments) {
          
          #print("")
          #print(paste("  Experiment:",experiment))
          
          indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
          indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qc) & !is.na(data[[input]]))
          
          if("all treatments" %in% negcontrols) {
            indices_controls_ok <- indices_ok
          } else {
            indices_controls_ok <- which((data[[htm@settings@columns$experiment]] == experiment) 
                                         & !is.na(data[[input]]) 
                                         & (data$HTM_qc) 
                                         & (data[[htm@settings@columns$treatment]] %in% negcontrols))
          }
          
          #print(paste("   Total", length(indices_all)))
          #print(paste("   Valid", length(indices_ok)))      
          #print(paste("   Valid Control", length(indices_controls_ok)))
          
          # extract control values 
          valuescontrol <- data[indices_controls_ok, input]
          #print(valuescontrol)
          
          nr_of_controls <-  length(valuescontrol)
          meancontrol <- mean(valuescontrol)    
          sigmacontrol <- sd(valuescontrol) 
          mediancontrol <- median(valuescontrol)
          madcontrol <- mad(valuescontrol)  
          semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
          #print(paste("    Control Mean:", meancontrol))
          #print(paste("    Control SD:", sigmacontrol))
          #print(paste("    Control Median:", mediancontrol))
          #print(paste("    Control MAD:", madcontrol))
          
          if(normalisation == "z_score") {
            data[indices_all, output] <- ( data[indices_all, input] - meancontrol ) / sigmacontrol
          } 
          else if(normalisation == "robust_z_score") {
            data[indices_all, output] <- ( data[indices_all, input] - mediancontrol ) / madcontrol
          } 
          else if(normalisation == "subtract_mean_ctrl") {
            data[indices_all, output] <- data[indices_all, input] - meancontrol 
          }
          else if(normalisation == "divide_by_mean_ctrl") {
            data[indices_all, output] <- data[indices_all, input] / meancontrol 
          }
          else if(normalisation == "subtract_median_ctrl") {
            data[indices_all, output] <- data[indices_all, input] - mediancontrol 
          }
          else if(normalisation == "divide_by_median_ctrl") {
            data[indices_all, output] <- data[indices_all, input] / mediancontrol 
          }
          
        } # experiment loop
        
        input <- output
      
      } # if normalisation
  
    } # measurement loop
  
  return(data)
  
}


#
# Compute scalar product of each data point with the average effect of the whole treatment
#

htmComputeCombinedVector <- function(htm) {
    
    print("*")
    print("* Compute combined effect")
    print("*" )
    print("")
    
    data <- htm@data
    
    # get all necessary information
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    normalisation <- htmGetListSetting(htm,"statistics","normalisation")
    cos_theta_exponent <- as.numeric(htmGetListSetting(htm,"statistics","cos_theta_exponent"))
    treatments <- data[[htm@settings@columns$treatment]]
    
    print("Performing quality cntrol:")
    htm <- htmApplyQCs(htm)
    
    print(paste("Total data points",length(data$HTM_qc)))
    print(paste("Valid data points",sum(data$HTM_qc)))
    
    #
    # initialisation
    #
    length = paste("HTM",normalisation,"length",sep="__")
    data <- data[ , !(names(data) %in% length) ]
    
    cosine = paste("HTM",normalisation,"cosine",sep="__")
    data <- data[ , !(names(data) %in% cosine) ]
    
    projection = paste("HTM",normalisation,"projection",sep="__")
    data <- data[ , !(names(data) %in% projection) ]
    
    features = names(data)[which(grepl(normalisation,names(data)))]
    cat("\nFeatures:")
    print(features)
    
    data[[length]] = NA
    data[[cosine]] = NA
    data[[projection]] = NA
    
    
    # computation
    cat("\nComputing combined effect\n")
    
    for(experiment in experiments) {
        
        if(experiment %in% experiments_to_exclude) next
        
        print(paste("  Experiment:",experiment))
        
        for (treatment in unique(treatments)) {
            
            indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qc) & (data[[htm@settings@columns$treatment]] == treatment) )
            indices_all <- which((data[[htm@settings@columns$experiment]] == experiment) & (data[[htm@settings@columns$treatment]] == treatment) )
            
            # compute normalised direction
            v_avg = vector()
            for (feature in features) {
                v_avg <- c(v_avg, mean(data[indices_ok, feature], na.rm=T))  
            }
            print(treatment)
            names(v_avg) <- features
            print(v_avg)
            
            # compute length
            data[indices_all, length] = 0
            for (feature in features) { # sum square
                data[indices_all, length] = data[indices_all, length] + data[indices_all, feature]^2
            }
            data[indices_all, length] = sqrt(data[indices_all, length])
            
            # compute cosine
            data[indices_all, cosine] = 0
            for (feature in features) {  # scalar product
                data[indices_all, cosine] = data[indices_all, cosine] + data[indices_all, feature] * v_avg[feature]
            }
            v_avg_norm <- sqrt(sum(v_avg*v_avg))
            data[indices_all, cosine] = data[indices_all, cosine] / (data[indices_all, length] * v_avg_norm)
            
            # compute projection
            data[indices_all, projection] = data[indices_all, length] * sign(data[indices_all, cosine]) * abs(data[indices_all, cosine])^cos_theta_exponent 
            #data[indices_all, projection] = abs(data[indices_all, cosine])^cos_theta_exponent 
            
        }
        
        
    } # experiment loop
    
    return(data)
    
}


#
# Treatment Summary
#

htmTreatmentSummary_Data <- function(htm) {
  
  print("")
  print("Treatment Summary:")
  print("******************")
  print("")
  
  # get all necessary information
  data <- htm@data
  measurement <- htmGetListSetting(htm,"statistics","measurement")
  experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
  #experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  #transformation <- htmGetListSetting(htm,"statistics","transformation")
  treatments <- sort(unique(data[[htm@settings@columns$treatment]]))
  colObjectCount <- htmGetListSetting(htm,"statistics","objectCount")
  
  #ctrls <- htm@settings@ctrlsNeg
  negative_ctrl <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  positive_ctrl <- c(htmGetListSetting(htm,"statistics","positiveControl"))
  
  # output
  print("");print("Experiments:")
  print(experiments)
  print("");print(paste("Number of treatments:",length(treatments)))
  print("");print(paste("Negative control:",negative_ctrl))
  print("");print(paste("Positive control:",positive_ctrl))
  print("");print(paste("Measurement:",measurement))
  print(""); print("")
  
  if(!(measurement %in% names(data))) {
    print(paste("ERROR: measurement",measurement,"does not exist in data"))
    return(0)
  }
  if(!(colObjectCount %in% names(data))) {
    print(paste("ERROR: object count",measurement,"does not exist in data"))
    return(0)
  }
  
  
  numEntries = length(treatments)
  
  results <- data.frame(measurement=rep(measurement,numEntries),
                        controls=rep(negative_ctrl,numEntries),  
                        treatment=rep(NA,numEntries),
                        batches = rep(NA,numEntries),
                        means = rep(NA,numEntries),
                        median__means = rep(NA,numEntries),
                        #z_scores = rep(NA,numEntries),
                        #median__z_scores = rep(NA,numEntries),
                        
                        t_test__estimate=rep(NA,numEntries),
                        t_test__p_value=rep(NA,numEntries),
                        t_test__signCode=rep(NA,numEntries),
                        
                        
                        #z_score__allBatches=rep(NA,numEntries),
                        #robust_z_score__allBatches=rep(NA,numEntries),
                        
                        mean_number_of_objects_per_image=rep(NA,numEntries),

                        numObjectsOK=rep(NA,numEntries),
                        numImagesOK=rep(NA,numEntries), 
                        numReplicatesOK=rep(NA,numEntries),
                        #numPositionsOK=rep(NA,numEntries),
                        #numPositions=rep(NA,numEntries),
                        stringsAsFactors = FALSE
  )
  

  
  ###################################
  # Compute stats
  ###################################
  
  print("Computing statistics...")
  ids_treatments = split(1:nrow(data), data[[htm@settings@columns$treatment]])
  
  i=0
  
  for(ids in ids_treatments) {
    
    # ********************************
    # T-test  
    # ********************************
    
    # treatment name
    treat <- data[ids[1],htm@settings@columns$treatment]
    
    # init
    t_test__p_value = NA
    t_test__signCode = NA
    t_test__estimate = NA
    z_scores = NA
    median__z_scores = NA
    median__means = NA
    means = NA
    batches = NA
    d = data.frame(value=NA, treatment=NA, experiment=NA)
    
    # compute
    if(1) { #treat %in% c("SETDB1_s19112")) {
      
      # only keep valid treatment values to find the corresponding experiments
      ids <- ids[which((data[ids,"HTM_qc"]==1) & !(is.na(data[ids,measurement])))]
      exps <- unique(data[ids,htm@settings@columns$experiment])
      
      # extract treatment and control values of the respective experiments
      d <- subset(data, 
                  (data[[htm@settings@columns$treatment]] %in% c(treat,negative_ctrl))
                  & (data[[htm@settings@columns$experiment]] %in% exps) 
                  # & !(data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")) 
                  & (data[["HTM_qc"]]==1)
                  & !(is.na(data[[measurement]])), 
                  select = c(htm@settings@columns$treatment,measurement,htm@settings@columns$experiment,colObjectCount))
      
      names(d)[names(d)==measurement] <- "value"
      names(d)[names(d)==htm@settings@columns$treatment] <- "treatment"
      names(d)[names(d)==htm@settings@columns$experiment] <- "experiment"
      names(d)[names(d)==colObjectCount] <- "count"
      
      d$treatment = ifelse(d$treatment %in% negative_ctrl, "control", d$treatment)
      
      #print(d)
        
      if ( (sum(d$treatment=="control")>1) & (sum(d$treatment==treat)>1) ) {
        
        #d$experiment <- as.factor(substr(d$experiment, nchar(d$experiment)-7+1, nchar(d$experiment)))
        d$treatment <- as.factor(d$treatment)
        d$treatment <- relevel( d$treatment, "control" ) # control must be the 1st level for the linear model
        t <- t.test(d$value ~ d$treatment)  # as there typically is enough data no equal variance is assumed
        
        nBlocks = length(unique(d$experiment)) 
        n = nrow(d)
        #print(2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 ))
        
        t_test__p_value <- 2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 )
        t_test__estimate <- t$estimate[2]
        t_test__signCode <- ifelse(t_test__p_value<0.001,"***",
                                   ifelse(t_test__p_value<0.01,"**",
                                          ifelse(t_test__p_value<0.05,"*",
                                                 ifelse(t_test__p_value<0.1,"."," "
                                                 ))))
      }
      
      if ( (sum(d$treatment=="control")>=1) & (sum(d$treatment==treat)>=1) ) {
        
        d_ctrl = subset(d, d$treatment=="control")
        means_ctrl <- tapply(d_ctrl$value, d_ctrl$experiment, mean)
        sds_ctrl <- tapply(d_ctrl$value, d_ctrl$experiment, sd)
        
        d_treat = subset(d, d$treatment==treat)
        means_treat <- tapply(d_treat$value, d_treat$experiment, mean)
        
        tryCatch(z_scores <- (means_treat - means_ctrl) / sds_ctrl
          , error = function(e) {
            print(d)
            print(treat)
            print(means_treat)
            print(negative_ctrl)
            print(means_ctrl)
            #print(sds_ctrl)
            print(z_scores)
            print(ids)
            print(idsOld)
            print(data[ids,])
            print(data[idsOld,])
            print(e)
            
            
            ddd
          })
        
          median__z_scores = median(z_scores)
        
        z_scores = paste(round(z_scores,2),collapse=";")
        
      }
        
      #print(d)
      #print(treat)
      #print(means_treat)
      #print(negative_ctrl)
      #print(means_ctrl)
      #print(sds_ctrl)
      #print(z_scores)
      #ddd        
        
    
    } # select treatment for debugging
    
    
    if(!(treat %in% negative_ctrl)) {
        d_treated = subset(d, d$treatment==treat )
    } else {
        d_treated = subset(d, d$treatment=="control")
    }
    
    
    # these  values need no negative control, that's why they are outside of above if-statement
    means_treated <- tapply(d_treated$value, d_treated$experiment, mean)
    batches = paste(names(means_treated),collapse=";")
    means = paste(round(means_treated,3),collapse=";")
    median__means = median(means_treated)
      
  
    i = i + 1
    results$treatment[i] <- treat
    results$batches[i] = batches
    results$means[i] = means
    results$median__means[i] = median__means
    results$t_test__p_value[i] = t_test__p_value
    results$t_test__signCode[i] = t_test__signCode
    results$t_test__estimate[i] = t_test__estimate
    #results$z_scores[i] = z_scores
    #results$median__z_scores[i] = median__z_scores
    results$numObjectsOK[i] = sum(d_treated$count)
    results$numImagesOK[i] = nrow(d_treated)
    results$numReplicatesOK[i]= length(unique(d_treated$experiment))
    results$mean_number_of_objects_per_image[i] = results$numObjectsOK[i]/results$numImagesOK[i]
    
    
  }  # treatment loop   
    

  print("")
  print("done. Created Treatment Summary Table.")
  
  
  # Diagnostics 
  
  #hist(res)
  #dev.new()
  #qqnorm(res)
  #qqline(res)
  #print(wellScoreForANOVA)
  
  # display controls plot: raw well scores
  #print("Plot control scores...")
  #htmJitterplot(htm, cx="experiment", cy=well_raw_score, .ylab=well_raw_score, datatype="wells", 
  #              treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
  #              showMedian = F, showMean = T)
  
  # display controls plot: batch corrected well scores
  #print("Plot batch corrected control scores...")
  #htmJitterplot(htm, cx="experiment", cy=wellMinusMeanCtrlScores, .ylab=wellMinusMeanCtrlScores, datatype="wells", 
  #              treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
  #              showMedian = F, showMean = T)
  
  
  # save controls plot
  #  htmJitterplot(htm, cx="experiment", cy=wellScoreForANOVA, .ylab=wellScoreForANOVA, datatype="wells", 
  #                treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
  #                showMedian = F, showMean = T, save2file = T, newdev = F)
  
  # save histogram as plot
  #  print(paste("histo:",wellMinusMeanCtrlScores))
  #  htmHisto(wellMinusMeanCtrlScores, datatype="wells", treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"), save2file=T)
  
  #htm@wellSummary$exp_treat = paste(htm@wellSummary$experiment,htm@wellSummary$treatment,sep="_")
  #edit(htm@wellSummary$exp_treat)
  #print(wellMinusMeanCtrlScores)
  # todo: plot only if makes sense
  #htmJitterplot(htm, cx="exp_treat", cy=wellMinusMeanCtrlScores, .ylab=wellMinusMeanCtrlScores, datatype="wells", 
  #                treatmentSubset = c(htmGetListSetting(htm,"statistics","negativeControl"),htmGetListSetting(htm,"statistics","positiveControl")),
  #                showMedian = F, showMean = T, save2file = F, newdev = F)
  
  
  #####
  
  if(positive_ctrl != "None selected") {
    
    print("")
    print("")
    print("Checking positive and negative control separation in each batch:")
    print("")
    print(paste("Measurement:",measurement))
    print(paste("Positive control:",positive_ctrl))
    print(paste("Negative control:",negative_ctrl))
    print("")
    
    z_scores = c()
    
    for(exp in experiments) {
      
      d <- subset(data, (data[[htm@settings@columns$experiment]]==exp) & (data$HTM_qc==1) & !(is.na(data[[measurement]])), select = c(htm@settings@columns$treatment, measurement))
      names(d)[names(d) == measurement] <- "value"
      names(d)[names(d) == htm@settings@columns$treatment] <- "treatment"
      
      d_neg <- subset(d,d$treatment == negative_ctrl)
      mean_neg = mean(d_neg$value)
      sd_neg = sd(d_neg$value)
      n_neg = length(d_neg$value)
      
      d_pos <- subset(d,d$treatment == positive_ctrl)
      mean_pos = mean(d_pos$value)
      sd_pos = sd(d_pos$value)
      n_pos = length(d_pos$value)
      
      #print(paste(min_neg,max_neg,mean_pos,sd_pos))
      #probability_of_pos_outside_neg = 1 - integrate( function(x) {dnorm(x,mean=mean_pos,sd=sd_pos)}, min_neg, max_neg)$value
      z_score = (mean_pos-mean_neg) / sd_neg
      z_scores = c(z_scores, z_score)
      #t_value = (mean_pos-mean_neg) / sqrt(sd_pos^2+sd_neg^2)
      
      
      #print(paste0(exp,"  N_neg: ",n_neg,"  N_pos: ",n_pos,"  Probability: ",round(probability_of_pos_outside_neg,3))) #,"  t-value: ",t_value))
      #quality = ifelse(abs(z_score)<1,"XX",
      #                 ifelse(abs(z_score)<2,"X",""
      #                               ))
      quality = ""
      
      if ( exp %in% htmGetVectorSettings("statistics$experiments_to_exclude") ) {
        comment = "(Excluded)  " 
      } else {
        comment = ""
      }
      
      print(paste0(comment,exp,"; N_neg: ",n_neg,"; N_pos: ",n_pos,"; mean z-score of positive controls: ",round(z_score,3)," ",quality)) #,"  t-value: ",t_value))
      
    }
  
    print("")
    print(paste("z-scores (N, mean, sd):",length(z_scores),mean(z_scores, na.rm=T),sd(z_scores, na.rm=T)))
    
  }
  
  
  # sorted hit-list with significance level * ** ***
  # plot colored according to significance level
  #with hits marked as sign
  
  results_ordered <- results[order(results$t_test__p_value),]
  return(results_ordered)
  
}


#
# Local data normalisation
#

htmMedpolish <- function(xx, yy, val) {
  
  
  # averaging for multi-sub-positions?
  ny = htm@settings@visualisation$number_positions_y
  nx = htm@settings@visualisation$number_positions_x
  m = matrix(nrow=nx,ncol=ny)
  mi = m 
  for(i in seq(1:length(val))) {
    #print(paste(xx[i],yy[i],val[i]))
    m[xx[i],yy[i]] <- val[i]
    mi[xx[i],yy[i]] <- i # remember where the data belongs in the original format
  }
  #print("raw"); dev.new(); image(m, col=gray((0:32)/32))
  
  med = medpolish(m, maxiter = 100, na.rm = T)
  m_gradient <-  med$overall + outer(med$row,med$col, "+")
  m_residuals <- m - m_gradient
  
  #print("gradient"); dev.new(); image(m_gradient, col=gray((0:32)/32))
  #print("residuals"); dev.new(); image(m_residuals, col=gray((0:32)/32))
  
  # covert back
  val_gradient = vector(length=length(val))
  val_residual = vector(length=length(val))
  for(i in seq(1:length(val))) {
    val_gradient[mi[xx[i],yy[i]]] = m_gradient[xx[i],yy[i]]
    val_residual[mi[xx[i],yy[i]]] = m[xx[i],yy[i]] - m_gradient[xx[i],yy[i]] 
  }
  
  list(gradient = val_gradient,
      residuals = val_residual)
}

htmLocalMedian <- function(xx, yy, val, size) {
  
  print(paste("  median filter with size", size))
  
 
  nx = htm@settings@visualisation$number_positions_x
  ny = htm@settings@visualisation$number_positions_y
  x <- htmXYVtoMatrix(xx, yy, val, nx, ny) # averaging for multi-sub-positions?
  m <- x$m
  
  m_gradient <- as.matrix(focal(raster(m), matrix(1, size, size), function(z) median(z, na.rm=T), pad = T, padValue = NA))
  m_residuals <- m - m_gradient
  
  list(gradient = htmMatrixToXYV(xx, yy, m_gradient, x$mi),
       residuals = htmMatrixToXYV(xx, yy, m_residuals, x$mi))
}

htmXYVtoMatrix <- function(xx, yy, val, nx, ny) {
  m = matrix(nrow=ny, ncol=nx)
  mi = m 
  for(i in seq(1:length(val))) {
    m[yy[i],xx[i]] <- val[i]
    mi[yy[i],xx[i]] <- i # remember where the data belongs in the original format
  }
  return(list(m = m, mi = mi))
}

htmMatrixToXYV <- function(xx, yy, m, mi) {
  val = vector(length=length(xx))
  for(i in seq(1:length(xx))) {
    val[mi[yy[i],xx[i]]] = m[yy[i],xx[i]]
  }
  return(val)
}

htmLocalZScore <- function(xx, yy, val, size) {
  
  print(paste("  local z_score filter with size", size))
  
  
  nx = htm@settings@visualisation$number_positions_x
  ny = htm@settings@visualisation$number_positions_y
  x <- htmXYVtoMatrix(xx, yy, val, nx, ny) # averaging for multi-sub-positions?
  m <- x$m
  
  m_mean <- as.matrix(focal(raster(m), matrix(1, size, size), function(z) mean(z, na.rm=T), pad = T, padValue = NA))
  m_meansqr <- as.matrix(focal(raster(m^2), matrix(1, size, size), function(z) mean(z, na.rm=T), pad = T, padValue = NA))
  m_sd = sqrt( m_meansqr - m_mean^2 )
  m_z = (m - m_mean) / m_sd
  
  #print("before")
  #print(m[1:10,1:6])
  #print("avg")
  #print(m_mean[1:10,1:6])
  #print("sd")
  #print(m_sd[1:10,1:6])
  #print("z")
  #print(m_z[1:10,1:6])
  #ddd

  list(avg = htmMatrixToXYV(xx, yy, m_mean, x$mi),
       sd = htmMatrixToXYV(xx, yy, m_sd, x$mi),
       z = htmMatrixToXYV(xx, yy, m_z, x$mi))
}




  
