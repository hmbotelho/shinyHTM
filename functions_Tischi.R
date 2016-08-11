# todo:

# - Plate Layout: replace gedit by dropdownlists to avoid pressing enter
# - Heatmaps: compute image positions on the fly (don't add as column)
#     - do this in a clean way
# - Plate Plot: 
#    -- LUT min max selection: think about avoiding pressing enter
#    -- add some GUI elements for initialising the LUTminmax by min max of all plates or percen
#    -- aspect ratio compute from nWellsX and nWellsY
# - JOHN: allow one only window open at the time
# - visualisation options: prefill and replace by sliders
# - Plate Plot: have three different Highlight Treatment drop downs (black, dark-green, yellow)
# - Save all plates: add treatment highlight somehow
# - Highlight treatment: remove button, but add a NONE
# - INteractive plate plot: button: click and show treatment

# - add measurements gui: Method in label is wroing

# gui_AddRemoveVectorSetting: frames: Add and Remove, specific instructions in title
# remove frames: put buttons to the left
# add: text window: currently selected

# add more columns (division?)  how to?
# - text field for code that is saved in the settings, e.g.
# htm@data[["newcolumn"]] = htm@data$PathName_TOTAL / htm@data$PathName_All

# run extra code if:
# - new image table is loaded
# - if the extra code changes (either manually or when loading new settings)

# button for "run extra code"

# image normalisation: add "without_normalisation"

# have for each negative control an own plateQC evaluation and column

# ALLOW ONLY ONE NEGATIVE CONTROL

# plate QC: make negative Control to percentage

# "Select Treatment Summary Methods: Within Plate 
# "Select Treatment Summary Methods: Across Plates 


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

htmGenerateImageObjectLink <- function(htm, columns=NULL) {
  
  print("Generating columns that link image and object table.")
  if (is.null(columns)) {
    # columns to match images and objects
    c1 = htmGetListSetting(htm,"columns","treatment")
    c2 = htmGetListSetting(htm,"columns","experiment")
    c3 = htmGetListSetting(htm,"columns","wellnum")
    c4 = htmGetListSetting(htm,"columns","posnum")  
    htm@objectdata$HTM_imageobjectlink = paste(htm@objectdata[[c1]],htm@objectdata[[c2]],htm@objectdata[[c3]],htm@objectdata[[c4]],sep="--")
    htm@data$HTM_imageobjectlink = paste(htm@data[[c1]],htm@data[[c2]],htm@data[[c3]],htm@data[[c4]],sep="--")
    htm@objectdata$HTM_imageID = unlist(lapply(htm@objectdata$HTM_imageobjectlink, function(x) which(htm@data$HTM_imageobjectlink==x)))    
  }
  
  return(htm)
}

htmOverview <- function(htm) {
  cExp = htm@settings@columns$experiment
  cWell = htm@settings@columns$wellnum
  cPos = htm@settings@columns$posnum
  
  nExp = length(unique(paste(htm@data[[cExp]])))
  nWell = length(unique(paste(htm@data[[cExp]],htm@data[[cWell]],sep="_")))
  nPos =  length(unique(paste(htm@data[[cExp]],htm@data[[cWell]],htm@data[[cPos]],sep="_")))
  
  print("")
  print("Overiew of the contents of the image table:")
  print(paste("experiments =",nExp))
  print(paste("wells =",nWell))
  print(paste("wells/experiment =",nWell/nExp))
  print(paste("positions =",nPos))
  print(paste("positions/well =",nPos/nWell))
                 
  
  
}

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

htmCellsToTreatments <- function(d, stats = "classic", keepdate = F){
  
  if(keepdate) {
    d$treatment = unlist(lapply(strsplit(rownames(d),"_"), function(x) paste(x[1],x[2])))
  } else {
    d$treatment = unlist(lapply(strsplit(rownames(d),"_"), function(x) x[1]))  
  }
    
  
  if(stats =="classic") {
    dmean <- ddply(d, .(treatment), numcolwise(mean))
    rownames(dmean) <- dmean$treatment
    dmean$treatment <- NULL
    return(dmean)
  }
  
  if(stats =="robust") {
    dmean <- ddply(d, .(treatment), numcolwise(median))
    rownames(dmean) <- dmean$treatment
    dmean$treatment <- NULL
    return(dmean)
  }
  
  
}

htmMaxDeviationPerChannel <- function(treatFeat) {
  
  treatFeat = htm@other$treatFeat
  treatFeat[apply(treatFeat, 2, Negate(is.finite))] <- NA 
  treatFeat<-treatFeat[,-which(grepl("Max",colnames(treatFeat)))]
  #treatFeat<-treatFeat[,which(grepl("IntegratedIntensity_",colnames(treatFeat)))]
  treatFeat<-treatFeat[,which(grepl("Texture_",colnames(treatFeat)))]
  
  
  idsChannels = split(1:ncol(treatFeat), unlist(lapply(strsplit(colnames(treatFeat),"_"), function(x) {x[3]})))
  print(idsChannels)
 
  for (ichannel in 1:nrow(treatFeat)) {
    print(rownames(treatFeat)[ichannel])
  
    for (ids in idsChannels) {
      iMax = ids[which.max(abs(treatFeat[ichannel,ids]))]
      print(paste(colnames(treatFeat)[iMax], treatFeat[ichannel,iMax]))
    }
  
  }
  
}

htmAnalyseMahalanobis <- function(htm, d, selectSubset = "") {
  # input:
  # rows: treat_cells
  # cols: features
  
  treatments <- unique(unlist(lapply(strsplit(rownames(d),"_"), function(x) x[1])))
  channels <- unique(unlist(lapply(strsplit(colnames(d),"xx"), function(x) {x[2]})))
  
  N = ncol(d)

  
  #for (channel in channels) {
  #  selectSubset = ""
  #  print(channel)
    
    d$SelfMahalanobis <- NA
    
    for (treatment in treatments) {
      
      idsTreatment = which(grepl(treatment,rownames(d)))
      x <- d[idsTreatment,]
      
      if(selectSubset != "") {
        ids = which(grepl(selectSubset, colnames(x)))
        x <- x[,ids]
      }
      
      ids = which( !grepl("SelfMahalanobis", colnames(x)))
      x <- x[,ids]
       
      N = ncol(x)
    
      Sx <- cov(x)
      D2 <- mahalanobis(x, colMeans(x), Sx)
      d[names(D2),"SelfMahalanobis"] = D2 / N
      
      # minimum:
      i = which.min(d[idsTreatment,"SelfMahalanobis"])
      iObject = as.numeric(strsplit(rownames(d)[idsTreatment[i]],"_")[[1]][2])
      print(paste(treatment,"i",idsTreatment[i],"iObjectdata",iObject))
      #htmShowObjectsFromRow(htm,iObject)
      
      # maximum:
      #i = which.max(d[idsTreatment,"SelfMahalanobis"])
      #iObject = as.numeric(strsplit(rownames(d)[idsTreatment[i]],"_")[[1]][2])
      #print(paste(treatment,"i",idsTreatment[i],"iObjectdata",iObject))
      #htmShowObjectsFromRow(htm,iObject)
      
    }  
  
  #}# channel loop
  
  return(d)
  
}

htmObjectMultiFeatureAnalysis <- function(htm, centerChannel, stats="classic", perBatchNorm = F, divideByCellArea = F) {
  
  print("")
  print("Image-based multi-feature analysis:")
  print("***********************************")
  print("")
  
  data <- htm@objectdata
  
  # get all necessary information
  treatments <- sort(unique(htm@objectdata[[htm@settings@columns$treatment]]))
  features_channels <- htmGetVectorSettings("statistics$multipleFeatureSelection")
  negative_ctrl <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  experiments <- sort(unique(htm@objectdata[[htm@settings@columns$experiment]]))
  experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
  
  # output
  print("");print("Experiments:")
  print(experiments)
  print("");print(paste("Number of Treatments:",length(treatments)))
  print("");print(paste("Negative Control:",negative_ctrl))
  print(""); print("")
  
  numEntries = length(treatments) 
 
  #channels <- unique(unlist(lapply(strsplit(features_channels,"xx"), function(x) {x[2]})))
  #features <- unique(unlist(lapply(strsplit(features_channels,"xx"), function(x) {paste(x[1],x[3],sep=".*")})))

  channels_to_exclude = c("Vimentin","Cla")
  
  
  if(divideByCellArea) {
    
    htm@objectdata[[htm@settings@columns$treatment]]
    
    cNorm <- htm@objectdata[["Shape_xxCELLxx_Area"]]
    
    for (feature_channel in features_channels) {
      
      htm@objectdata[[feature_channel]] = htm@objectdata[[feature_channel]] / cNorm
    }
    
   # htm@objectdata[["Shape_xxCELLxx_Area"]] <- cNorm
    
  }
  
  for (feature_channel in features_channels) {
    
    #feature_channels = colnames(htm@objectdata)[which(grepl(feature,colnames(htm@objectdata)))]
  
    #varTotal = 0
    
    #vars = vector()
    #for(feature_channel in feature_channels) {
            
    #  vars <- append(vars, sd(htm@objectdata[[feature_channel]])^2)
    #  print(paste(feature_channel, sqrt(sd(htm@objectdata[[feature_channel]])^2)))
      
      for(experiment in experiments) {
        
        # normalise to control
        # find control cells
        if(perBatchNorm) {
          idsCtrl = which( (htm@objectdata[[htm@settings@columns$treatment]]==negative_ctrl) 
                         & (htm@objectdata[[htm@settings@columns$experiment]]==experiment) )
        } else {
          idsCtrl = which( (htm@objectdata[[htm@settings@columns$treatment]]==negative_ctrl))
        }
        
        
        meanCtrlLog2 = mean(log2(htm@objectdata[idsCtrl, feature_channel]))
        meanCtrl = mean(htm@objectdata[idsCtrl, feature_channel])
        sdCtrl = sd(htm@objectdata[idsCtrl, feature_channel])
        medianCtrl = median(htm@objectdata[idsCtrl, feature_channel])
        madCtrl = mad(htm@objectdata[idsCtrl, feature_channel])
        
        ids = which( (htm@objectdata[[htm@settings@columns$experiment]]==experiment) )
        
        channel = strsplit(feature_channel,"xx")[[1]][2]
        feature_channel_Norm = paste0(channel,"_",feature_channel,"__Norm")
        feature_channel_raw = paste0(channel,"_",feature_channel,"__raw")
        
        htm@objectdata[ids, feature_channel_raw] = htm@objectdata[ids, feature_channel]
        
        if(stats=="classic") {
          htm@objectdata[ids, feature_channel_Norm] = (htm@objectdata[ids, feature_channel] - meanCtrl) / sdCtrl
        }
        
        if(stats=="robust") {
          htm@objectdata[ids, feature_channel_Norm] = (htm@objectdata[ids, feature_channel] - medianCtrl) / madCtrl
        } 
     
        if(stats=="log2") {
          htm@objectdata[ids, feature_channel_Norm] = log2(htm@objectdata[ids, feature_channel]) - meanCtrlLog2 
        } 
        
        if(stats=="none") {
          htm@objectdata[ids, feature_channel_Norm] = htm@objectdata[ids, feature_channel] 
        } 
        
        
      }
      
    #}
    
    
    # variance normalisation
    #iCenter = which(grepl(centerChannel,feature_channels))
    #meanCenter = mean(htm@objectdata[[feature_channels[iCenter]]])
    #print(paste("Centering around",feature_channels[iCenter]))
    #print(paste("*** Standard deviation",feature,sqrt(mean(vars)),length(vars)))
    #print("")
    
    #for(feature_channel in feature_channels) {
      
    #  # channel = strsplit(feature_channel,"xx")[[1]][2]
    #  feature_channel_varNorm = paste0(feature_channel,"__VarNorm")
    #  htm@objectdata[[feature_channel_varNorm]] = (htm@objectdata[[feature_channel]] - meanCenter) / sqrt(mean(vars))

    #}
     
    
  }
  

  l = list()
  
  # varNorm
  #ids = which(grepl("__VarNorm",colnames(htm@objectdata)) )
  #d = htm@objectdata[ids]
  #print(paste(htm@objectdata[[htm@settings@columns$treatment]],ids,sep="_"))
  #rownames(d) <- paste(htm@objectdata[[htm@settings@columns$treatment]],seq(1:nrow(htm@objectdata)),sep="_")
  #d <- d[,order(colnames(d))]
  #d <- d[order(rownames(d)),]
  #l$cells_varNorm <- d
  
  # zScoreNorm
  ids = which( grepl("__Norm",colnames(htm@objectdata))  )
  d = htm@objectdata[ids]
  d$treatment = htm@objectdata[[htm@settings@columns$treatment]]
  d$date = htm@objectdata[[htm@settings@columns$experiment]]
  rownames(d) <- paste(htm@objectdata[[htm@settings@columns$treatment]], htm@objectdata[[htm@settings@columns$experiment]], formatC(seq(1:nrow(htm@objectdata)), width = 3, format = "d", flag = "0"),sep="_")
  d <- d[,order(colnames(d))]
  d <- d[order(rownames(d)),]
  l$cells_Norm <- d
  
  
  # raw
  ids = which(  grepl("__raw",colnames(htm@objectdata))  )
  d = htm@objectdata[ids]
  d$treatment = htm@objectdata[[htm@settings@columns$treatment]]
  d$date = htm@objectdata[[htm@settings@columns$experiment]]
  rownames(d) <- paste(htm@objectdata[[htm@settings@columns$treatment]],seq(1:nrow(htm@objectdata)),sep="_")
  d <- d[,order(colnames(d))]
  d <- d[order(rownames(d)),]
  l$cells_raw <- d

  
  return(l)
  
  
  #print(colnames(htm@data))
  
  
  if(0) {
    
    # initialisation
    d <- data.frame(treatment = rep(NA,numEntries))
    for (feature in features) {
      d[[feature]] <- NA 
      htm@data[[paste0(feature,"__zScore")]] <- NA
    }
    
    
    htm@data$MDD_vsCtrl = 0
    htm@data$MDD_vsTreat = 0
  
    
    
    # computation
    for(i in 1:length(treatments)) {
      
      treatment = treatments[i]
      #print(treatment)
      
      for (measurement in measurements) {
        
        indices_ok_treat <- which( ( htm@data[[htm@settings@columns$treatment]] == treatment ) 
                                  & (htm@data$HTM_qcImages==1) 
                                  & !is.nan(htm@data[[measurement]]) & !(htm@data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")))
        
        
        indices_ok_ctrl <- which( ( htm@data[[htm@settings@columns$treatment]] == negative_ctrl) 
                                   & (htm@data$HTM_qcImages==1) 
                                   & !is.nan(htm@data[[measurement]]) & !(htm@data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")))
        
        
        #print(paste("    Treatment", treatment))
        #print(paste("    Images Valid Treatment", length(indices_ok_treat)))      
        #print(paste("    Images Valid Control", length(indices_ok_ctrl)))
        
        mean_treat = mean(htm@data[indices_ok_treat,measurement])
        mean_ctrl = mean(htm@data[indices_ok_ctrl,measurement])
        sd_ctrl = sd(htm@data[indices_ok_ctrl,measurement])
        
        d[i,measurement] = ( mean_treat - mean_ctrl ) / sd_ctrl
        
        htm@data[indices_ok_treat,"MDD_vsCtrl"] =  htm@data[indices_ok_treat,"MDD_vsCtrl"] + ((htm@data[indices_ok_treat,measurement] - mean_ctrl) / sd_ctrl)^2
        htm@data[indices_ok_treat,"MDD_vsTreat"] =  htm@data[indices_ok_treat,"MDD_vsTreat"] + ((htm@data[indices_ok_treat,measurement] - mean_treat) / sd_ctrl)^2
        htm@data[indices_ok_treat,paste0(measurement,"__zScore")]  = (htm@data[indices_ok_treat,measurement] - mean_ctrl) / sd_ctrl
        
      }
    
      d[i,"treatment"] = treatment
    
    }
    
    htm@data$MDD_vsCtrl = sqrt(htm@data$MDD_vsCtrl / length(measurements))
    htm@data$MDD_vsTreat = sqrt(htm@data$MDD_vsTreat / length(measurements))
    
    rownames(d) <- d$treatment
    d$treatment <- NULL
    
    l = list()
    l$treatFeat <- d
    l$images <- htm@data
    }
  
  
 # return(l)
  
}

htmMakeFeatureFrame <- function(d) {
  
  
  channels <- unique(unlist(lapply(strsplit(colnames(d),"xx"), function(x) {x[2]})))
  
  i = 0
  
  for (channel in channels) {
  
    print(channel)
    ids = which(grepl(channel,colnames(d)))
    ds = d[,ids]
    rownames(ds) <- paste(rep(channel,nrow(ds)),rownames(ds),sep="_")
    colnames(ds) <- unlist(lapply(colnames(ds), function(x) {gsub(channel,"",x)}))
    
    #print( rownames(ds))
    #print( colnames(ds))
    
    if(i==0) {
        dc = ds 
    } else { 
        dc = rbind(dc, ds)
    }
    
    i = i + 1
  
  }
  
  dc2 <- dc
  dc2$channeltreat = unlist(lapply(strsplit(rownames(dc),"_"), function(x) {paste(x[1],x[2],sep="_")}))
  dc2mean <- ddply(dc2, .(channeltreat), numcolwise(mean))
  rownames(dc2mean) <- dc2mean$channeltreat
  dc2mean$channeltreat <- NULL
  
  l = list()
  l$df <- dc
  l$dfmean <- dc2mean
  return(l)
  
}

htmMDStreatFeat <- function(dTreatFeat, negCtrl) {
  
  print("htmMDStreatFeat...")
  # MDS
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  rCtrl = which(rownames(dTreatFeat)==negCtrl)
  dTreatFeat[rCtrl,] = 0
  
  d <- (dist(dTreatFeat)) # euclidean distances between the rows
  #print(d)
  
  fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
  
  pc = fit$points[negCtrl,1:2]  
  x <- fit$points[,1]
  y <- fit$points[,2]
  
  # center the negative control
  x <- x - pc[1]
  y <- y - pc[2]
  
  #print(fit)
  dMDS = dist(fit$points)
  dDiff = dMDS - d
  #print(d)
  #print(dDiff)
  # plot solution 
  #dev.new()
  
  #plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
  #     main="Metric  MDS",  type="n", asp=1)
  #text(x, y, labels = rownames(dTreatFeat), cex=.7)
  
  MDS = data.frame(treatment=rownames(dTreatFeat), x=x, y=y) 
  

  return(MDS)
  
}

htmMDStreatFeat2 <- function(d) {
  
  # MDS
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  #ids = which(grepl("Control",rownames(d)))
  #d <- d[ids,]
  
  dd <- sqrt(dist(d)) # euclidean distances between the rows
  
  fit <- cmdscale(dd, eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2]
  
  dMDS = dist(fit$points)
  dDiff = dMDS - dd
  
  lables = unlist(lapply(strsplit(rownames(d),"_"), function(x) {paste(x[1],"",sep="_")}))
  lables = rownames(d)
  
  
  dev.new()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
       main="Metric  MDS",  type="n", asp=1)
  text(x, y, labels = lables, cex=.7)
  
  MDS = data.frame(treatment=rownames(d), x=x, y=y) 
  
  return(MDS)
  
}

scaleForDisplay <- function(values,lut_min,lut_max){
  v = 255 * (values-as.numeric(lut_min) )/(as.numeric(lut_max)-as.numeric(lut_min) ) + 1
  toosmall = which(v<1)
  toolarge = which(v>255)
  v[toosmall]=1
  v[toolarge]=256
  return(v)
}

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

htmAddImageQC <- function(htm,.colname,.min,.max) {
  
  qc = htm@settings@qcImages
  
  if(qc[1,1]=="None selected") {
    htm@settings@qcImages <- data.frame(colname=.colname, min=.min, max=.max)
  } else {
    htm@settings@qcImages <- rbind(htm@settings@qcImages,data.frame(colname=.colname, min=.min, max=.max))
  }

  #print(paste("added image QC: colname =",.colname,"; min =",.min,"; max =",.max))
  return(htm)
  
}

htmGetImageQCs <- function(htm) {
  
  nImageQCs = nrow(htm@settings@qcImages)
  QCs = vector()
  for(i in 1:nImageQCs) {
    .colname = htm@settings@qcImages[i,1]
    .min =     htm@settings@qcImages[i,2]
    .max =     htm@settings@qcImages[i,3]
    QCs[length(QCs)+1]=paste(.colname,"  min=",.min,"  max=",.max,sep="")
  }
  return(QCs)
  
}

htmRemoveImageQCs <- function(htm, indices) { 
  
  htm@settings@qcImages <- htm@settings@qcImages[-indices,]
  if(nrow(htm@settings@qcImages)==0) {
    htm@settings@qcImages <- data.frame(colname="None selected", min=NA, max=NA)
  }

  #print(htm@settings@qcImages)
  return(htm)
  
}

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

htmPrintLog <- function(){
  htm <- get("htm", envir = globalenv()) 
  cat(htm@log)
}

htmAddLog <- function(text){
  htm <- get("htm", envir = globalenv()) 
  htm@log = paste(htm@log,text,sep="\n")
  assign("htm", htm, envir = globalenv())            
}

htmClearLog <- function(text){
  htm <- get("htm", envir = globalenv()) 
  htm@log = ""
  assign("htm", htm, envir = globalenv())            
}

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

htmApplyImageQCs <- function(htm) {
    
  print("Performing Image QC:")
  
  # get image QC dataframe from htm object
  data = htm@data
  qc = htm@settings@qcImages
  
  if(qc[1,1]=="None selected") {
    print("  No image QCs selected. Setting all images to valid.")
    htm@data$HTM_qcImages <- rep(1,nrow(htm@data)) # at this point something happens to the memory of htm... 
  } else {
    # compute QC and put results into htm
    passedQC = dataframeQC(data,qc)  
    # return the modified htm
    htm@data$HTM_qcImages <- passedQC # at this point something happens to the memory of htm...
  }  
  
  print("  (The column HTM_qcImages has been updated or generated.)")
  print("")
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

htmImageNormalization <- function(htm) {
    
  print("")
  print("Image normalization")
  print("*******************")
  print("")
  
  # get all necessary information
  measurement <- htmGetListSetting(htm,"statistics","measurement")
  experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
  experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  transformation <- htmGetListSetting(htm,"statistics","transformation")
  
  cat("\nMeasurement:\n")
  print(measurement)
  cat("\nNegative Control:\n")
  print(negcontrols)
  
  
  
  # check whether we know everything            
  if( is.null(experiments) || measurement=="None selected") {
    print("")
    print("  ERROR: cannot perform analysis due to lacking information (see above).")
    gmessage("Error: see R console output.")
    return(htm)
  }
  
  if( is.null(htm@data$HTM_qcImages) ) {
    print(" WARNING: there was no Image QC column; all images with non NA values will be set tovalid!")
    htm@data$HTM_qcImages = !is.na(htm@data[[measurement]])
  } 
      
  
  if(transformation == "log2") {
    # compute log transformation
    # create new column name
    logScoreName = paste(measurement,"log2",sep="__")
    htm@data[[logScoreName]] <- log2(htm@data[[measurement]]) 
    
    # todo: this should be at a more prominent position
    print("Replacing -Inf in log scores ******************************")
    logScores = htm@data[[logScoreName]]
    finiteLogScores = subset(logScores,is.finite(logScores))
    minimum = min(finiteLogScores)
    idsInf = which(is.infinite(logScores))
    logScores[idsInf] <- minimum
    htm@data[[logScoreName]] <- logScores
    
    #htmAddLog("Replacing Infinities in Log2 Score by")
    #htmAddLog(minimum)
    #htmAddLog("Affected Wells:")
    #for(id in idsInf) {
    #  htmAddLog(htm@wellSummary$treatment[id])
    #  htmAddLog(htm@wellSummary$wellQC[id])
    #  htmAddLog(htm@wellSummary[id,logScoreName])
    #  htmAddLog("")
    #}
  } # if log transformation
  
  
  # select log2 data in case data transformation is selected
  if(transformation == "log2") {
    measurement = logScoreName
  } else {
    measurement = measurement
  }
    
  measurement_minusMeanCtrl = paste(measurement,"minusMeanCtrl",sep="__")
  
  # initialisation
  htm@data[[measurement_minusMeanCtrl]] = NA
  
  # computation
  cat("\nComputing Image Normalisations...\n")
  
  for(experiment in experiments) {
   
    if(experiment %in% experiments_to_exclude) next
    
    print("")
    print(paste("  Experiment:",experiment))
    
    indices_all <- which((htm@data[[htm@settings@columns$experiment]] == experiment))
    indices_ok <- which((htm@data[[htm@settings@columns$experiment]] == experiment) & (htm@data$HTM_qcImages) & !is.nan(htm@data[[measurement]]))
    
    if("all treatments" %in% negcontrols) {
      indices_controls_ok <- indices_ok
    } else {
      indices_controls_ok <- which((htm@data[[htm@settings@columns$experiment]] == experiment) & (htm@data$HTM_qcImages) & !is.nan(htm@data[[measurement]]) & (htm@data[[htm@settings@columns$treatment]] %in% negcontrols))
    }
    
    print(paste("    Images Total", length(indices_all)))
    print(paste("    Images Valid", length(indices_ok)))      
    print(paste("    Images Valid Control", length(indices_controls_ok)))
    
    # here values are extracted 
    valuescontrol <- htm@data[indices_controls_ok, measurement]
    #print(valuescontrol)
    
    nr_of_controls <-  length(valuescontrol)
    meancontrol <- mean(valuescontrol)    
    sigmacontrol <- sd(valuescontrol) 
    mediancontrol <- median(valuescontrol)
    madcontrol <- mad(valuescontrol)  
    semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
    print(paste("    Control Mean:", meancontrol))
    print(paste("    Control SD:", sigmacontrol))
    print(paste("    Control Median:", mediancontrol))
    print(paste("    Control MAD:", madcontrol))
    
    htm@data[indices_all, measurement_minusMeanCtrl] <- ( htm@data[indices_all, measurement] - meancontrol )
    
    # t_test, image based
    
    
  } # experiment loop
  
   
  return(htm@data)
   
 }

htmObjectNormalization <- function(htm) {
  
  print("")
  print("Object normalization")
  print("********************")
  print("")
  
  
  data <- htm@objectdata
  
  # get all necessary information
  measurement <- htmGetListSetting(htm,"statistics","measurement")
  experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
  experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  transformation <- htmGetListSetting(htm,"statistics","transformation")
  
  cat("\nMeasurement:\n")
  print(measurement)
  cat("\nNegative Control:\n")
  print(negcontrols)
  
  # check whether we know everything            
  if( is.null(experiments) || measurement=="None selected") {
    print("")
    print("  ERROR: cannot perform analysis due to lacking information (see above).")
    gmessage("Error: see R console output.")
    return(htm)
  }
  
  
  
  #if( is.null(data$HTM_qcObjects) ) {
  #  print(" WARNING: there was no QC column; all objects with non NA values will be set to valid!")
  print("Performing quality control:")
  htm <- htmApplyImageQCs(htm)
  print("transferring image QC to the objects")
  htm <- htmGenerateImageObjectLink(htm)
  data$HTM_qcObjects = unlist(lapply(htm@objectdata$HTM_imageID,function(x) htm@data$HTM_qcImages[x]))
  data$HTM_qcObjects = data$HTM_qcObjects & (!is.nan(data[[measurement]]))
  print(paste("Total number of objects",length(data$HTM_qcObjects)))
  print(paste("Valid number of objects",sum(data$HTM_qcObjects)))
  
  
  #} else {
  #  data$HTM_qcObjects = (data$HTM_qcObjects) & (!is.nan(data[[measurement]]))
  #} 
  
  #
  htmNormName = "HTM_Norm"
  
  
  # remove previously computed columns
  idsRemove = which(grepl(htmNormName,colnames(data)))
  data[,idsRemove] <- NULL
  
  
  if(transformation == "log2") {
    # compute log transformation
    # create new column name
    logScoreName = paste(htmNormName,measurement,"log2",sep="__")
    data[[logScoreName]] <- log2(data[[measurement]]) 
    
    # todo: this should be at a more prominent position
    print("Replacing -Inf in log scores ******************************")
    logScores = data[[logScoreName]]
    finiteLogScores = subset(logScores,is.finite(logScores))
    minimum = min(finiteLogScores)
    idsInf = which(is.infinite(logScores))
    logScores[idsInf] <- minimum
    data[[logScoreName]] <- logScores
    
    #htmAddLog("Replacing Infinities in Log2 Score by")
    #htmAddLog(minimum)
    #htmAddLog("Affected Wells:")
    #for(id in idsInf) {
    #  htmAddLog(htm@wellSummary$treatment[id])
    #  htmAddLog(htm@wellSummary$wellQC[id])
    #  htmAddLog(htm@wellSummary[id,logScoreName])
    #  htmAddLog("")
    #}
  } # if log transformation
  
  
  # select log2 data in case data transformation is selected
  if(transformation == "log2") {
    measurement = logScoreName
  } else {
    measurement = measurement
  }
  
  measurement_minusMeanCtrl = paste(htmNormName,measurement,"minusMeanCtrl",sep="__")
  
  # initialisation
  data[[measurement_minusMeanCtrl]] = NA
  
  # computation
  cat("\nComputing normalisations...\n")
  
  for(experiment in experiments) {
    
    if(experiment %in% experiments_to_exclude) next
    
    print("")
    print(paste("  Experiment:",experiment))
    
    indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
    indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qcObjects))
    
    if("all treatments" %in% negcontrols) {
      indices_controls_ok <- indices_ok
    } else {
      indices_controls_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qcObjects) & (data[[htm@settings@columns$treatment]] %in% negcontrols))
    }
    
    print(paste("    Objects Total", length(indices_all)))
    print(paste("    Objects Valid", length(indices_ok)))      
    print(paste("    Objects Valid Control", length(indices_controls_ok)))
    
    # here values are extracted 
    valuescontrol <- data[indices_controls_ok, measurement]
    #print(valuescontrol)
    
    nr_of_controls <-  length(valuescontrol)
    meancontrol <- mean(valuescontrol)    
    sigmacontrol <- sd(valuescontrol) 
    mediancontrol <- median(valuescontrol)
    madcontrol <- mad(valuescontrol)  
    semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
    print(paste("    Control Mean:", meancontrol))
    print(paste("    Control SD:", sigmacontrol))
    print(paste("    Control Median:", mediancontrol))
    print(paste("    Control MAD:", madcontrol))
    
    data[indices_all, measurement_minusMeanCtrl] <- ( data[indices_all, measurement] - meancontrol )
    
    # t_test, image based
    
    
  } # experiment loop
  
  
  return(data)
  
}


#
# Generic data normalisation
#

htmNormalization <- function(htm) {
  
  print("*")
  print("* Data normalization")
  print("*" )
  print("")
  
  data <- htm@data
  
  # get all necessary information
  measurement <- htmGetListSetting(htm,"statistics","measurement")
  experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
  transformation <- htmGetListSetting(htm,"statistics","transformation")
  gradient_correction <- htmGetListSetting(htm,"statistics","gradient_correction")
  normalisation <- htmGetListSetting(htm,"statistics","normalisation")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  
  cat("\nMeasurement:\n")
  print(measurement)
  cat("\nNegative Control:\n")
  print(negcontrols)
  
  #
  # Check           
  #
  if(measurement=="None selected") {
    cat("\n\nError: please select a measurement!\n\n")
    return(htm)
  }
  if( ! (measurement %in% names(htm@data)) ) {
    cat(names(htm@data))
    cat("\nError: selected measurement is none of above data column names!\n")
    return(htm)
  }
  
  
  #
  # Analyze
  #
  print("Performing quality cntrol:")
  htm <- htmApplyQCs(htm)
  
  print(paste("Total data points",length(data$HTM_qc)))
  print(paste("Valid data points",sum(data$HTM_qc)))
  
  # init
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
  }
  
  return(data)
  
}



#
# Select a subset of the data
#
# ids <- htmSelectData(htm, treatments=unique(htm@data$Metadata_Well), measurement="HTM__z_score__projection", r=c(1,100), method="random", n=3)
# htmShowDataFromRow(htm,htm@data,ids)

htmSelectData <- function(htm, treatments, measurement, r=c(2,100), method="random", n=5, save_to_disc=FALSE) {
  
  print("*")
  print("* Data selection")
  print("*" )
  print("")
  
  data <- htm@data
  
  cat("\nMeasurement:\n")
  print(measurement)
  cat("\nTreatments:\n")
  print(treatments)
  
  htm <- htmApplyQCs(htm)
  
  print(paste("Total data points",length(data$HTM_qc)))
  print(paste("Valid data points",sum(data$HTM_qc)))
  
  ids_selected = vector()
  
  for(treatment in treatments) {
    
    ids <- which( (data[[htm@settings@columns$treatment]] == treatment) &
                  (data$HTM_qc==1) & 
                  (data[[measurement]] > r[1]) &
                  (data[[measurement]] < r[2]) )
  
    if(method == "random") {
      ids <- sample(ids, min(n,length(ids)))
    }
    
    ids_selected = c(ids_selected, ids)
    print(paste(treatment,"selected",length(ids)))
    
  }
  
  if(save_to_disc) {
    data_subset <- data[ids_selected,]
    saveTable(data_subset)
  }
  
  return(ids_selected)
  
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



htmMultiChannelFeatureNormalization <- function(htm) {
  
  print("")
  print("Feature normalization")
  print("**********************")
  print("")
  
  # get all necessary information
  #measurement <- htmGetListSetting(htm,"statistics","measurement")
  #experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
  #experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  #transformation <- htmGetListSetting(htm,"statistics","transformation")
  
  cat("\nNegative Control:\n")
  print(negcontrols)
  
  if( is.null(htm@data$HTM_qcImages) ) {
    print(" WARNING: there was no Image QC column; all images with non NA values will be set tovalid!")
    htm@data$HTM_qcImages = !is.na(htm@data[[measurement]])
  } 
  
  
  measurement_minusMeanCtrl = paste(measurement,"minusMeanCtrl",sep="__")
  
  # initialisation
  htm@data[[measurement_minusMeanCtrl]] = NA
  
  # computation
  cat("\nComputing Image Normalisations...\n")
  
  for(experiment in experiments) {
    
    if(experiment %in% experiments_to_exclude) next
    
    print("")
    print(paste("  Experiment:",experiment))
    
    indices_all <- which((htm@data[[htm@settings@columns$experiment]] == experiment))
    indices_ok <- which((htm@data[[htm@settings@columns$experiment]] == experiment) & (htm@data$HTM_qcImages) & !is.nan(htm@data[[measurement]]))
    
    if("all treatments" %in% negcontrols) {
      indices_controls_ok <- indices_ok
    } else {
      indices_controls_ok <- which((htm@data[[htm@settings@columns$experiment]] == experiment) & (htm@data$HTM_qcImages) & !is.nan(htm@data[[measurement]]) & (htm@data[[htm@settings@columns$treatment]] %in% negcontrols))
    }
    
    print(paste("    Images Total", length(indices_all)))
    print(paste("    Images Valid", length(indices_ok)))      
    print(paste("    Images Valid Control", length(indices_controls_ok)))
    
    # here values are extracted 
    valuescontrol <- htm@data[indices_controls_ok, measurement]
    #print(valuescontrol)
    
    nr_of_controls <-  length(valuescontrol)
    meancontrol <- mean(valuescontrol)    
    sigmacontrol <- sd(valuescontrol) 
    mediancontrol <- median(valuescontrol)
    madcontrol <- mad(valuescontrol)  
    semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
    print(paste("    Control Mean:", meancontrol))
    print(paste("    Control SD:", sigmacontrol))
    print(paste("    Control Median:", mediancontrol))
    print(paste("    Control MAD:", madcontrol))
    
    htm@data[indices_all, measurement_minusMeanCtrl] <- ( htm@data[indices_all, measurement] - meancontrol )
    
    # t_test, image based
    
    
  } # experiment loop
  
  
  return(htm@data)
  
}


htmWellSummary <- function(htm) {
      
    print("")
    print("Image score -> Well score:")
    print("**************************")
    print("")
    
    # get all necessary information
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    method <- htmGetListSetting(htm,"statistics","wellSummaryMethod")
    minNumValidImages <- 1 #htmGetListSetting(htm,"statistics","WellQC_Minimum_Number_Valid_Images")
    minNumObjects <- htmGetListSetting(htm,"statistics","WellQC_Minimum_Number_Objects")
    weighting <- htmGetListSetting(htm,"statistics","objectCount")
    colObjectCount <- htmGetListSetting(htm,"statistics","objectCount")
    experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    experiments_to_exculde <- htmGetVectorSettings("statistics$experiments_to_exclude")
    
    # output
    #print("");print("Experiments:")
    #print(experiments)
    print("");print("Well Summary Method:")
    print(method)
    print("");print("Measurement:")
    print(measurement)
    print("");print("Weighting with:")
    print(weighting)
    print("");print("Column containing the object count:")
    print(colObjectCount)
    
    
    print("");
    print(paste("Well QC: minimum number of valid images:",minNumValidImages))
    print(paste("Well QC: minimum number of objects",minNumObjects))
    
    
    if(htm@settings@columns$wellnum=="") {
      gmessage("Cannot proceed: you need to specify a Well Column!")
      return(htm)
    }
    
    # check whether we know everything            
    if( is.null(experiments) ||
          (measurement=="None selected") ||
          (method=="None selected") 
    ) {
      print("")
      print("  ERROR: cannot perform analysis due to lacking information (see above).")
      gmessage("Error: see R console output.")
      return(htm)
    }
    
    if( is.null(htm@data$HTM_qcImages) ) {
      print(" WARNING: there is no Image QC column; all images with non NA values will be considered valid!")
    } 
    
    ids_wells = split(1:nrow(htm@data), paste(htm@data[[htm@settings@columns$experiment]], htm@data[[htm@settings@columns$wellnum]]) )
    
    # removing "bad experiments" => why should this be here?
    
    #print(""); print("removing bad wells")
    #ids_to_exclude = vector()
    #for(i in 1:length(ids_wells)) {
    #  
    #  experiment = htm@data[ids_wells[[i]][1],htm@settings@columns$experiment]
    #  
    #  if( experiment %in% experiments_to_exculde ) {
    #    ids_to_exclude = append(ids_to_exclude,i)
    #  }
    #  
    #}
    #print(ids_to_exclude)
    #print(length(ids_wells))
    #if(length(ids_to_exclude)) {
    #  ids_wells = ids_wells[-ids_to_exclude]
    #}
    #print(length(ids_wells))
    
    
    numEntries = length(ids_wells) 
    
    results <- data.frame(wellNum=rep(NA,numEntries),
                          numObjectsOK=rep(NA,numEntries),
                          wellQC=rep(NA,numEntries),
                          numImages=rep(NA,numEntries),
                          numImagesOK=rep(NA,numEntries),
                          objectsPerImage=rep(NA,numEntries),
                          experiment=rep(NA,numEntries),
                          treatment=rep(NA,numEntries),
                          stringsAsFactors = FALSE)
    
    
    # init columns for the scores that are obtained by averaging the images in the well  
    
    # average
    scorename <- paste("wellscore",measurement,method,sep="__")
    results[[scorename]] <- rep(NA,numEntries)
    
    # average deviation
    if( method == "mean_of_images" ) {
      wellscoredevmethod <- "sd"
    } else if (method == "median_of_images") {              
      wellscoredevmethod <- "MAD"
    } else if (method== "weighted_mean_of_images") {
      wellscoredevmethod <- "NotSureYet"
    }
    print(method)
    #print(wellscoredevmethod)
    scoredevname <- paste("wellscore_deviation",measurement,method,wellscoredevmethod,sep="__")
    #print(scoredevname)
    results[[scoredevname]] <- rep(NA,numEntries)
    #print(colnames(results))
    
    # start computing the well average from the images 
    i = 0
    print("Analyzing.....")
    
    for(ids in ids_wells) {
      
      if( is.null(htm@data$HTM_qcImages) ) {
        idsOK = ids[which(!is.na(htm@data[ids,measurement]))]
      } else {
        idsOK = ids[ which( (htm@data$HTM_qcImages[ids]==1) & (!is.na(htm@data[ids,measurement])) ) ]
      }
      
      ## QC
      
      wellQC <- 1
      
      if( length(idsOK) < minNumValidImages ) wellQC <- 0
      
      if(colObjectCount != "None selected") {
        numObjectsOK <- sum(htm@data[idsOK,colObjectCount])
        if( is.na(numObjectsOK) || (numObjectsOK < minNumObjects) ) wellQC <- 0
      } else {
        numObjectsOK <- NA
      }
      
      
      i=i+1
      results$experiment[i] <- htm@data[ids[1],htm@settings@columns$experiment]
      results$treatment[i] <- htm@data[ids[1],htm@settings@columns$treatment]
      results$wellNum[i] <- htm@data[ids[1],htm@settings@columns$wellnum]
      results$numImagesOK[i] <- length(idsOK)
      results$numImages[i] <- length(ids)
      results$numObjectsOK[i] <- numObjectsOK
      results$objectsPerImage[i] <- numObjectsOK/length(idsOK)
      # print(scorename)
      # print(wellscore)
      
          
    if( method == "mean_of_images" ) {
      wellscore <- mean(htm@data[idsOK,measurement])
      wellscoredeviation <- sd(htm@data[idsOK,measurement])
    } else if (method == "median_of_images") {              
      wellscore <- median(htm@data[idsOK,measurement])
      wellscoredeviation <- mad(htm@data[idsOK,measurement])
    } else if (method== "weighted_mean_of_images") {
      wellscore <- sum( htm@data[idsOK,measurement]*htm@data[idsOK,weighting] ) / sum(htm@data[idsOK,weighting])
      wellscoredeviation <- NA
    } 
    
    results[i,scorename] <- wellscore
    results[i,scoredevname] <- wellscoredeviation
    
      
    if( is.na(wellscore)) wellQC <- 0
    results$wellQC[i] <- wellQC
                
    } # well loop
    
    measurement <- scorename
    
    
    # put wellscores into HTM object
    htm@wellSummary <- results
      
    print(paste("  Wells Total:",length(results$wellQC)))
    print(paste("  Wells OK:",sum(results$wellQC)))
    print(paste("  Images Total:",sum(results$numImages)))
    print(paste("  Images OK:",sum(results$numImagesOK)))
    
  
    htmAddLog("Well Normalisation:")
    htmAddLog("*******************")
    
    experiments <- unique(htm@data[[htm@settings@columns$experiment]])   
    #negcontrols <- htm@settings@ctrlsNeg
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    #normmethod <- htmGetListSetting(htm,"statistics","normalisationMethod")
    transformation <- htmGetListSetting(htm,"statistics","transformation")
     
    # output
    htmClearLog()
    htmAddLog("");htmAddLog("Experiments:")
    htmAddLog(experiments)
    htmAddLog("");htmAddLog("Negative controls:")
    htmAddLog(negcontrols)
    #htmAddLog("");htmAddLog("Well normalisation method:")
    #htmAddLog(normmethod)
    htmAddLog("");htmAddLog("Measurement:")
    htmAddLog(measurement);
    htmAddLog("");
    
    # check whether we know everything            
    if( is.null(experiments) ||
          (negcontrols=="None selected") ||
          #(normmethod=="None selected") || 
          (measurement=="None selected")
    ) 
    {
      print("")
      print("  ERROR: cannot perform analysis due to lacking information (see above).")
      gmessage("Error: see R console output.")
      
      print("")
      return(htm)
    }
    
    # todo:
    # replace below print functions with showinfo, which could be turned off
     
     
    # indicate the negative controls
    #htm@wellSummary$negCtrl <- rep(control,nrow(htm@wellSummary))
    htm@wellSummary$negCtrl <- rep(paste(negcontrols,collapse="--"),nrow(htm@wellSummary))
     
    if(transformation == "log2") {
      
      print("Computing log2 transformation")
      # compute log transformation
      logScoreName = paste("log2",measurement,sep="__")
      htm@wellSummary[[logScoreName]] <- log2(htm@wellSummary[[measurement]]) 
      
      # todo: this should be at a more prominent position
      logScores = htm@wellSummary[[logScoreName]]
      finiteLogScores = subset(logScores,is.finite(logScores))
      minimum = min(finiteLogScores)
      print(paste("  Replacing -Inf in log scores by ",minimum))
      
      idsInf = which(is.infinite(logScores))
      logScores[idsInf] <- minimum
      htm@wellSummary[[logScoreName]] <- logScores
      
      #htmAddLog("Replacing Infinities in Log2 Score by")
      #htmAddLog(minimum)
      #htmAddLog("Affected Wells:")
      #for(id in idsInf) {
      #  htmAddLog(htm@wellSummary$treatment[id])
      #  htmAddLog(htm@wellSummary$wellQC[id])
      #  htmAddLog(htm@wellSummary[id,logScoreName])
      #  htmAddLog("")
      #}
      
      measurement = logScoreName
      
    } # if log transformation
    

    if(htmGetListSetting(htm,"statistics","gradientCorrection") == "medpolish") {
      
      print("Apply medpolish")
     
      medpolish_name =  paste("medpolish",measurement,sep="__")
      
      htm@wellSummary[[medpolish_name]] = rep(NA,nrow(htm@wellSummary))
         
      for(experiment in experiments) {
      
        indices_all <- which((htm@wellSummary$experiment == experiment))
        
        # extract values
        htm@wellSummary[indices_all, measurement]
        xy = htm_convert_wellNum_to_xy(htm@wellSummary[indices_all, "wellNum"]) 
        print(length(xy$x))
        print(htm@wellSummary[indices_all, measurement])
        mp = htmMedpolish(x=xy$x, y=xy$y, val=htm@wellSummary[indices_all, measurement])
        print(mp$residuals)
        print(indices_all)
        htm@wellSummary[indices_all, medpolish_name] = mp$residuals
        
        } # experiment loop
    
        measurement = medpolish_name
          
    } #medpolish
    
  
    print("")
    print("Computing Normalisations...")
    
    zScoreName = paste("zScore",measurement,sep="__")
    robust_z_score_name = paste("robust_z_score",measurement,sep="__")
    minusMeanCtrlName = paste("minusMeanCtrl",measurement,sep="__")
    
    htm@wellSummary[[zScoreName]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[minusMeanCtrlName]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[robust_z_score_name]] <- rep(NA,nrow(htm@wellSummary))
    
     
    # init documentation of control values
    
    if(transformation == "log2") {
      ctrls_mean_name = "ctrls__log2__position_mean"
      ctrls_median_name = "ctrls__log2__position_median"
      ctrls_sd_name = "ctrls__log2__position_sd"
      ctrls_mad_name = "ctrls__log2__position_mad"        
    } else {
      ctrls_mean_name = "ctrls__position_mean"
      ctrls_median_name = "ctrls__position_median"
      ctrls_sd_name = "ctrls__position_sd"
      ctrls_mad_name = "ctrls__position_mad"      
    }
    
    htm@wellSummary[[ctrls_mean_name]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[ctrls_median_name]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[ctrls_sd_name]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[ctrls_mad_name]] <- rep(NA,nrow(htm@wellSummary))
    
  
    for(experiment in experiments) {
      print("")
      #print(paste("  Experiment:",experiment))
      
      indices_all <- (htm@wellSummary$experiment == experiment)
      indices_ok <- (htm@wellSummary$experiment == experiment) & (htm@wellSummary$wellQC)
      if("all treatments" %in% negcontrols) {
        indices_controls_ok <- indices_ok
      } else {
        #indices_controls_ok <- (htm@wellSummary$experiment == experiment) & (htm@wellSummary$treatment == control) & (htm@wellSummary$wellQC) 
        # combine different controls:
        indices_controls_ok <- (htm@wellSummary$experiment == experiment) & (htm@wellSummary$treatment %in% negcontrols) & (htm@wellSummary$wellQC)       
      }
      
      #print(paste("    Wells Total", sum(indices_all)))
      #print(paste("    Wells Valid", sum(indices_ok)))      
      #print(paste("    Wells Valid Control", sum(indices_controls_ok)))
      
      # here values are extracted 
      valuescontrol <- htm@wellSummary[indices_controls_ok, measurement]
      print(measurement)
      
      
      if(0) {
      if("all treatments" %in% negcontrols) {
        print("") 
      } else {
        print("    Valid Control Wells:")
        for (id in which(indices_controls_ok==T)) {
          print(paste("      WellNum:",htm@wellSummary$wellNum[id]))
          print(paste("        Treatment:",htm@wellSummary$treatment[id]))
          print(paste("        Value:",htm@wellSummary[[measurement]][id]))
          print(paste("        Objects_OK:",htm@wellSummary$numObjectsOK[id]))
          print(paste("        Images_OK:",htm@wellSummary$numImagesOK[id]))
        }
      }
      }
      
      
      nr_of_controls <-  length(valuescontrol)
      meancontrol <- mean(valuescontrol)    
      sigmacontrol <- sd(valuescontrol) 
      mediancontrol <- median(valuescontrol)
      madcontrol <- mad(valuescontrol)  
      semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
      print(paste("    Control Mean:", meancontrol))
      print(paste("    Control StdD:", sigmacontrol))
      print(paste("    Control Median:", mediancontrol))
      print(paste("    Control MAD:", madcontrol))
      
      htm@wellSummary[indices_all, ctrls_mean_name] <- meancontrol
      htm@wellSummary[indices_all, ctrls_sd_name] <- sigmacontrol
      htm@wellSummary[indices_all, ctrls_median_name] <- mediancontrol
      htm@wellSummary[indices_all, ctrls_mad_name] <- madcontrol
      
      # computed scores on a well level
      htm@wellSummary[indices_all, zScoreName] <- ( htm@wellSummary[indices_all, measurement] - meancontrol ) / ( sigmacontrol )
      htm@wellSummary[indices_all, robust_z_score_name] <- ( htm@wellSummary[indices_all, measurement] - mediancontrol ) / ( madcontrol )
      htm@wellSummary[indices_all, minusMeanCtrlName] <- ( htm@wellSummary[indices_all, measurement] - meancontrol )

      } # experiment loop
  
     
  
  # output
  print("");print("Experiments:")
  print(experiments)
  print("");print("Negative controls:")
  print(negcontrols)
  #print("");print("Well normalisation method:")
  #print(normmethod)
  print("");print("Measurement:")
  print(measurement)
 
  return(htm@wellSummary)
    
}



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
                        z_scores = rep(NA,numEntries),
                        median__z_scores = rep(NA,numEntries),
                        
                        t_test__estimate=rep(NA,numEntries),
                        t_test__p_value=rep(NA,numEntries),
                        t_test__signCode=rep(NA,numEntries),
                        
                        means = rep(NA,numEntries),
                        median__means = rep(NA,numEntries),
                        
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
    results$t_test__p_value[i] = t_test__p_value
    results$t_test__signCode[i] = t_test__signCode
    results$t_test__estimate[i] = t_test__estimate
    results$median__means[i] = median__means
    results$batches[i] = batches
    results$z_scores[i] = z_scores
    results$means[i] = means
    results$median__z_scores[i] = median__z_scores
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



# todo: combine this stuff
# put matrix generation into main function!

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




  
