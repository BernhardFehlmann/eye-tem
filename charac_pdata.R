charac_pdata <- function (sub, alldirs, meanProfiles_filtered_sub_smooth, max.tp_ampl, min.tp_ampl, max.tp_dilation, min.tp_dilation){
  
  ################################################################################
  # Beni - 2016.11.01
  # Goal: add slope, min pupil height and the mean (onestep) of the dilation period (everything after minimum)
  # input: meanProfiles_filtered_sub_smooth: mean Profiles per subject, filtered and smoothed
  # output: charac data (all + separated per valence category): dataframes per sub with added information
  #         charac data over all subjects in long format: directly passed to global environment
  ################################################################################
  
  print(paste0("charac pdata: ", sub))
  
  # loop over valence category
  which_val <- c( "all", "neg", "pos", "neu", "scr")
  
  for (val in which_val) {
    
    if (val == "all"){
      data <- as.data.frame (meanProfiles_filtered_sub_smooth[[1]])
    }
    
    if (val == "neg"){
      data <- as.data.frame (meanProfiles_filtered_sub_smooth[[2]])
    }
    
    if (val == "pos"){
      data <- as.data.frame (meanProfiles_filtered_sub_smooth[[3]])
    }
    
    if (val == "neu"){
      data <- as.data.frame (meanProfiles_filtered_sub_smooth[[4]])
    }
    
    if (val == "scr"){
      data <- as.data.frame (meanProfiles_filtered_sub_smooth[[5]])
    }

  colnames (data) = "height"
  data$timeframe = c(1:150)
  
  # slope
  slope <- c(0,(diff(data$height)))   # add slopeinformation (+ add a zero for first timepoint)
  
  # minimum height
  minheight <- min(data[data$timeframe <= max.tp_ampl & data$timeframe >= min.tp_ampl,"height"]) # minimal height within chosen time window (ev. smooth data first??)    
  qc.1 <- data[data$height == minheight & data$timeframe <= max.tp_ampl & data$timeframe >= min.tp_ampl,"timeframe"] # return timepoint, at which the pupil height is minimal WITHIN SELECTED TIMERANGE
  
  if (length(qc.1) != 1){ # if several minima -->...
    print(paste0 ("attention: several minima detected; n= ", length(qc.1)))
    print ("last one taken for analyses")
    qc.1 <- tail(qc.1[length(qc.1)]) # ...take the LAST minimum
  }
  
  # Last timepoint of minimum height
  mintime <- qc.1
  
  # mean for dilation period, based on detected last minimum within specified timeframe
  dilation_mean <- m.dilation_mean(data[data$timeframe <= max.tp_dilation & data$timeframe >= qc.1,"height"])
  
  assign(paste0("charac_data_",val), cbind (data, slope, minheight, mintime, dilation_mean, sub, val)) # create a new dataframe with additional infos for each valence category (+ for all)
  
  # concatenate files over subjects into long format
  if (exists(paste0("charac_data_long_",val))){
    assign(paste0("charac_data_long_",val), rbind(get(paste0("charac_data_long_",val)),get(paste0("charac_data_",val))), envir = .GlobalEnv) # return longformatted file to the global environment (so that it's constantly updated)
    }else{assign(paste0("charac_data_long_",val), get(paste0("charac_data_",val)), envir = .GlobalEnv)
     # return longformatted file to the global environment (so that it's constantly updated)
    }
  
  } # end of valence loop 
  
  return(list(charac_data_all, charac_data_neg, charac_data_neu, charac_data_pos, charac_data_scr))
} # end of function
