aggreg3_pdata <- function(sub, alldirs, pdata_smooth, trialfilter, m.trial_filtered_aggr, ntrial.aggregate_all, ntrial.aggregate_val){  
  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: aggregate all the trial values per subject (+ seperated aggregation per valence category) AFTER FILTERING
  # input: pictures_summary_all_smoothed
  # output: meanProfiles per sub filtered: for all the pictures together + per valence category
  ################################################################################
 
  file_to_read = paste0(rdir,"output_per_sub/picsum_smooth/",'pdata_smooth_',sub,"-",Sys.Date(),'.RData') # read picsum_smooth file for each subject
  
  if (file.exists(file_to_read)){
    
    print(paste0("aggreg filtered pdata within sub: ", sub))
    
    # unpack gr_meanProfiles_smooth-list
    print(sub, quote = F) 
    load(file = file_to_read)
    pdata_smooth <- pdata_smooth[[1]]
  
    
    # add filter before calculating medians
    rownames(trialfilter) <- trialfilter$rownames
    excluding.vector <- trialfilter[,which (colnames(trialfilter) == sub)]
    which(excluding.vector == T) # which elements of excluding vector must be filtered out
    which.cols_excl <- rownames(trialfilter [which(excluding.vector == T),]) # with which picture-names do these elements correspond
    which.cols_incl <- rownames(trialfilter [which(excluding.vector == F),]) # names of pictures to keep
  
  
  
  ################################################################################
    #calculate mean Profiles for each subject
    
    meanProfile_filtered_all_per_sub <- vector(mode = "numeric", length = 150) # initialzise empty vetctor with slots for each of the 150 rows
    for (i in 1:150){
      if (length(which(colnames(pdata_smooth) %in% c(which.cols_incl) == T)) >= ntrial.aggregate_all){ # at least specified amount of good trials needed to do aggregation; see MAIN
      os.mean <- m.trial_filtered_aggr(na.omit(unlist(pdata_smooth [i,!colnames(pdata_smooth) %in% c(which.cols_excl)]))) # only consider not exluded columns, non-NAs
      meanProfile_filtered_all_per_sub[i] <- os.mean
      }else{meanProfile_filtered_all_per_sub[i] = NA}
    }
    
    meanProfile_filtered_neg_per_sub <- vector(mode = "numeric", length = 150) # initialzise empty vetctor with slots for each of the 150 rows
    for (i in 1:150){
      if (length(which(colnames(pdata_smooth[,grepl("neg", names(pdata_smooth))]) %in% c(which.cols_incl) == T)) >= ntrial.aggregate_val){ # at least specified amount of good trials (out of 24) needed to do aggregation; see MAIN
      os.mean <- m.trial_filtered_aggr(na.omit(unlist(pdata_smooth [i,!colnames(pdata_smooth) %in% c(which.cols_excl) & grepl("neg", names(pdata_smooth))]))) # only consider not exluded columns + columnes with neg in their colname
      meanProfile_filtered_neg_per_sub[i] <- os.mean
      }else{meanProfile_filtered_neg_per_sub[i] = NA}
    }
    
    meanProfile_filtered_neu_per_sub <- vector(mode = "numeric", length = 150) # initialzise empty vetctor with slots for each of the 150 rows
    for (i in 1:150){
      if (length(which(colnames(pdata_smooth[,grepl("neu", names(pdata_smooth))]) %in% c(which.cols_incl) == T)) >= ntrial.aggregate_val){ # at least specified amount of good trials (out of 24) needed to do aggregation; see MAIN
        os.mean <- m.trial_filtered_aggr(na.omit(unlist(pdata_smooth [i,!colnames(pdata_smooth) %in% c(which.cols_excl) & grepl("neu", names(pdata_smooth))]))) # only consider not exluded columns + columnes with neu in their colname
        meanProfile_filtered_neu_per_sub[i] <- os.mean
      }else{meanProfile_filtered_neu_per_sub[i] = NA}
    }
    
    meanProfile_filtered_pos_per_sub <- vector(mode = "numeric", length = 150) # initialzise empty vetctor with slots for each of the 150 rows
    for (i in 1:150){
      if (length(which(colnames(pdata_smooth[,grepl("pos", names(pdata_smooth))]) %in% c(which.cols_incl) == T)) >= ntrial.aggregate_val){ # at least specified amount of good trials (out of 24) needed to do aggregation; see MAIN
        os.mean <- m.trial_filtered_aggr(na.omit(unlist(pdata_smooth [i,!colnames(pdata_smooth) %in% c(which.cols_excl) & grepl("pos", names(pdata_smooth))]))) # only consider not exluded columns + columnes with pos in their colname
        meanProfile_filtered_pos_per_sub[i] <- os.mean
      }else{meanProfile_filtered_pos_per_sub[i] = NA}
    }
    
    meanProfile_filtered_scr_per_sub <- vector(mode = "numeric", length = 150) # initialzise empty vetctor with slots for each of the 150 rows
    for (i in 1:150){
      if (length(which(colnames(pdata_smooth[,grepl("scr", names(pdata_smooth))]) %in% c(which.cols_incl) == T)) >= ntrial.aggregate_val){ # at least specified amount of good trials (out of 24) needed to do aggregation; see MAIN
        os.mean <- m.trial_filtered_aggr(na.omit(unlist(pdata_smooth [i,!colnames(pdata_smooth) %in% c(which.cols_excl) & grepl("scr", names(pdata_smooth))]))) # only consider not exluded columns + columnes with scr in their colname
        meanProfile_filtered_scr_per_sub[i] <- os.mean
      }else{meanProfile_filtered_scr_per_sub[i] = NA}
    }
    
    return(list(meanProfile_filtered_all_per_sub, meanProfile_filtered_neg_per_sub, meanProfile_filtered_neu_per_sub, meanProfile_filtered_pos_per_sub, meanProfile_filtered_scr_per_sub))
  } # end if file exists loop
} # end function