aggreg_pdata <- function(sub, alldirs, pdata_smooth, m.trial_aggr){  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: aggregate all the trial values per subject (+separated aggregation per valence category)
  # input: pictures_summary_all_smoothed
  # output: meanProfiles per sub: for all the pictures together + per valence category
  ################################################################################
  
  print(paste0("aggreg pdata within sub: ", sub))
  
  # unpack pdata_smooth-list
  pictures_summary_all_smoothed = pdata_smooth[[1]]

  ################################################################################
  #calculate mean Profiles for each subject
  meanProfile_all_per_sub <- vector(mode = "numeric", length = 150) # initialzise empty vetctor with slots for each of the 150 rows
  for (i in 1:150){
    os.mean <- m.trial_aggr(na.omit(unlist(pictures_summary_all_smoothed [i,])))
    meanProfile_all_per_sub[i] <- os.mean # fill vector with adjusted RowMeans
  }
  
  meanProfile_neg_per_sub <- vector(mode = "numeric", length = 150)
  for (i in 1:150){
    if (length(unlist(pictures_summary_all_smoothed [i,grepl("neg", names(pictures_summary_all_smoothed))])) > 0){ # preventing aggregation if pictures_summary_all_smoothed has no "neg" pictures
      os.mean <- m.trial_aggr(na.omit(unlist(pictures_summary_all_smoothed [i,grepl("neg", names(pictures_summary_all_smoothed))])))
      meanProfile_neg_per_sub[i] <- os.mean
    }
  }
  
  meanProfile_neu_per_sub <- vector(mode="numeric", length=150)
  for (i in 1:150){
    if (length(unlist(pictures_summary_all_smoothed [i,grepl("neu", names(pictures_summary_all_smoothed))])) > 0){
      os.mean <- m.trial_aggr(na.omit(unlist(pictures_summary_all_smoothed [i,grepl("neu", names(pictures_summary_all_smoothed))])))
      meanProfile_neu_per_sub[i] <- os.mean
    }
  }
  
  meanProfile_pos_per_sub <- vector(mode="numeric", length=150)
  for (i in 1:150){
    if (length(unlist(pictures_summary_all_smoothed [i,grepl("pos", names(pictures_summary_all_smoothed))])) > 0){
      os.mean <- m.trial_aggr(na.omit(unlist(pictures_summary_all_smoothed [i,grepl("pos", names(pictures_summary_all_smoothed))])))
      meanProfile_pos_per_sub[i] <- os.mean
    }
  } 
  
  meanProfile_scr_per_sub <- vector(mode="numeric", length=150)
  for (i in 1:150){
    if (length(unlist(pictures_summary_all_smoothed [i,grepl("scr", names(pictures_summary_all_smoothed))])) > 0){
      os.mean <- m.trial_aggr(na.omit(unlist(pictures_summary_all_smoothed [i,grepl("scr", names(pictures_summary_all_smoothed))])))
      meanProfile_scr_per_sub[i] <- os.mean
    }
  } 
  
  return(list(meanProfile_all_per_sub, meanProfile_neg_per_sub, meanProfile_neu_per_sub, meanProfile_pos_per_sub, meanProfile_scr_per_sub))
}# end function