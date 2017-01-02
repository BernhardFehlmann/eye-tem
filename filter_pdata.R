filter_pdata <- function(sub, alldirs, pdata_trial_corr, corr.threshold){  
  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: create a filter on the single trial level based on a specified threshold (see MAIN): exclude = 1, include = 0
  # input: gr_meanProfiles, smoothed pupil data for each trial + each subject
  # output: correlation for each trial + each subject
  ################################################################################
  
  print(paste0("create filter: threshold = ", corr.threshold))
  
  exclude_trials <- pdata_trial_corr
  for (i in 2:ncol(exclude_trials)){
    for (j in 1:nrow(exclude_trials)){
      
      if (exclude_trials[j,i] < corr.threshold|is.na(exclude_trials[j,i])){
        exclude_trials[j,i] <- T
      }else{exclude_trials[j,i] <- F}
    }
  }
  return (exclude_trials)
} # end function