smooth_pdata <- function(sub, alldirs, pdata_bscorr, window.size){  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: smooth the pupil data during picture trial; window sizes of recursive median filtering:vgl. MAIN-script
  # input: pdata_bscorr, containing pictures_summary_neg, pictures_summary_neu,pictures_summary_pos, pictures_summary_scr
  # output: pictures_summary_all_smoothed
  ################################################################################
  
  print(paste0("smooth pdata: ", sub))
  
  # unpack pdata_bscorr-list
  pictures_summary_neg = pdata_bscorr[[1]]
  pictures_summary_neu = pdata_bscorr[[2]]
  pictures_summary_pos = pdata_bscorr[[3]]
  pictures_summary_scr = pdata_bscorr[[4]]
  
  pictures_summary_all_raw <- cbind(pictures_summary_neg, pictures_summary_neu, pictures_summary_pos, pictures_summary_scr)
  pictures_summary_all <- cbind(pictures_summary_neg, pictures_summary_neu, pictures_summary_pos, pictures_summary_scr)

  ################################################################################
  # smooth data with recursive median filter
  for (w in window.size){
    print(paste0("smoothing with windowsize ", w, " of ",window.size[length(window.size)]))
    k = (w-1)/2
    
    for (i in 1:ncol(pictures_summary_all)){
      if (length(na.omit(pictures_summary_all[,i])) > 0){ # don't smooth if all the data are NA (not enough timepoints for interpolation in bscorr_script)
      
        # testdata to see if filter works  
        # between<- rep(c(1,5,1,10,100,2,2,20,20,20,60,9,1,4,3),10)  
        # start<-(rep(1,k))
        # end <- (rep(3,k))
        
        between <- as.vector(pictures_summary_all[,i]) # look at values of ith element of dataframe
        between <- between[!is.na(between)] # remove NAs (possible at the beginning and the end of column due to impossible interpolation)
        start <- rep(head(between, n = 1),k) # repeat the first element of the vector (no NAs) k times
        end <- rep(tail(between, n = 1),k) # repeat the last element of the vector (no NAs) k times
        
        total_vector <- as.data.frame(c(start, between, end))
        colnames (total_vector) <- "prior"
        total_vector$update1 <- total_vector$prior
        total_vector$update2 <- rep(0,nrow(total_vector))
        
        for (j in (k+1):(nrow(total_vector)-k)){ # repeat median calculation for every row (exept boarders)
          total_vector$update2[j] <- median(c(total_vector$update1[(j-1):(j-k)], total_vector$prior[(j):(j+k)])) # recursive median filter
          total_vector$update1[j] <- total_vector$update2[j]
        } 
        NonNAindex <- which(!is.na(pictures_summary_all[,i]))
        firstNonNA <- min(NonNAindex)
        lastNonNA <- max(NonNAindex)
        
        pictures_summary_all[firstNonNA:lastNonNA,i] <- total_vector$update2[(k+1):(nrow(total_vector)-k)] # return filtered data to original dataframe, skip NAs at the beginning and the end 
      } 
    }
  }
  pictures_summary_all_smoothed <- pictures_summary_all # update dataframe to smoothed version
  rm(pictures_summary_all)
  return(list(pictures_summary_all_smoothed,pictures_summary_all_raw))
} # end function