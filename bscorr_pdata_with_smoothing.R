bscorr_pdata_with_smoothing <- function(sub, alldirs, pdata_raw, m.baseline_aggr){  
  
  ################################################################################
  # Beni - 2016.10.21
  # exclude blinks, interpolate pupil height (exclusion if impossible), calculate baseline, correct pupil response during trial by baseline
  # input: pdata_raw (raw-files set together, with fixations detected)
  # QCs: 
    # file_to_read exists, not primacy-picture, at least one previous fixation timepoint detected, at least one fixation within trial detected
    # picure-trial equals exactly 150 timepoints, the height/width ratio is >.5 for at least 75% of the points,
    # delete the first and last 2 (?) time points of the fixation periods to avoid artifacts
    # over 50 timepoints within pic-trial (150 timepoints) that are "good-timepoints" (within fixations, good quality,..)-->used to interpolate the rest
  # output: pdata_bs_corr: pictures_summary_neg, pictures_summary_neu,pictures_summary_pos, pictures_summary_scr 
    # all with pupil information after blink detection, interpol + baseline substraction
  ################################################################################
  print(paste0("bscorr pdata: ", sub))
  
  # unpack pdata_raw-list
  data <- pdata_raw[[1]]
  fixations <- pdata_raw[[2]]
  primacy <- pdata_raw[[3]]

  ################################################################################
  # loop over pictures
  ################################################################################
  pictures_summary_neg = data.frame(matrix(nrow = 150, ncol = 24))
  pictures_summary_neu = data.frame(matrix(nrow = 150, ncol = 24))
  pictures_summary_pos = data.frame(matrix(nrow = 150, ncol = 24))
  pictures_summary_scr = data.frame(matrix(nrow = 150, ncol = 24))
  
  idx = data.frame(negative = 0, neutral = 0, positive = 0, scramble = 0)
  
  # pic = "negative_3102" # for test purpose
  for (pic in unique(data$picture_name)) {
    
    # initialize time marker vectors
    data$current_timePoints = NULL
    data$blinks = NULL
    
    if (pic != "primacy"){
      
      # get picture name
      im_str = strsplit(pic,"_")
      im_valence = im_str[[1]][1]
      im_id = im_str[[1]][2]
      
      # first sample of the picture presentation
      start_trial = data[data$picture_name == pic & data$event == "picture", "time"][1] # starting point of picture presentation
      end_trial = tail(data[data$picture_name == pic & data$event =="ITI" & data$time > start_trial, "time"], n = 1) # end point of picture presentation
      
      # previous 500ms of cross fixation (only good quality trials) - for baseline purposes
      previous_fixation_timePoints = data$event == "ITI" & data$time < start_trial & data$time > (start_trial-.5) & data$quality == 1 # index of good-quality timepoints in the window 500ms before pic-presentation
      previous_fixation = subset(fixations, start < start_trial & start > (start_trial-5) & end <= end_trial & trial == "ITI") # fixations 5s before pic-presentation
      
      ################################################################################
      # QC-steps
      # QC: check if previous_timepoints were detected (only then, baseline will be available)
      if (sum(as.numeric(previous_fixation_timePoints) > 0)){
        # only consider the first 2500ms of the 'picture' event, as there are 3500ms of 'picture' event after the 
        # 1.parse_data.sh command, with the difference corresponding to a black screen between end of picture and 
        # presentation of the valence scale
        current_timePoints = data$picture_name == pic       
        picture_timePoints = data$picture_name == pic & data$event == 'picture' & data$time <= (start_trial+2.5) # picturetimepoints: only look at rows of current picture
        data[current_timePoints, "current_timePoints"] = 1 #...set index to 1 for those
        
        # identify the different fixation periods
        # take into account fixations that might overlap between trials (before and after)
        before_picture_fixations = subset(fixations, start < start_trial & end > start_trial & end <= end_trial & trial == "ITI")
        picture_fixations = subset(fixations, start >= start_trial & start < (start_trial+2.5) & end <= end_trial & trial == "picture")
        blank_fixations = subset(fixations, start >= (start_trial+2.5) & start <= (start_trial+3.5) & end <= end_trial & trial == "picture")
        valence_fixations = subset(fixations, start >= start_trial & end <= end_trial & trial == "valence")
        arousal_fixations = subset(fixations, start>=start_trial & end <= end_trial & trial == "arousal")
        iti_fixations = subset(fixations, start >= start_trial & end <= end_trial & trial == "ITI")
        after_picture_fixations = subset(fixations, start >= start_trial & start < end_trial & end > end_trial & trial == "ITI")
        within_trial_fixations= rbind(picture_fixations,blank_fixations, valence_fixations,arousal_fixations,iti_fixations, after_picture_fixations) # all fixations STARTING within the trial
        
        
        
        # QC: we have all data points for the pic-event; correct the pupil height by substracting calculated baseline
        if (length(data[picture_timePoints,"time"]) == 150) { # equals 2.5s
          
          
          # QC: the height/width ratio is >.5 for at least 75% of the points
          data[picture_timePoints, "pupilRatio"] = data[picture_timePoints, "height"]/data[picture_timePoints, "width"] # calculate pupil ratio within one picture
          if (sum(data$pupilRatio > .5, na.rm = T) >= 75){
            fixations_starting_before = before_picture_fixations[,"end"] # fixations starting before the trialstart and end within trial--> return endpoint
            fixations_start = within_trial_fixations[,"start"] # starting point of all fixations startin within a trial
            fixations_end =  within_trial_fixations[,"end"] # endpoint ""
            
            # QC: delete the first and last 2 (?) time points of the 
            # fixation periods to avoid artifacts
            fixations_starting_before = fixations_starting_before - 2*0.016
            for (fix in 1:length(fixations_start)) {
              if (fixations_end[[fix]] - fixations_start[[fix]] > 5*0.016){ # make sure that there are enough time points to delete 4 of them -->avB: at least 5
                fixations_start[[fix]] = fixations_start[[fix]] + 2*0.016 # shift starting point two timeframes ahead
                fixations_end[[fix]] = fixations_end[[fix]] - 2*0.016 # shift endpoint two timeframes to the back (less confounding through blinks)
              } 
            }
            
            # case where there was a fixation that started before the onset of the picture
            if (length(fixations_starting_before) > 0){
              tmp = data$picture_name == pic & data$time <= fixations_starting_before[[1]] & data$time >= start_trial & data$quality == 1 # only consider timepoints of current pic, timepoints after trialstart and before ENDPOINT of fixation starting before trial
              data$good_timePoints[tmp] = 1
            }
            
            
            # fixations during picture presentation
            if (length(fixations_start) > 0) {
              for (fix in 1:length(fixations_start)) {
                # good_timePoints = fixation periods
                tmp = data$picture_name == pic & data$time >= fixations_start[[fix]] & data$time <= fixations_end[[fix]] & data$time <= end_trial & data$quality==1  #nur Zeitpunkte auswaehlen des gewaehlten Pictures anschauen, nur Zeitpunkte zwischen dem Start und Ende der fixation und vor dem Ende des Trials (avB ev. automatsch durch Definition von fixation_start)
                data$good_timePoints[tmp]=1
                
              }
            }    
            
            # for the whole trial: define blinks
            # based on a pupil ratio criteria<0.6 (following user manual's recommendation)
            tmp =  data$picture_name == pic & data$event == 'picture' & data$time <= (start_trial+2.5) & data$pupilRatio < 0.6
            data$blinks[tmp] = 1
            data$good_timePoints[tmp] = NA # exlude blinks from good timeopoints again
            
            
            # linear interpolation
            df1_picture = subset(data, picture_timePoints) # only look at the true picture presentation 
            df1_picture$height_before_interp = df1_picture[df1_picture$good_timePoints == 1, "height"] 
            
            
            
            #############################################################################################################
            ################################################ ev. smooth here ############################################
            #############################################################################################################
            
            window.size = c(3,5,7,9,11)
            # smooth data with recursive median filter
            for (w in window.size){
              print(paste0("smoothing with windowsize ", w, " of ",window.size[length(window.size)]))
              k = (w-1)/2
              
              tobe_smoothed = df1_picture$height_before_interp
                if (length(na.omit(tobe_smoothed)) > 0){ # don't smooth if all the data are NA (not enough timepoints for interpolation in bscorr_script)
                  
                  # testdata to see if filter works  
                  # between<- rep(c(1,5,1,10,100,2,2,20,20,20,60,9,1,4,3),10)  
                  # start<-(rep(1,k))
                  # end <- (rep(3,k))
                  
                  between <- tobe_smoothed # look at values of ith element of dataframe
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
                  NonNAindex <- which(!is.na(tobe_smoothed))
                  firstNonNA <- min(NonNAindex)
                  lastNonNA <- max(NonNAindex)
                  
                  tobe_smoothed[which(!is.na(tobe_smoothed))] <- total_vector$update2[(k+1):(nrow(total_vector)-k)] # return filtered data to original dataframe, skip NAs at the beginning and the end 
                } 
              }
           df1_picture$height_before_interp_smoothed <- tobe_smoothed # update dataframe to smoothed version
           df1_picture$height_before_interp = df1_picture$height_before_interp_smoothed #!!!!!!!!!!!!!!only for trial purposes
            #############################################################################################################
            ################################################ end smoothing #############################################
            #############################################################################################################
            
            
            
            
       
            
            
            if (sum(df1_picture$good_timePoints,na.rm = T) < 50){
              print(paste0("picture ", pic,": not enough time points for interpolation")); df1_picture$height_after_interp = NA # exclude values from further processing
            } else if (sum(df1_picture$good_timePoints,na.rm = T) == 150){
              print(paste0("picture ", pic,": no need for interpolation")); df1_picture$height_after_interp = df1_picture$height_before_interp
            } else {
              tmp = approx(x = df1_picture$time, y = df1_picture$height_before_interp, xout = df1_picture$time) # linear interpolation based on pupil-height of godd timepoints (only during fixations): tails won't be interpolated
              df1_picture$height_after_interp = tmp$y # update df1_picture-dataframe
            # plot(df1_picture$height_after_interp) # for visual quality check
            }
          
            
            # baseline correction
            if (length(fixations_start) > 0) {
              df1_picture$height_blcorr = df1_picture$height_after_interp - m.baseline_aggr(data[previous_fixation_timePoints, 'height']) # substract the mean (onestep-method) of the baseline (qc above: there are good quality baseline measures)
              idx[1, im_valence] = idx[1, im_valence]+1
              
              
              # separate based on valence
              if (im_valence == "negative"){
                pictures_summary_neg[,idx[1, im_valence]] = df1_picture$height_blcorr
                colnames(pictures_summary_neg)[idx[1, im_valence]] = pic
              } else if (im_valence == "neutral"){
                pictures_summary_neu[,idx[1, im_valence]] = df1_picture$height_blcorr
                colnames(pictures_summary_neu)[idx[1, im_valence]] = pic
              } else if (im_valence == "positive"){
                pictures_summary_pos[,idx[1, im_valence]] = df1_picture$height_blcorr
                colnames(pictures_summary_pos)[idx[1, im_valence]] = pic
              } else if (im_valence == "scramble"){
                pictures_summary_scr[,idx[1, im_valence]] = df1_picture$height_blcorr
                colnames(pictures_summary_scr)[idx[1, im_valence]] = pic
              }
            }
          }else{print("not enough good pupil data")} # end QC pupil ratio
        }else{print("picture trial is not 150 timepoints long")}
      }else{print("no previous fixation timepoints found")}  
    } # end "if pic != primary-loop
  } # end of picture loop --> pictures_summary for every picture (not aggregated); idx counting number of pictures per valence category
  pdata_bscorr <- list(pictures_summary_neg, pictures_summary_neu,pictures_summary_pos, pictures_summary_scr) # pupil height for every picture for one subject, later updated to smoothed version
  return(pdata_bscorr)
} # end function
