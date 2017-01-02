bscorr_pdata_adv_bs <- function(sub, alldirs, pdata_raw, m.baseline_aggr){  
  
  ################################################################################
  # Beni - 2016.10.21
  # exclude blinks, interpolate pupil height (exclusion if impossible), calculate baseline, correct pupil response during trial by baseline
  # input: pdata_raw (raw-files set together, with fixations detected)
  # QCs: 
    # file_to_read exists, not primacy-picture, at least one previous fixation timepoint detected, at least one fixation within trial detected
    # picure-trial equals exactly 150 timepoints, the height/width ratio is >.5 for at least 75% of the points,
    # delete the first and last 2 (?) time points of the fixation periods to avoid artifacts
    # over 50 timepoints within pic-trial (150 timepoints) that are "good-timepoints" (within fixations, good quality,..)-->used to inerpolate the rest
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
  # pic = "positive_5830" # for test purpose
  
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
        within_trial_fixations = rbind(picture_fixations,blank_fixations, valence_fixations,arousal_fixations,iti_fixations, after_picture_fixations) # all fixations STARTING within the trial
        
        # QC: there are at least two fixations within the pic_trial detects (just one: probably eye-tracker error)
        if (nrow(within_trial_fixations) > 1) {
        
          # QC: we have all data points for the pic-event; correct the pupil height by substracting calculated baseline
          if (length(data[picture_timePoints,"time"]) == 150) { # equals 2.5s
            
            
            # QC: the height/width ratio is > 0.5 for at least 50% of the points
            data[, "pupilRatio"] = data[, "height"]/data[, "width"] # calculate pupil ratio FOR the WHOLE DATA
            if (sum(data[picture_timePoints, "pupilRatio"] > .5, na.rm = T) >= 75){ # check within the current picture
              fixations_starting_before = before_picture_fixations[,"end"] # fixations starting before the trialstart and end within trial--> return endpoint
              fixations_start = within_trial_fixations[,"start"] # starting point of all fixations startin within a trial
              fixations_end =  within_trial_fixations[,"end"] # endpoint ""
              
              # QC: delete the first and last 2 (?) time points of the 
              # fixation periods to avoid artifacts
              fixations_starting_before = fixations_starting_before - 2*0.016
              if (length(fixations_start) > 0){ 
                for (fix in 1:length(fixations_start)) {
                  if (fixations_end[[fix]] - fixations_start[[fix]] > 5*0.016){ # make sure that there are enough time points to delete 4 of them -->avB: at least 5
                    fixations_start[[fix]] = fixations_start[[fix]] + 2*0.016 # shift starting point two timeframes ahead
                    fixations_end[[fix]] = fixations_end[[fix]] - 2*0.016 # shift endpoint two timeframes to the back (less confounding through blinks)
                  } 
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
              
              
              if (sum(df1_picture$good_timePoints,na.rm = T) < 50){
                print(paste0("picture ", pic,": not enough time points for interpolation")); df1_picture$height_after_interp = NA # exclude values from further processing
              } else if (sum(df1_picture$good_timePoints,na.rm = T) == 150){
                print(paste0("picture ", pic,": no need for interpolation")); df1_picture$height_after_interp = df1_picture$height_before_interp
              } else {
                tmp = approx(x = df1_picture$time, y = df1_picture$height_before_interp, xout = df1_picture$time) # linear interpolation based on pupil-height of godd timepoints (only during fixations): tails won't be interpolated
                df1_picture$height_after_interp = tmp$y # update df1_picture-dataframe
               # plot(df1_picture$height_after_interp) # for visual quality check
              }
            
            }else{print(paste0("picture ", pic,": not enough good pupil ratio data"))} # end QC pupil ratio
          }else{print(paste0("picture ", pic,": picture trial is not 150 timepoints long"))}
        }else{print(paste0("picture ", pic,": no previous fixation timepoints found"))}  
      } else{print(paste0("picture ", pic,": maximum 1 fixation per trial detected"))} # end QC number of fixations within pic-presentation  

      
########################################################################################################################################################
########################################################################################################################################################
#advanced baseline calculation

      baseline_timePoints= data$time >= (start_trial-0.5) & data$time <= (start_trial)
      
      
      # identify the different fixation periods
      # take into account fixations that might overlap between trials (before and after)
      before_baseline_fixations = subset(fixations, start < start_trial & start >= (start_trial-5) & end >= (start_trial-0.5) & trial == "ITI")
      baseline_fixations = subset(fixations, start < start_trial & start >= (start_trial-0.5) & end <= start_trial & trial == "ITI")
      after_baseline_fixations = subset(fixations, start < start_trial & start >= (start_trial-0.5) & end > start_trial & trial == "ITI")
      within_baseline_fixations = rbind(baseline_fixations, after_baseline_fixations) # all fixations STARTING within the baseline
      
      # QC: there are at least two fixations within the baseline detected (just one: probably eye-tracker error)
      if (nrow(within_baseline_fixations) + nrow(before_baseline_fixations) > 0) {
      
        # QC: we have all data points for the baseline-event; correct the pupil height by substracting calculated baseline
        if (length(data[baseline_timePoints,"time"]) == 30) { # equals 2.5s
      
      
          # QC: the height/width ratio is > 0.5 for at least 50% of the points
          if (sum(data[baseline_timePoints, "pupilRatio"] > .5, na.rm = T) >= 15){ # basline: 30 timepoints overall
            bs_fixations_starting_before = before_baseline_fixations[,"end"] # fixations starting before the trialstart and end within trial--> return endpoint
            bs_fixations_start = within_baseline_fixations[,"start"] # starting point of all fixations starting within baseline
            bs_fixations_end =  within_baseline_fixations[,"end"] # endpoint ""
      
      
            # QC: delete the first and last 2 (?) time points of the
            # fixation periods to avoid artifacts
      
      
            bs_fixations_starting_before = bs_fixations_starting_before - 2*0.016
            if (length(bs_fixations_start) > 0){ 
              for (fix in 1:length(bs_fixations_start)) {
                if (bs_fixations_end[[fix]] - bs_fixations_start[[fix]] > 5*0.016){ # make sure that there are enough time points to delete 4 of them -->avB: at least 5
                  bs_fixations_start[[fix]] = bs_fixations_start[[fix]] + 2*0.016 # shift starting point two timeframes ahead
                  bs_fixations_end[[fix]] = bs_fixations_end[[fix]] - 2*0.016 # shift endpoint two timeframes to the back (less confounding through blinks)
                }
              }
            }
            # case where there was a fixation that started before the onset of the baseline
            if (length(bs_fixations_starting_before) > 0){
              bs_tmp = data$time <= bs_fixations_starting_before[[1]] & data$time >= (start_trial-0.5) & data$quality == 1 # only consider timepoints of current pic, timepoints after trialstart and before ENDPOINT of fixation starting before trial
              data$good_bs_timePoints[bs_tmp] = 1
            }
      
      
            # fixations during baseline 
            if (length(bs_fixations_start) > 0) {
              for (fix in 1:length(bs_fixations_start)) {
                # good_timePoints = fixation periods
                bs_tmp = data$time >= bs_fixations_start[[fix]] & data$time <= fixations_end[[fix]] & data$time <= (start_trial-0.5) & data$quality==1  #nur Zeitpunkte auswaehlen des gewaehlten Pictures anschauen, nur Zeitpunkte zwischen dem Start und Ende der fixation und vor dem Ende des Trials (avB ev. automatsch durch Definition von fixation_start)
                data$good_bs_timePoints[bs_tmp] = 1
              }
            }
      
            # for the whole trial: define blinks
            # based on a pupil ratio criteria < 0.6 (following user manual's recommendation)
            bs_tmp =  data$pupilRatio < 0.6
            data$bs_blinks[bs_tmp] = 1
            data$good_bs_timePoints[bs_tmp] = NA # exlude blinks from good timeopoints again
      
      
            # linear interpolation
            df2_baseline = subset(data, baseline_timePoints) # only look at the true picture presentation
            df2_baseline$height_before_interp = df2_baseline[df2_baseline$good_bs_timePoints == 1, "height"]
      
      
            if (sum(df2_baseline$good_bs_timePoints,na.rm = T) < 10){
              print(paste0("baseline ", pic,": not enough time points for interpolation")); df2_baseline$height_after_interp = NA # exclude values from further processing
            } else if (sum(df2_baseline$good_timePoints,na.rm = T) == 30){
              print(paste0("baseline ", pic,": no need for interpolation")); df2_baseline$height_after_interp = df2_baseline$height_before_interp
            } else {
              tmp = approx(x = df2_baseline$time, y = df2_baseline$height_before_interp, xout = df2_baseline$time) # linear interpolation based on pupil-height of good timepoints (only during fixations): tails won't be interpolated
              df2_baseline$height_after_interp = tmp$y # update df2_baseline-dataframe
              # plot(df2_baseline$height_after_interp) # for visual quality check
            }
      
            
            ########################################################################################################################################################
            ########################################################################################################################################################
            # end of advanced baseline calculation         
            
            # baselines
            
            baseline_advanced = m.baseline_aggr(na.omit(df2_baseline[, 'height_after_interp']))
            baseline_oldschool = m.baseline_aggr(na.omit(data[previous_fixation_timePoints, 'height']))

            
            # baseline correction
            if (length(fixations_start) > 0) {
              if (is.na (baseline_advanced)){
              df1_picture$height_blcorr = df1_picture$height_after_interp - baseline_oldschool # substract the mean (onestep-method) of the baseline (qc above: there are good quality baseline measures)
              print(paste0(pic, ": no adv baseline calculation possible -> old one is used"))
              }else{df1_picture$height_blcorr = df1_picture$height_after_interp - baseline_advanced}
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
            
            
          }else{print(paste0("baseline ", pic,": not enough good pupil ratio data"))} # end QC pupil ratio
        }else{print(paste0("baseline ", pic,": baseline is not 30 timepoints long"))}
      } else{print(paste0("baseline ", pic,": no fixations within baseline detected"))} # end QC number of fixations within baseline 
      
    } # end "if pic != primary-loop
  } # end of picture loop --> pictures_summary for every picture (not aggregated); idx counting number of pictures per valence category
  
  pdata_bscorr <- list(pictures_summary_neg, pictures_summary_neu,pictures_summary_pos, pictures_summary_scr) # pupil height for every picture for one subject, later updated to smoothed version
  return(pdata_bscorr)
} # end function