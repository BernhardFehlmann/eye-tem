trialcorr_pdata <- function(sub, alldirs, gr_meanProfiles_smooth) {  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: correlate each trial of each subject with the meanProfile
  # input: gr_meanProfiles, smoothed pupil data for each trial + each subject
  # output: correlation for each trial + each subject
  ################################################################################
  
  file_to_read = paste0(rdir,"output_per_sub/picsum_smooth/",'pdata_smooth_',sub,"-",Sys.Date(),'.RData') # read picsum_smooth file for each subject
  
  if (file.exists(file_to_read)){
    
    print(paste0("trialcorr pdata: ", sub))
      
    # unpack gr_meanProfiles_smooth-list
    gr_meanProfile_all <- gr_meanProfiles_smooth[[1]] 
    load(file = file_to_read)
    pdata_smooth <- pdata_smooth[[1]]
    
    
    # correlations
    rownames <- sort(c("negative_6350",   "scramble_Folie38",  "negative_9571",    "positive_8190",    "scramble_Folie12", "neutral_2516",    "positive_2540",    "neutral_7004",      "positive_5001",   
                       "scramble_Folie35", "positive_2050",    "negative_3102",    "neutral_4605",     "scramble_Folie19", "neutral_2210",     "negative_3266",    "neutral_neu11",    "negative_2141",   
                       "negative_9252",    "scramble_Folie43", "positive_1440",    "scramble_Folie11", "neutral_neu10",    "positive_2208",    "scramble_Folie47", "negative_2750",    "neutral_7050",    
                       "positive_4680",    "scramble_Folie13", "negative_3530",    "neutral_2385",     "positive_2304",    "neutral_7034",     "positive_5270",    "scramble_Folie36", "scramble_Folie14",
                       "negative_1050",    "negative_9181",    "neutral_neu12",    "positive_2057",    "positive_1340",    "neutral_neu15",    "negative_6020",    "neutral_2381",     "scramble_Folie8", 
                       "scramble_Folie16", "positive_1750",    "negative_9340",    "positive_4610",    "negative_9040",    "neutral_2850",     "negative_3015",    "neutral_7002",     "positive_8210",   
                       "scramble_Folie25", "scramble_Folie33", "neutral_4000",     "scramble_Folie4",  "positive_7260",    "negative_3010",    "neutral_5520",     "scramble_Folie48", "negative_2722",   
                       "positive_8510",    "negative_3180",    "scramble_Folie1",  "positive_2340",    "neutral_2230",     "scramble_Folie24", "positive_5780",    "negative_9800",    "neutral_2570",    
                       "neutral_7590",     "neutral_2575",     "positive_2550",    "negative_6370",    "scramble_Folie31", "scramble_Folie41", "negative_9920",    "positive_5600",    "scramble_Folie3", 
                       "positive_2150",     "negative_9470",   "neutral_neu8",     "scramble_Folie10", "neutral_neu14",    "positive_5830",    "negative_9050",    "positive_1500",    "scramble_Folie27",
                       "scramble_Folie39", "negative_9530",    "positive_8370",    "neutral_neu7",     "negative_6200",    "neutral_neu13"))
    
    
    out = as.data.frame(cor(pdata_smooth, gr_meanProfile_all, use = "pairwise.complete.obs"))  # exlude NAs 
    colnames(out) = sub
    
    if (exists("pdata_trial_corr")) {
      pdata_trial_corr <<- merge(pdata_trial_corr,out, by.x= "rownames", by.y = 0, all.x = T)  # write correlation from out-dataframe into the according rows of pdata_trial_corr-dataframe
    }else{
      pdata_trial_corr <- data.frame(rownames = character(96),stringsAsFactors = FALSE)
      pdata_trial_corr$rownames <- rownames
      pdata_trial_corr <<- merge(pdata_trial_corr,out, by.x= "rownames", by.y = 0, all.x = T)
    }  
  } # end if file exists-loop
}# end function


