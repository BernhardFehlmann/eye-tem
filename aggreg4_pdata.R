aggreg4_pdata <- function(alldirs, m.sub_filtered_aggr){
  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: aggregate all the pupil values over subjects ( + seperated aggregation per valence category), ONLY "GOOD" TRIALS CONSIDERED
  # input: loaded pdata_charac-files in long format
  # output: gr_meanProfiles_filtered: aggregated over all the subjects + all the trials (all valences + seperated per valence category), FILTERED
  ################################################################################ 
  
  library(corrplot)
  library (ggplot2)
  
  file_to_read = paste0('pdata_charac_long',"-",Sys.Date(),'.RData')
  load(paste0(rdir,"/",file_to_read))
  
  print(paste0("aggreg filtered pdata over subs"))
  
  which.val = c("all", "neg", "neu", "pos", "scr")
  
  # control, which data are aggregated with index of which.val[1-5]
  for (i in 1:5){
    gr_meanProfile_filtered <- data.frame ("height" = rep(NA, 150))
    charac_data_long <- get(paste0("charac_data_long_", which.val[i]))
    
    for (j in 1:150){
      os.mean.height <- m.sub_filtered_aggr(na.omit(charac_data_long[charac_data_long$timeframe == j, "height"]))
      gr_meanProfile_filtered$height[j] <- os.mean.height # fill vector with adjusted RowMeans
      os.mean.slope <- m.sub_filtered_aggr(na.omit(charac_data_long[charac_data_long$timeframe == j, "slope"]))
      gr_meanProfile_filtered$slope[j] <- os.mean.slope # fill vector with adjusted RowMeans
      os.mean.minheight <- m.sub_filtered_aggr(na.omit(charac_data_long[charac_data_long$timeframe == j, "minheight"]))
      gr_meanProfile_filtered$minheight[j] <- os.mean.minheight # fill vector with adjusted RowMeans
      os.mean.mintime <- m.sub_filtered_aggr(na.omit(charac_data_long[charac_data_long$timeframe == j, "mintime"]))
      gr_meanProfile_filtered$mintime[j] <- os.mean.mintime # fill vector with adjusted RowMeans
      os.mean.dilation_mean <- m.sub_filtered_aggr(na.omit(charac_data_long[charac_data_long$timeframe == j, "dilation_mean"]))
      gr_meanProfile_filtered$dilation_mean[j] <- os.mean.dilation_mean # fill vector with adjusted RowMeans
    } 
    gr_meanProfile_filtered$val = rep(which.val[i],150)
    gr_meanProfile_filtered$timeframe = c(1:150)
    assign(paste0("gr_meanProfile_filtered_", which.val[i]), gr_meanProfile_filtered)
  }
  
  
  return(list(gr_meanProfile_filtered_all, gr_meanProfile_filtered_neg, gr_meanProfile_filtered_neu, gr_meanProfile_filtered_pos, gr_meanProfile_filtered_scr))
  
} # end of function