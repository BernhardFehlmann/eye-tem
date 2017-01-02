plot2_pdata <- function(alldirs,gr_meanProfiles_filtered_smooth){
  ################################################################################
  # Beni - 2016.11.01
  # Goal: plot the pupil data based overall level (aggregated over subjects)
  # input: gr_meanProfiles_filtered_smooth files (already aggregated over subjects)
  # output: different box- and density plots; attention: due to aggregation, box+ density plots don't show SD anymore, but onestep mean might be informative nontheless
  ################################################################################
  
  library(corrplot)
  library (ggplot2)
  
  print(paste0("plotting on overall basis"))
  
  # choose which one to plot
  data1 = rbind (as.data.frame(gr_meanProfiles_filtered_smooth[2]), as.data.frame(gr_meanProfiles_filtered_smooth[3]), 
                 as.data.frame(gr_meanProfiles_filtered_smooth[4]), as.data.frame(gr_meanProfiles_filtered_smooth[5]))
  # data1 = as.data.frame(gr_meanProfiles_filtered_smooth[1]
  
  
  ################################################################################
  # Plot pupil height per valence category
  ################################################################################
  
  # in the timecourse
  bf1 <- ggplot(data = na.omit(data1), aes(x = timeframe, y = height, color= val))                   
  bf1 <- bf1 + stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), geom = 'line')
  

  ################################################################################
  # Plot slope per valence category
  ################################################################################
  
  bf2 <- ggplot(data = na.omit(data1), aes(x = timeframe, y = slope, color= val))                   
  bf2 <- bf2 + stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), geom = 'line')
  
  
  ################################################################################
  # Plot min height per valence category
  ################################################################################

  # boxplots
  bf3 <- ggplot(data = na.omit(data1), aes(factor(val), minheight))
  bf3 <- bf3 + geom_boxplot(outlier.color="darkgrey", aes(fill = factor(val)))
  
  
  ################################################################################
  # Plot last timepoint of min height per valence category
  ################################################################################
  
  # boxplots
  bf4 <- ggplot(data = na.omit(data1), aes(factor(val), mintime))
  bf4 <- bf4 + geom_boxplot(outlier.color="darkgrey", aes(fill = factor(val)))
  
  ###############################################################################
  # Plot dilation mean time per valence category
  ################################################################################
  
  # boxplots
  bf5 <- ggplot(data = na.omit(data1), aes(factor(val), dilation_mean))
  bf5 <- bf5 + geom_boxplot(outlier.color="darkgrey", aes(fill = factor(val)))
  
  return(list(bf1, bf2, bf3, bf4, bf5))
  
} # end of function 
