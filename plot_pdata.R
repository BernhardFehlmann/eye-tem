plot_pdata <- function(alldirs){
 
  ################################################################################
  # Beni - 2016.11.01
  # Goal: plot the pupil data based on the subject level (aggregated over trials)
  # input: loaded pdata_charac-files in long format
  # output: different box- and density plots; attention: plot function automatically uses (normal) mean, might differ slightly from onestep means (see plot2-function)
  ################################################################################
  
  library(corrplot)
  library (ggplot2)
  
  
  file_to_read = paste0('pdata_charac_long',"-", Sys.Date(),'.RData')
  load(paste0(rdir,file_to_read))
  load(file.choose()) # choose file manually
  
  print(paste0("plotting on sub basis"))
  
  # choose which one to plot
  data1 = rbind (charac_data_long_neg, charac_data_long_neu, charac_data_long_pos, charac_data_long_scr) 
  #data1 = charac_data_long_all 
  
  
  # asses number of cases used
  

  ncases <- c(
  nrow(df.check <- na.omit(data1[data1$timeframe == 150 & data1$val == "neg",])),
  nrow(df.check <- na.omit(data1[data1$timeframe == 150 & data1$val == "neu",])),
  nrow(df.check <- na.omit(data1[data1$timeframe == 150 & data1$val == "pos",])),
  nrow(df.check <- na.omit(data1[data1$timeframe == 150 & data1$val == "scr",]))
  )

  df.cases <- data.frame (N = ncases, val = c("neg", "neu", "pos", "scr"))
  # chart_title <- c(df.cases$N) #paste0(df.cases$N)
  # chart_title
  ################################################################################
  # Plot pupil height per valence category
  ################################################################################
  # in the timecourse
  bf1 <- ggplot(data = na.omit(data1), aes(x = timeframe, y = height, color= val))                   
  bf1 <- bf1 + stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), geom = 'line') +
    ggtitle(paste("N", "\n", "neg = ", df.cases$N[1], "\n", "neu = ", df.cases$N[2], 
                       "\n", "pos = ", df.cases$N[3], "\n", "scr = ", df.cases$N[4]))

  ################################################################################
  # Plot slope per valence category
  ################################################################################
  bf2 <- ggplot(data = na.omit(data1), aes(x = timeframe, y = slope, color= val))                   
  bf2 <- bf2 + stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), geom = 'line')
  # bf2
  
  ################################################################################
  # Plot min height per valence category
  ################################################################################
  # boxplots
  bf3 <- ggplot(data = na.omit(data1), aes(factor(val), minheight))
  bf3 <- bf3 + geom_boxplot(outlier.color="darkgrey", aes(fill = factor(val)))
  # bf3
  
  # outlier identification
  if (length(subjects)>20){ # only if there are data for more than 20 subjects (otherwise instable results, under 3: error)
    data2 <- data1[data1$timeframe == 1 & is.na(data1$dilation_mean) == F,] # only look at one min-height-value per subject, exclude subs with NA in dilation mean (and therefore 0 for minheight)
    out_data2_neg <- outbox(data2[data2$val == "neg", "minheight"],mbox = T)
    out_data2_neu <- outbox(data2[data2$val == "neu","minheight"],mbox = T)
    out_data2_pos <- outbox(data2[data2$val == "pos", "minheight"],mbox = T)
    out_data2_scr <- outbox(data2[data2$val == "scr", "minheight"],mbox = T)
    out_data2_all <- rbind(data2[data2$val == "neg",][out_data2_neg$out.id,], data2[data2$val == "neu",][out_data2_neu$out.id,], 
                        data2[data2$val == "pos",][out_data2_pos$out.id,], data2[data2$val == "scr",][out_data2_scr$out.id,])
  }
  
  # density-plots
  bf4 <- ggplot(data = na.omit(data1), aes(x = minheight, color = val, fill = val))
  bf4 <- bf4 + geom_density(position = "identity", alpha = 0.0) +
        theme(legend.position = "bottom") +              
        guides(colour = "legend", fill = "none") 
  # bf4
  
  ###############################################################################
  # Plot last timepoint of minheight per valence category
  ################################################################################
  
  # boxplots
  bf5 <- ggplot(data = na.omit(data1), aes(factor(val), mintime))
  bf5 <- bf5 + geom_boxplot(outlier.color="darkgrey", aes(fill = factor(val)))
  # bf5
  
  # density-plots
  bf6 <- ggplot(data = na.omit(data1), aes(x = mintime, color = val, fill = val))
  bf6 <- bf6 + geom_density(position = "identity", alpha = 0.0) +
    theme(legend.position = "bottom") +              
    guides(colour = "legend", fill = "none") 
  # bf6
  
  ###############################################################################
  # Plot dilation mean time per valence category
  ################################################################################
  # boxplots
  bf7 <- ggplot(data = na.omit(data1), aes(factor(val), dilation_mean))
  bf7 <- bf7 + geom_boxplot(outlier.color="darkgrey", aes(fill = factor(val)))
  # bf7
  
  # density-plots
  bf8 <- ggplot(data = na.omit(data1), aes(x = dilation_mean, color = val, fill=val))
  bf8 <- bf8 +  geom_density(position = "identity", alpha=0.0)+
         theme(legend.position = "bottom") +              
         guides(colour = "legend", fill = "none")
  # bf8
  
  return(list(bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8))  
} # end of function 
