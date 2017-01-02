aggreg2_pdata <- function(sub, alldirs, mean_Profiles_sub_smooth, m.sub_aggr){  
  
  ################################################################################
  # Beni - 2016.10.21
  # Goal: aggregate all the pupil values over subjects (+seperated aggregation per valence category)
  # input: mean_Profiles_sub_smooth: meanProfiles per sub: for all the pictures together + per valence category
  # output: gr_meanProfiles: aggregated over all the subjects + all the trials (all valences + seperated per valence category)
  ################################################################################
  
  print(paste0("aggreg pdata over subs: ", sub))
  
  #unpack meanProfiles_sub_smooth-list
  meanProfile_all_per_sub = meanProfiles_sub_smooth[[1]]
  meanProfile_neg_per_sub = meanProfiles_sub_smooth[[2]]
  meanProfile_neu_per_sub = meanProfiles_sub_smooth[[3]]
  meanProfile_pos_per_sub = meanProfiles_sub_smooth[[4]]
  meanProfile_scr_per_sub = meanProfiles_sub_smooth[[5]]
  
  
  ################################################################################
  # calculate grand-mean Profiles over all subjects
  
  
  #-------------------------------------------------------------------------------
  # all valences together

  if (exists("meanProfile_all")){
    meanProfile_all_per_sub <- as.data.frame(meanProfile_all_per_sub)
    colnames(meanProfile_all_per_sub) <- sub
    meanProfile_all <<- cbind(meanProfile_all,meanProfile_all_per_sub) # return object to global environment to make sure it is found next time while entering the loop
  }else{meanProfile_all <<- as.data.frame(meanProfile_all_per_sub) 
  colnames(meanProfile_all) <<- sub
  }
  
  gr_meanProfile_all =  vector(mode="numeric", length=150) # initialzise empty vetctor with slots for each of the 150 rows
  for (i in 1:150){
    os.mean <- m.sub_aggr(na.omit(unlist(meanProfile_all[i,])))
    gr_meanProfile_all[i] <- os.mean # fill vector with adjusted RowMeans
  } 
  
  #-------------------------------------------------------------------------------
  # negative
  
  if (exists("meanProfile_neg")){
    meanProfile_neg_per_sub <- as.data.frame(meanProfile_neg_per_sub)
    colnames(meanProfile_neg_per_sub) <- sub
    meanProfile_neg <<- cbind(meanProfile_neg,meanProfile_neg_per_sub)
  }else{meanProfile_neg <<- as.data.frame(meanProfile_neg_per_sub) 
  colnames(meanProfile_neg) <<- sub
  }
  
  gr_meanProfile_neg =  vector(mode = "numeric", length=150)
  for (i in 1:150){
    os.mean <- m.sub_aggr(na.omit(unlist(meanProfile_neg[i,])))
    gr_meanProfile_neg[i] <- os.mean
  } 
  
  #-------------------------------------------------------------------------------
  # neutral
  
  if (exists("meanProfile_neu")){
    meanProfile_neu_per_sub <- as.data.frame(meanProfile_neu_per_sub)
    colnames(meanProfile_neu_per_sub) <- sub
    meanProfile_neu <<- cbind(meanProfile_neu,meanProfile_neu_per_sub)
  }else{meanProfile_neu <<- as.data.frame(meanProfile_neu_per_sub) 
  colnames(meanProfile_neu) <<- sub
  }
  
  gr_meanProfile_neu =  vector(mode = "numeric", length=150)
  for (i in 1:150){
    os.mean <- m.sub_aggr(na.omit(unlist(meanProfile_neu[i,])))
    gr_meanProfile_neu[i] <- os.mean
  } 
  
  #-------------------------------------------------------------------------------
  # positive
  
  if (exists("meanProfile_pos")){
    meanProfile_pos_per_sub <- as.data.frame(meanProfile_pos_per_sub)
    colnames(meanProfile_pos_per_sub) <- sub
    meanProfile_pos <<- cbind(meanProfile_pos,meanProfile_pos_per_sub)
  }else{meanProfile_pos <<- as.data.frame(meanProfile_pos_per_sub) 
  colnames(meanProfile_pos) <<- sub
  }
  
  gr_meanProfile_pos =  vector(mode = "numeric", length=150)
  for (i in 1:150){
    os.mean <- m.sub_aggr(na.omit(unlist(meanProfile_pos[i,])))
    gr_meanProfile_pos[i] <- os.mean
  } 
  
  
  #-------------------------------------------------------------------------------
  # scrambled
  
  if (exists("meanProfile_scr")){
    meanProfile_scr_per_sub <- as.data.frame(meanProfile_scr_per_sub)
    colnames(meanProfile_scr_per_sub) <- sub
    meanProfile_scr <<- cbind(meanProfile_scr,meanProfile_scr_per_sub)
  }else{meanProfile_scr <<- as.data.frame(meanProfile_scr_per_sub) 
  colnames(meanProfile_scr) <<- sub
  }
  
  gr_meanProfile_scr =  vector(mode = "numeric", length=150)
  for (i in 1:150){
    os.mean <- m.sub_aggr(na.omit(unlist(meanProfile_scr[i,])))
    gr_meanProfile_scr[i] <- os.mean
  } 
  
  return(list(gr_meanProfile_all, gr_meanProfile_neg, gr_meanProfile_neu, gr_meanProfile_pos, gr_meanProfile_scr))
}# end function