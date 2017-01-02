extract_pdata <- function(sub, alldirs){  
  
################################################################################
# Beni - 2016.10.21
# extract data from raw files, bind primacy and main trials together, detect fixations
# input: data (ALL.txt + primacy.txt file per subject)
# output: pdata_raw file with all raw data (data, fixations, primacy)
################################################################################

  file_to_read = paste(ddir,sub,'ALL.txt', sep='/')
  print(paste0("extract pdata: ", sub))
  
  # read data
  data = read.table(file_to_read, header=T)
  data$trial = data$event
  
  ################################################################################
  # primacy
  primacy = read.table(paste0(ddir, sub,"/primacy.txt"), header=T)
  colnames(primacy) = c("quality", "time", "delta", "x", "y", "width", "height", "fixation")
  primacy$event="ITI"
  primacy$trial = primacy$event
  primacy$picture_name="primacy"
  
  ################################################################################
  # concatenate data frames
  data = rbind(primacy, data)
  
  ################################################################################
  # detect saccades & fixations
  fixations = detect.fixations(data)
  rownames(fixations) = NULL
  
  ################################################################################
  # delete fixations that are shorter than 100ms
  fixations = subset(fixations, dur >= 0.100)
  pdata_raw <- list(data, fixations, primacy)
  return(pdata_raw)
} # end function

  