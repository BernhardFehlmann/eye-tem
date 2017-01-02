Added Text from patch 1
################################################################################

# Main script to get to clean Pupil profiles overall, per valence category, subject, trials
# 1.0 extract pupil data from raw files 
# 1.1 exclude blinks, interpolate pupil height (exclusion if impossible), correct pupil height by baseline
# 1.2 smooth the data with recursive median filter
# 1.3 aggregate pupil values over all trials + seprated per valence category
# 1.4 aggregate pupil values over subjects--> mean Profile as a template for good trials (aggregated over subjects+trials)
# 1.5 correlate each trial to the mean Profile
# 1.6 exclude trials below a certain threshold
# 1.7 aggregate pupil measures over "good" trials
# 1.8 extract additional information: minimal height, slope, mean during dilation period, min height time,....
# 1.9 plot the data (per subject)  # attention: plotting uses normal mean etc. (not onestep)--> picture might differ slightly from plotting of over-sub-aggregated data!!
# 2.0 aggregate "good" pupil measures over subjects
# 2.1 plot the data (over subjects)
# 2.2 plotting_aid write plots to pdf and save them

# 8. ev. exclude whole subjects based on whole subject correl. with meanProfile
# Beni - 2016.10.21

#################################################################################

#clean workspace
rm(list = ls())

# load all the required packages
library(gtools)
require(saccades)
require(ggplot2)
require(reshape)
require(gridExtra) # for saving two ggplots together
require(tidyr) # short-to-long format manipulations
require(pspline) # to compute time series derivatives
require(robfilter) # robust filtering package

##################################################################################
# set directories

bindir = '~/Desktop/Eye-tem/Preprocessing/3.pupil_profile/r_skripts/'
ddir = '~/Desktop/Eye-tem/Preprocessing/0.data/import_data/import-2015.10.26/' 
rdir = '~/Desktop/Eye-tem/Preprocessing/3.pupil_profile/r_output/'
alldirs = c(bindir, ddir, rdir)
setwd(bindir)

##################################################################################

# source the scripts

source(paste0(bindir,"extract_pdata.R"))
source(paste0(bindir, "bscorr_pdata.R"))
source(paste0(bindir, "bscorr_pdata_with_smoothing.R"))
source(paste0(bindir, "bscorr_pdata_adv_bs.R"))
source(paste0(bindir, "smooth_pdata.R"))
source(paste0(bindir, "aggreg_pdata.R"))
source(paste0(bindir, "aggreg2_pdata.R"))
source(paste0(bindir, "trialcorr_pdata.R"))
source(paste0(bindir, "filter_pdata.R"))
source(paste0(bindir, "aggreg3_pdata.R"))
source(paste0(bindir, "charac_pdata.R"))
source(paste0(bindir, "plot_pdata.R"))
source(paste0(bindir, "aggreg4_pdata.R"))
source(paste0(bindir, "plot2_pdata.R"))
source(paste0(bindir, "plotting_aid.R"))
source("~/Desktop/Eye-tem/Documents/Rallfun-v31.txt")# robust-statistic-functions of Rand Wilcox

#################################################################################
# define the variables

subjects = 1472:2541

# 1.1
m.baseline_aggr = onestep # method for baseline aggregation (f.e onestep, mean, median, mom)
nbaseline_timepoints = 8 # minimal amount of timepoints to allow baseline aggregation (500 ms --> max. 30 good quality timepoints, 6 = 20 %)
# 1.2
window.size = c(3,5,7,9,11) # window size for recursive median filtering (vgl. Kliemann 2012)
# 1.3
m.trial_aggr = onestep # method for trial aggregation (f.e onestep, mean, median, mom)
# 1.4
m.sub_aggr = onestep # method for subject aggregation (f.e onestep, mean, median, mom)
reference = "new" # (old: take already existing meanprofile reference. Default: the one calculated on 04_12_2016 
                  # (if f.e only one subject is investigated for outlier detection, that leads to more stable results); new: take the aggregated pupil profile of this Rscript_run)
# 1.6
corr.threshold = 0.90
# 1.7
m.trial_filtered_aggr = onestep # method for trial aggregation after filtering (f.e onestep, mean, median, mom)
ntrial.aggregate_all = 24  # how many good trials (out of 96) per subject are necessary to allow aggregation 
ntrial.aggregate_val = 6 # how many good trials per subject per valence category (out of 24) are necessary to allow aggregation 
# 1.8
max.tp_ampl = 100 # set time window for minimum height calculation--> how short is dilation period allowed to be at max (150-x)
min.tp_ampl = 30 # set time window for minimum height calculation
max.tp_dilation = 150 # set time window for mean calculation (f.e during dilation phase)
min.tp_dilation = 80 # not used currently because min is set based on amplitude minimum
m.dilation_mean = onestep # method to calculate mean pupil height during dilation period (f.e onestep, mean, median, mom)
# 2.0
m.sub_filtered_aggr = onestep # method for trial aggregation after filtering (f.e onestep, mean, median, mom)

##################################################################################
# files to save
                        # default #
savefile_1.0 = T        # F # pdata_raw: extracted pupil data from raw files
savefile_1.1 = T        # F # pdata_bscorr: interpolated, baseline corrected pupil data of pic-trial 
savefile_1.2 = T        # T # pdata_smooth:smoothed data --> loaded in trialcorr-skript # 1.5
savefile_1.3 = T        # F # meanProfiles_sub_smooth: aggregated pupil values over all trials + separated per valence category
savefile_1.4 = T        # F # gr_meanProfiles_smooth: aggregated pupil values over subjects
savefile_1.5 = T        # F # pdata_trial_corr: each trial correlated to the mean Profile
savefile_1.6 = T        # F # trialfilter: excluded trials based on threshold
savefile_1.7 = T        # F # meanProfiles_filtered_sub_smooth: aggregated pupil measures over "good" trials
savefile_1.8 = T        # F # pdata_charac_sub: additional information: minimal height, slope, mean during dilation period,...
savefile_1.8_long = T   # T # pdata_charac_long: 1.8 in long format --> loaded in function # 1.9 and # 2.0
savefile_1.9 = T        # T # pdata_plot: plots based on subject basis
savefile_2.0 = T        # F # gr_meanProfiles_filtered_smooth: aggregate "good" pupil measures over subjects
savefile_2.1 = T        # T # pdata2_plot: plots, aggregated over the subjects

################################################################################
# create log file

sink(paste(rdir, 'PUPIL_MAIN_logfile.txt', sep = '/'), append = F)
print(paste0("Date:", Sys.Date()))
print(paste0("subjects: ", head(subjects, n = 1),":", tail(subjects, n = 1)))
print(paste0("nbaseline_timepoints = ", nbaseline_timepoints))
print(paste0("window.size = ",paste(window.size, collapse = ' ')))
print(paste0("reference = ",reference))
print(paste0("corr.threshold = ",corr.threshold))
print(paste0("ntrial.aggregate_all = ",ntrial.aggregate_all))
print(paste0("ntrial.aggregate_val = ",ntrial.aggregate_val))
print(paste0("max.tp_ampl = ",max.tp_ampl))
print(paste0("min.tp_ampl = ",min.tp_ampl))
print(paste0("max.tp_dilation = ",max.tp_dilation))
print(paste0("min.tp_dilation = ",min.tp_dilation))
sink()

################################################################################
# create sub-variable: all subject-numbers with data
subjects_exist = c()
for (sub_prov in subjects){
  file_to_read = paste(ddir,sub_prov,'ALL.txt', sep='/')
  if (file.exists(file_to_read)){
    subjects_exist = c(subjects_exist, sub_prov)
  }
}

# sub = subjects # for skipping the sub-loops
# loop over subjects
for (sub in subjects_exist){

  #-------------------------------------------------------------------------------
  # 1.0 extract pupil data from raw files
  
  pdata_raw <- extract_pdata(sub, alldirs)
  # save raw data per subject 
  if (savefile_1.0 == T){
    file_to_save = paste0('pdata_raw_',sub,"-",Sys.Date(),'.RData')
    save(pdata_raw, file = paste0(rdir,"output_per_sub/picsum_raw/",file_to_save))
  }
  #-------------------------------------------------------------------------------
  # 1.1 exclude blinks, interpolate pupil height (exclusion if impossible), correct pupil height by baseline
  
    pdata_bscorr <- bscorr_pdata(sub, alldirs, pdata_raw, m.baseline_aggr, nbaseline_timepoints)
  # pdata_bscorr <- bscorr_pdata_with_smoothing (sub, alldirs, pdata_raw, m.baseline_aggr) # alternative with smoothing
  # pdata_bscorr <- bscorr_pdata_adv_bs (sub, alldirs, pdata_raw, m.baseline_aggr) # alternative with advanced baseline calculation
  # save interpolated data per subject 
  if (savefile_1.1 == T){
    file_to_save = paste0('pdata_bscorr_',sub,"-",Sys.Date(),'.RData')
    save(pdata_bscorr, file = paste0(rdir,"output_per_sub/picsum_bscorr/",file_to_save))
  }
  
  #------------------------------------------------------------------------------- 
  # 1.2 smooth the data with recursive median filter
  
  pdata_smooth <- smooth_pdata(sub, alldirs, pdata_bscorr, window.size)
  # save smoothed data per subject 
  if (savefile_1.2 == T){
    file_to_save = paste0('pdata_smooth_',sub,"-",Sys.Date(),'.RData')
    save(pdata_smooth, file = paste0(rdir,"output_per_sub/picsum_smooth/",file_to_save))  
  }
  
  #-------------------------------------------------------------------------------   
  # 1.3 aggregate pupil values over all trials + separated per valence category (per sub)
  
  meanProfiles_sub_smooth <- aggreg_pdata(sub, alldirs, pdata_smooth, m.trial_aggr)
  # save aggregated data per subject 
  if (savefile_1.3 == T){
    file_to_save = paste0('meanProfiles_smooth_',sub,"-",Sys.Date(),'.RData')
    save(meanProfiles_sub_smooth, file = paste0(rdir,"output_per_sub/picsum_meanProfiles/",file_to_save))
  }
   # plot(meanProfiles_sub_smooth[[1]]) # all
   # plot(meanProfiles_sub_smooth[[2]]) # neg
   # plot(meanProfiles_sub_smooth[[3]]) # neu
   # plot(meanProfiles_sub_smooth[[4]]) # pos
   # plot(meanProfiles_sub_smooth[[5]]) # scr
  
  #-------------------------------------------------------------------------------   
  # 1.4 aggregate pupil values over subjects
  if (reference == "new") {
    gr_meanProfiles_smooth <- aggreg2_pdata(sub, alldirs, mean_Profiles_sub_smooth, m.sub_aggr)
  }else{
    load("~/Desktop/Eye-tem/Preprocessing/3.pupil_profile/r_output/04112016/gr_meanProfiles_smooth-2016-11-04.RData")
    gr_meanProfiles_smooth <- gr_meanProfiles_smooth
  }
} # end subject loop

#rm(meanProfile_all, meanProfile_neg, meanProfile_neu, meanProfile_pos, meanProfile_scr)
# plot(gr_meanProfiles_smooth[[1]]) # all
# plot(gr_meanProfiles_smooth[[2]]) # neg
# plot(gr_meanProfiles_smooth[[3]]) # neu
# plot(gr_meanProfiles_smooth[[4]]) # pos
# plot(gr_meanProfiles_smooth[[5]]) # scr
# save aggregated data over all subjects  
if (savefile_1.4 == T){
  file_to_save = paste0("gr_meanProfiles_smooth", "-",Sys.Date(),'.RData')
  save(gr_meanProfiles_smooth, file = paste0(rdir,file_to_save))
}
  
#-------------------------------------------------------------------------------   
# 1.5 correlate each trial to the mean Profile

for (sub in subjects_exist){
  pdata_trial_corr <- trialcorr_pdata(sub, alldirs, gr_meanProfiles_smooth)
} # end subject loop 
# save correlation for each trial + each subject  
if (savefile_1.5 == T){
  file_to_save = paste0("pdata_trial_corr", "-",Sys.Date(),'.RData')
  save(pdata_trial_corr, file = paste0(rdir,file_to_save))
}
  
#-------------------------------------------------------------------------------  
# 1.6 exclude trials under a to be specified threshold
trialfilter <- filter_pdata(sub, alldirs, pdata_trial_corr, corr.threshold)
# save trialfilter 
if (savefile_1.6 == T){
  file_to_save = paste0("trialfilter_",corr.threshold,"-",Sys.Date(),'.RData')
  save(pdata_trial_corr, file = paste0(rdir,file_to_save))
}

#-------------------------------------------------------------------------------  
# 1.7 aggregate pupil measures over "good" trials

for (sub in subjects_exist){
  meanProfiles_filtered_sub_smooth <- aggreg3_pdata(sub, alldirs, pdata_smooth, trialfilter, m.trial_filtered_aggr, ntrial.aggregate_all, ntrial.aggregate_val)
  # save aggregated + filtered data per subject
  if (savefile_1.7 == T){  
    file_to_save = paste0('meanProfiles_filtered_smooth_',sub,"-",Sys.Date(),'.RData')
    save(meanProfiles_filtered_sub_smooth, file = paste0(rdir,"output_per_sub/picsum_meanProfiles_filtered/",file_to_save))
  }
  
  # plot(meanProfiles_filtered_sub_smooth[[1]]) # all
  # plot(meanProfiles_filtered_sub_smooth[[2]]) # neg
  # plot(meanProfiles_filtered_sub_smooth[[3]]) # neu
  # plot(meanProfiles_filtered_sub_smooth[[4]]) # pos
  # plot(meanProfiles_filtered_sub_smooth[[5]]) # scr
  
  #------------------------------------------------------------------------------- 
  # 1.8 extract additional information: minimal height, slope, mean during dilation period,...
  
  pdata_charac_sub <- charac_pdata(sub, alldirs, meanProfiles_filtered_sub_smooth, max.tp_ampl, min.tp_ampl, max.tp_dilation, min.tp_dilation)
  # save additional data per subject
  if (savefile_1.8 == T){
    file_to_save = paste0('pdata_charac_',sub,"-",Sys.Date(),'.RData')
    save(pdata_charac_sub, file = paste0(rdir,"output_per_sub/picsum_charac/",file_to_save))
  }
} # end subject loop 

# save additional data for all subjects in long format
if (savefile_1.8_long == T){
  file_to_save = paste0('pdata_charac_long',"-",Sys.Date(),'.RData')
  save(charac_data_long_all, charac_data_long_neg,charac_data_long_neu, charac_data_long_pos, charac_data_long_scr, file = paste0(rdir,file_to_save)) 
}

#------------------------------------------------------------------------------- 
# 1.9  plot the data (on the subjects basis) 
  
 pdata_plot <- plot_pdata(alldirs)
 # pdata_plot[[1]] # pupil height over 150 timepoints per valence category
 # pdata_plot[[2]] # slope over 150 timepoints per valence category
 # pdata_plot[[3]] # boxplot minheight per valence category (just one value per subject)
 # pdata_plot[[4]] # density plot minheight per valence category (just one value per subject)
 # pdata_plot[[5]] # boxplot last timepoint of minheight per valence category (just one value per subject)
 # pdata_plot[[6]] # density plot last timepoint of minheight per valence category (just one value per subject)
 # pdata_plot[[7]] # boxplot dilation mean per valence category (just one value per subject)
 # pdata_plot[[8]] # density plot dilation mean per valence category (just one value per subject)


# save plots on a subject basis
if (savefile_1.9 == T){
  file_to_save = paste0('pdata_plot',"-",Sys.Date(),'.RData')
  save(pdata_plot, file = paste0(rdir,file_to_save))
}

#------------------------------------------------------------------------------- 
# 2.0 aggregate "good" pupil measures over subjects

gr_meanProfiles_filtered_smooth <- aggreg4_pdata(alldirs, m.sub_filtered_aggr)
# save total aggregated data
if (savefile_2.0 == T){
  file_to_save = paste0("gr_meanProfiles_filtered_smooth", "-",Sys.Date(),'.RData')
  save(gr_meanProfiles_filtered_smooth, file = paste0(rdir,file_to_save)) 
}

#------------------------------------------------------------------------------- 
# 2.1 plot the data, aggregated over the subjects

pdata2_plot <- plot2_pdata(alldirs, gr_meanProfiles_filtered_smooth)
  # pdata2_plot[[1]] # pupil height per valence category
  # pdata2_plot[[2]] # slope per valence category
  # pdata2_plot[[3]] # min height per valence category
  # pdata2_plot[[4]] # last timepoint of min height per valence category
  # pdata2_plot[[5]] # dilation mean time per valence category



# save plots2 (aggregated over subjects)
if (savefile_2.1 == T){
  file_to_save = paste0('pdata2_plot',"-",Sys.Date(),'.RData')
  save(pdata2_plot, file = paste0(rdir,file_to_save))
}


#------------------------------------------------------------------------------- 
# 2.2 write all the plots to pdf
plotting_aid(rdir, pdata_plot, pdata2_plot)
