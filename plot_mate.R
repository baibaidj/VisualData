# function: install new packages and their dependencies
ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

## new packages can be added here
packages <- c("reshape2", "plyr", "readxl", #"sandwich", "nlme", "effects", "binom", "doBy", "grid",
              "boot", "ggplot2", #"scales", "lme4", "bootstrap", "car","arm","mfx","psych",
              "tidyr", #"devtools", "data.table", "stringr", "stats", "QuantPsyc", "lsmeans", 
              "multicon")
ipak(packages)

# library("readxl")
# library("ggplot2")


# function: Get the descrptive statistics of a data set: 
## including mean, sd, sem, by between-subject variable and within-subject variable
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = paste(measurevar, "mean", sep = "_")))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

##========================================

###assign body parts such as chest or headneck to individual results based on organ name
assignBodyPart <- function(data_frame, organ_columns = c('pred_rois')){
  data_frame$bodypart <- NA
  # eye_brain <- tolower(c("eye","Eye_L", "Eye_R", "lens", "Lens_L", "Lens_R", "opticnerve","OpticNerve_L", 
  #                        "OpticNerve_R", "temporallobe","TemporalLobe_L", "TemporalLobe_R", "Cochlea_L",
  #                        "Cochlea_R", 'BrainStem'))
  # headgland <- tolower(c("Parotid", "Parotid_L", "Parotid_R", "Pituitary", "Thyroid"))
  # thoracic <- tolower(c("lung", "Lung_L", "Lung_R", "Trachea", "Heart", "Esophagus","pulmonary",
  #                       "Atrium_L", "Atrium_R", "Ventricle_L", "Ventricle_R"))
  # abdomen <- tolower(c("kidney", "Kidney_L", "Kidney_R", "Liver",  "Spleen", "Stomach"))
  # pelvis <-tolower(c("Bladder", "Rectum","FemoralHead_L", "FemoralHead_R", "pelvis", 
  #                    "Femur_L", "Femur_R", "PelvicBone"))
  # longorgan <- tolower(c( "Body",  "SpinalCord"))
  eye_brain <- tolower(c("Eye_L", "Eye_R","Lens_L", "Lens_R", "OpticNerve_L", "OpticNerve_R", "OpticChiasm",
                         "TemporalLobe_L", "TemporalLobe_R", "Cochlea_L", "Cochlea_R", 'BrainStem', 
                         "Brain", "Cerebrum", "Cerebellum", "Body",'body',
                         'cochlea', 'eye', 'lens', 'optic_chiasm', 'optic_nerve', 'temporal_lobe',
                         'mandibleLR', 'opticnerve', 'temporallobe'))
  
  headgland <- tolower(c("Parotid", "Parotid_L", "Parotid_R", "Pituitary", "Thyroid", "Mandible",
                         "Mandible_L", "Mandible_R", "TMJ_L", "TMJ_R", "Tongue", "Larynx", "OralCavity",
                         "SMG_L", "SMG_R", 'SMG', 'oral', 'pulmonary_vessel'))
  
  thoracic <- tolower(c("lung", "Lung_L", "Lung_R", "Trachea", "Trachea_Bronchus", "Heart","pulmonary",
                        "Atrium_L", "Atrium_R", "Ventricle_L", "Ventricle_R", "Breast_L", "Breast_R",
                        "SpinalCord", "Esophagus"))
  
  abdomen <- tolower(c("kidney", "Kidney_L", "Kidney_R", "Liver",  "Spleen", "Stomach", "BowelBag", "Pancreas"))
  
  pelvis <-tolower(c("Bladder", "Rectum","FemoralHead_L", "FemoralHead_R", "pelvis", 
                     "Femur_L", "Femur_R", "PelvicBone"))
  
  vessels <- tolower(c( "Vertebral.A_L", "Vertebral.A_R", 'CCA',"CCA_L", "CCA_R", "Aorta", "IJV_L", "IJV_R",
                        "BCV_L", "BCV_R", "SVC", "IMA_L", "IMA_R", "IVC", "Subclavian.A_L", "Subclavian.A_R", 
                        "Pulmonary.A", "Pulmonary.V", "IMA", 'vein', 'Vertebral.A','pulmonaryvessel',
                        'subclavian', 'Vertebral.A'
                        ))
  
  muscles <- tolower(c( "Sternohyoid.M",  "Scleido.M", 'scleido', 'sternohyoid'))
  
  for (i in 1:dim(data_frame)[1]){
    if(tolower(data_frame[i, organ_columns]) %in% eye_brain){
      data_frame[i,"bodypart"] =1
    }else if(tolower(data_frame[i, organ_columns]) %in% headgland){
      data_frame[i,"bodypart"] =1
    }else if(tolower(data_frame[i, organ_columns]) %in% thoracic){
      data_frame[i,"bodypart"] = 2
    }else if(tolower(data_frame[i, organ_columns]) %in% abdomen){
      data_frame[i,"bodypart"] = 3
    }else if(tolower(data_frame[i, organ_columns]) %in% pelvis){
      data_frame[i,"bodypart"] = 4
    }
    # else if(tolower(data_frame[i, organ_columns]) %in% longorgan){
    #   data_frame[i,"bodypart"] = 5}
    else if(tolower(data_frame[i, organ_columns]) %in% vessels){
      data_frame[i,"bodypart"] = 5
    }else if(tolower(data_frame[i, organ_columns]) %in% muscles){
      data_frame[i,"bodypart"] = 6
    }else{
      data_frame[i,"bodypart"] = 0
    }
  }
  bodypart_name <- c("Others", "HeadNeck", "Chest", "Abdomen", "Pelvis", "Vessels", "Muscles")
  data_frame$bodypart<-factor(data_frame$bodypart,
                              levels = c(0:6),
                              labels = bodypart_name)
  return(data_frame)
}


# calculate the descriptive statistics of a metric, including mean, standard deviation, and standard error 
metricStat <- function(dataframe, target_metric, groupsvars, valid_range){
  metricdata_valid = dataframe[!is.na(dataframe[target_metric])& 
                                 dataframe[target_metric]>=valid_range[1] &
                                 dataframe[target_metric]<=valid_range[2],]
  print(dim(metricdata_valid)) #colnames(metricdata_valid)
  metric_by_groups <- summarySE(metricdata_valid, measurevar=target_metric, groupvars=groupsvars) #"z_spacing",
  write.csv(metric_by_groups, file = paste(target_metric, 'by', paste(groupsvars, collapse = '.'), 'csv', sep = '.'))
  return(metric_by_groups)
}


#############scatter plot the dice or hausdorff distance of different organs by hospitals###################
AccuracyScatterPlot <- function(dataframe, x_variable, target_metric, colors_on,
                                valid_range = c(0,1), fac1 = '.', fac2 = 'bodypart',
                                x_label = 'Organs', y_label = 'Dice Coefficent'){
  # metricdata_valid = clean_data[!is.na(clean_data$dice) & clean_data$dice>0.1,]
  metricdata_valid = dataframe[!is.na(dataframe[target_metric])& 
                                 dataframe[target_metric]>=valid_range[1] &
                                 dataframe[target_metric]<=valid_range[2],]
  
  ggplot(metricdata_valid, aes_string(x_variable, target_metric))+
    geom_boxplot( outlier.shape = NA, colour = "#3366FF", fill = "white")+
    #outlier.shape = NA, colour = "#3366FF", outlier.colour = "black", outlier.shape = 1
    geom_jitter(aes_string(colour = colors_on), size = 1.0, width = 0.2)+ # plot data points of all participants
    scale_shape_manual(values=c(1,17))+ # decide the shape of the data points for each group
    facet_grid(reformulate(fac2, fac1), scales = "free_x", space = "free_x")+ # divide the plot into several parts by conditions 
    # stat_summary(fun.data="mean_se", fun.args = list(mult=1),   # plot mean and error bar in the middle
    #              geom="crossbar", width=0.3, color = "red")+
    labs(x = x_label, y = y_label)+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_text(face = "bold", angle=90, vjust =0.7, size=8), #vjust =0,
          axis.title.y = element_text(face="bold", size=12),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=8),
          legend.title=element_text(face="bold", size=12),
          legend.text = element_text(size = 8),
          legend.position="top"  ## legend.position="none" if you don't need legend
    )
  # +coord_flip()
  # guides(shape =  guide_legend(override.aes = list(size=4))) # change the text size of the legend
}

