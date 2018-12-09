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
assignBodyPart <- function(data_frame){
  data_frame$bodypart <- NA
  eye_brain <- tolower(c("eye","Eye_L", "Eye_R", "lens", "Lens_L", "Lens_R", "opticnerve","OpticNerve_L", 
                         "OpticNerve_R", "temporallobe","TemporalLobe_L", "TemporalLobe_R", "Cochlea_L",
                         "Cochlea_R", 'BrainStem'))
  headgland <- tolower(c("Parotid", "Parotid_L", "Parotid_R", "Pituitary", "Thyroid"))
  thoracic <- tolower(c("lung", "Lung_L", "Lung_R", "Trachea", "Heart", "Esophagus","pulmonary",
                        "Atrium_L", "Atrium_R", "Ventricle_L", "Ventricle_R"))
  abdomen <- tolower(c("kidney", "Kidney_L", "Kidney_R", "Liver",  "Spleen", "Stomach"))
  pelvis <-tolower(c("Bladder", "Rectum","FemoralHead_L", "FemoralHead_R", "pelvis", 
                     "Femur_L", "Femur_R", "PelvicBone"))
  longorgan <- tolower(c( "Body",  "SpinalCord"))
  for (i in 1:dim(data_frame)[1]){
    if(tolower(data_frame$organs[i]) %in% eye_brain){
      data_frame[i,"bodypart"] =1
    }else if(tolower(data_frame$organs[i]) %in% headgland){
      data_frame[i,"bodypart"] =1
    }else if(tolower(data_frame$organs[i]) %in% thoracic){
      data_frame[i,"bodypart"] = 2
    }else if(tolower(data_frame$organs[i]) %in% abdomen){
      data_frame[i,"bodypart"] = 3
    }else if(tolower(data_frame$organs[i]) %in% pelvis){
      data_frame[i,"bodypart"] = 4
    }else if(tolower(data_frame$organs[i]) %in% longorgan){
      data_frame[i,"bodypart"] = 5
    }else{
      data_frame[i,"bodypart"] = 0
    }
  }
  bodypart_name <- c("Others", "HeadNeck", "Chest", "Abdomen", "Pelvis", "LongOrgan")
  data_frame$bodypart<-factor(data_frame$bodypart,
                              levels = c(0:5),
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