
##=================================================================================##
##########bar plot of the time algorithm used to segment each organ##################
##=================================================================================##

class_runtime_plot <- function(file_path){
  runtime_data <- read.csv(file_path)
  
  # extract distinct hospital names
  hospital = sapply(as.character(runtime_data$hosp), function(x) unlist(strsplit(x, '_'))[1])
  runtime_data$hospital <- hospital
  
  # transform the data to long format to be plotable
  runtime_long <- gather(runtime_data[,c(2,3,6,10:14,16)], method, duration, LoadDataTime:PredictTime)
  
  # get valid runtime data
  runtime_long_valid = runtime_long[(runtime_long$duration>0 & 
                                       runtime_long$duration<6 &
                                       runtime_long$num_slices<500 &
                                       !is.na(runtime_long$method)),]
  
  # reorder the levels in a factor
  operation_levels = c('PostProcessTime', 'PredictTime', 'LoadModelTime', 
                  'PreProcessTime', 'LoadDataTime')
  operation_names = c('postprocess', 'predcit', 'loadmodel', 'preprocess','loaddata')
  runtime_long_valid$method <- factor(runtime_long_valid$method,
                                      levels = operation_levels,
                                      labels = operation_names)
  
  # plot 1 time by patients
  ggplot(runtime_long_valid, aes(x= pat_index, y=duration,
                                 color = method #, group = method
  )) +
    geom_bar(stat="identity", fill="white") +    # Use hollow circles colour = hospital,  stat="identity",
    labs(x = 'Patient Index', y = "Running Time (s)")+
    scale_fill_brewer(palette="Set3")+
    # geom_smooth(method=lm,   # Add linear regression lines
    #             se=FALSE)+
    theme_bw()+
    theme_classic()
  
  ggsave(paste(version, "classify_runtime_by_pat", "pdf", sep = '.'),
         width = 7, height = 5, units = "in")
  
  # plot 2 time by number of slices
  ggplot(runtime_long_valid, aes(x= num_slices, y=duration, 
                                 color = method #, group = method
  )) +
    geom_point(stat="identity") +    # Use hollow circles colour = hospital,  stat="identity", 
    labs(x = 'Number of Slices', y = "Running Time (s)")+
    scale_fill_brewer(palette="Set3")+
    geom_smooth(method=lm,   # Add linear regression lines
                se=FALSE)+
    theme_bw()+
    theme(axis.title.x = element_text(face="bold", size=16),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
          axis.title.y = element_text(face="bold", size=16),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          # legend.title=element_blank()
          legend.position="top")
  
  ggsave(paste(version, "classify_runtime_by_numslices", "pdf", sep = '.'),
         width = 7, height = 5, units = "in")
}

# ggplot(runtime_data, aes(x=num_slices, y=LoadDataTime)) +
#   geom_point(shape = 2, aes(colour = hospital)) +    # Use hollow circles
#   geom_smooth(method=lm)+

