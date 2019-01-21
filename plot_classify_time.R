## import statistical functions from plot_mate.R for later use
rm(list=ls())  ##clear workspace
code.dir <- '/media/dejun/holder/gitlinux/VisualData'
setwd(code.dir)
source("plot_mate.R")

##=================================================================================##
##########bar plot of the time algorithm used to segment each organ##################
##=================================================================================##
this.dir <- dirname(parent.frame(2)$ofile)

data_dir = "/media/dejun/holder/algtest/info_board/1.1.2"
# data_dir = "/media/dejun/holder/Data/ruijin_vertebra/csv85_w_label"
setwd(data_dir)
version = c('1.1.2')
file_name = c('classify_runtime') #load_runtime_compare.csv 'classify_runtime'

file_path = paste(data_dir, paste(version, file_name, 'csv', sep = '.'), sep = '/')
runtime_data <- read.csv(file_path)

hospital = sapply(as.character(runtime_data$hosp), function(x) unlist(strsplit(x, '_'))[1])
runtime_data$hospital <- hospital

runtime_long <- gather(runtime_data[,c(2,3,6,10:14,16)], method, duration, LoadDataTime:PredictTime)

# get valid runtime data
runtime_long_valid = runtime_long[(runtime_long$duration>0 & 
                                     runtime_long$num_slices<500 &
                                     !is.na(runtime_long$method)),]

level_order = c('PostProcessTime', 'PredictTime', 'LoadModelTime', 'PreProcessTime', 'LoadDataTime')
operation_name = c('postprocess', 'predcit', 'loadmodel', 'preprocess','loaddata')
runtime_long_valid$method <- factor(runtime_long_valid$method)
runtime_long_valid$method <- factor(runtime_long_valid$method,
                                    levels = level_order,
                                    labels = operation_name)


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

  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # legend.title=element_blank()
        legend.position="top")
  

ggsave(paste(version, "classify_runtime_by_pat", "pdf", sep = '.'),
       width = 7, height = 5, units = "in")


# ggplot(runtime_data, aes(x=num_slices, y=LoadDataTime)) +
#   geom_point(shape = 2, aes(colour = hospital)) +    # Use hollow circles
#   geom_smooth(method=lm)+

