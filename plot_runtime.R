
##=================================================================================##
##########bar plot of the time algorithm used to segment each organ##################
##=================================================================================##

# extract roi name in the algorithm file name with a pattern get_xxx_contour.py
roi_in_alg_file <- function(alg_file){
  phrase_dot = unlist(strsplit(as.character(alg_file), "[_]"))
  num_words = length(phrase_dot)
  roi_name = paste(phrase_dot[2:(num_words-1)], collapse = '') #, sep = '_'
  return(roi_name)
}

seg_runtime_plot <- function(file_path){
  
  runtime_data <- read.csv(file_path)
  
  # extract roi names from the alg names
  num_col = dim(runtime_data)[2]
  runtime_valid <- runtime_data[,-c(1,2,5,num_col-1, num_col)]
  runtime_valid$pred_rois <- sapply(as.character(runtime_valid$pred_rois), 
                                    function(x) roi_in_alg_file(x))
  runtime_valid$pred_rois[runtime_valid$pred_rois=='AscendensArchDescendens'] <- 'aorta'
  
  # wide format to long format
  runtime_long <- gather(runtime_valid, operation, duration, ClassifyTime:PredictTime) 
  runtime_long_valid <- runtime_long[(!is.na(runtime_long$duration) & (runtime_long$duration != -1)), ]
  
  # calculate the descriptive statistic of operation time in each step of all algorithms
  target_metric = 'duration'
  groupsvars = c("pred_rois", "operation")
  valid_range = c(0,500)
  stat_by_group = metricStat(runtime_long_valid, target_metric, groupsvars, valid_range)
  
  # group rois by body part 
  stat_include <- assignBodyPart(stat_by_group)
  # reorder operations so that they appear on the plot in ordered sequence
  operation_levels = c('PostProcessTime', 'PredictTime', 'LoadModelTime', 
                       'PreProcessTime', 'LoadDataTime', 'ClassifyTime')
  operation_names = c('postprocess', 'predcit', 'loadmodel', 'preprocess','loaddata', 'bodyclassify')
  stat_include$operation <- factor(stat_include$operation, 
                                   levels = operation_levels, 
                                   labels = operation_names)
  
  # plot the mean runtime of operations for all alg
  unique_alg = unique(stat_include$pred_rois)
  pd <- position_dodge(0.9)
  ggplot(data = stat_include[stat_include$duration_mean>0, ], 
         aes(x= pred_rois, y=duration_mean, fill= operation), na.rm = TRUE)+
    geom_bar(stat="identity") + #position=position_dodge(),
    # scale_fill_brewer(palette="Set1")+
    ylim(0, 50)+
    scale_fill_manual(values = c('#367ABD','#F3A530', '#F9ED3A', 
                                 '#4CB2D4','#751CEC','#88C542'
    ))+
    labs(x = paste('OAR Auto-Contour Algorithms , Version:', version, 
                   '\n number of algorithms', as.character(length(unique_alg))),
         y = "Running Time (s)")+
    facet_grid(. ~ bodypart, scales = "free_x", space = "free_x")+
    theme_bw()+
    theme(axis.title.x = element_text(face="bold", size=12),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
          axis.title.y = element_text(face="bold", size=12),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          # legend.title=element_blank()
          legend.position="top"
          # legend.justification=c(0.5,0.7)
    )
  ggsave(paste(version, "runtime_by_operation_organ", "pdf", sep = '.'),
         width = 9, height = 6, units = "in")
  
}

# color in order: #navy, organge, yellow, 
# blue, purple, green
#, "#88C542","#F0A32F","#30499B" yellow '#F8D14E', blue '#2D9FDB'
# geom_errorbar(aes(ymin=time_mean-se, ymax=time_mean+se),
#               position=position_dodge(0.9),
#               width = .2)+  #size = 1,
# names(time_data)[2]<-"thickness"
# time_data$thickness <- factor(time_data$thickness,
#                               levels = c(1,3,5),
#                               labels = c('1mm', '3mm','5mm')
#                               )

# operation_no_need = c('TotalTime', 'SegmentTime')
# # organ_no_need = c("cochlea", "pulmonaryvessel")
# stat_include = stat_by_group[!(stat_by_group$operation %in% operation_no_need)
#                               # & !(stat_by_group$organs %in% organ_no_need)
#                              , 
#                             ]
