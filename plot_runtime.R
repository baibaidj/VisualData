## import statistical functions from plot_mate.R for later use
rm(list=ls())  ##clear workspace
code.dir <- '/media/dejun/holder/gitlinux/VisualData'
setwd(code.dir)
source("plot_mate.R")

##=================================================================================##
##########bar plot of the time algorithm used to segment each organ##################
##=================================================================================##
this.dir <- dirname(parent.frame(2)$ofile)

data_dir = "/media/dejun/holder/datavisualizeR/data/18.12.10"
setwd(data_dir)
# source("plot_model_accuracy.R")

first_category_name = list.files(data_dir, pattern = 'time.xlsx')
dir = paste(data_dir,first_category_name,sep="/")
n = length(dir)


# read data from several xlsx files into one data frame

##initialize an df object for later use
timedata_one <- data.frame(ot= character(0), organs= character(0),
                           operation = character(0), pid = character(0),
                           time = numeric(0), hosp = character(0), bodypart = character(0))
for(i in 1:n){
  ##split a string by certain sign
  dir_name = strsplit(first_category_name[i], '[_]')
  ##when calling, remember using double []
  hosp_name = dir_name[[1]][1]
  bodypart_name = strsplit(dir_name[[1]][2], '[.]')[[1]][1]
  ##using paste two concatenate several string to make a path
  new_1<-read_xlsx(dir[i])
  print(dir[i])
  print(dim(new_1))

  ##choose part of a df
  new_1 <- subset(new_1, select=-c(average,standard))

  ##a way to add new variables to a df
  new_col = c("organs", "operation", "hosp", "bodypart")
  new_1[, new_col] <- NA

  ##assign values to a variable for all samples
  new_1$hosp = hosp_name
  new_1$bodypart = bodypart_name

  for (j in 1:dim(new_1)[1]){
    phrase_split = strsplit(toString(new_1[j,1]), '[_]')[[1]]
    new_1$organs[j] = paste(phrase_split[1:length(phrase_split)-1], collapse = '')
    new_1$operation[j] = phrase_split[length(phrase_split)]
  }
  stlong <- gather(new_1, pid, time, c(2:(dim(new_1)[2]-4)))
  timedata_one <- rbind(timedata_one,  stlong)
}

time_data <- assignBodyPart(timedata_one)
time_data[time_data$operation=='TotalTime-ClassifyTime', 'operation'] = 'SegmentTime'
write.csv(time_data, file = 'operation.time.detail.csv')

target_metric = 'time'
groupsvars = c("organs", "operation")
valid_range = c(0,100)
stat_by_group = metricStat(time_data, target_metric, groupsvars, valid_range)
# names(time_data)[2]<-"thickness"
# time_data$thickness <- factor(time_data$thickness,
#                               levels = c(1,3,5),
#                               labels = c('1mm', '3mm','5mm')
#                               )

operation_need = c('ClassifyTime', 'SegmentTime')
organ_no_need = c("cochlea", "pulmonaryvessel")
stat_include = stat_by_group[stat_by_group$operation %in% operation_need &
                              !(stat_by_group$organs %in% organ_no_need)
                              ,]
stat_include <- assignBodyPart(stat_include)

pd <- position_dodge(0.9)
ggplot(data = stat_include[stat_include$time_mean>0, ], 
       aes(x= organs, y=time_mean, fill= operation), na.rm = TRUE)+
  geom_bar(stat="identity") + #position=position_dodge(),
  # geom_errorbar(aes(ymin=ClassifyTime_Mean-ClassifyTime_Std, ymax=ClassifyTime_Mean+ClassifyTime_Std), width=.1, position=pd)+
  scale_fill_manual(values = c( '#F8D14E','#2D9FDB'))+ #, "#88C542","#F0A32F","#30499B" yellow '#F8D14E', blue '#2D9FDB'
  # geom_errorbar(aes(ymin=time_mean-se, ymax=time_mean+se),
  #               position=position_dodge(0.9),
  #               width = .2)+  #size = 1,
  labs(y = "Total Time of Prediction")+
  facet_grid(. ~ bodypart, scales = "free_x", space = "free_x")+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # legend.title=element_blank()
        legend.position="top"
        # legend.justification=c(0.5,0.7)
  )
ggsave("totaltime_by_organ.pdf")


#
# # ##checking if there are temporallobe and opticnerve in the df
# # checkdf = timedata_one[timedata_one$organs %in% c("temporallobe", 'opticnerve')]
# 
# ##using the value of two variable making another
# timedata_one = subset(timedata_one, select = -c(X__1))
# segtime_data = timedata_one[timedata_one$operation %in% c("TotalTime"), ]
# classtime_data = timedata_one[timedata_one$operation %in% c("TotalTime-ClassifyTime"), ]
# needdata = timedata_one[timedata_one$operation %in% c("TotalTime","TotalTime-ClassifyTime"), ]
# spreaddata = spread(needdata, operation, time)
# names(spreaddata)[5] <- "totaltime"
# names(spreaddata)[6] <- "classify"
# spreaddata$segtime <-NA
# spreaddata$segtime = with(spreaddata, totaltime -classify)
# spreaddata = subset(spreaddata, select = -c(5))
# time_data = gather(spreaddata, operation, time, c(5:6))
# write.csv(time_data, "operationtime_detail.csv")
# 
# time_sum_data  = summarySE(time_data[!is.na(time_data$time), ],  measurevar = "time", groupvars = c("organs", "operation"))
# time_sum_data <- assignBodyPart(time_sum_data)
# write.csv(time_sum_data, "operationtime_stat.csv")


# data_dir = "/media/dejun/holder/datavisualizeR/data/18.11.7_valid"
# meta_data = read.csv(paste(data_dir, 'metadata.csv', sep = '/'))
# time_data <- read.csv(paste(data_dir, "dice_by_spacing.csv", sep = "/"))
# time_data$z_spacing <- NA
# add spacing to the table
# for(i in 1:dim(time_data)[1]){
#   time_data[i, 'z_spacing'] = meta_data[ meta_data[,'patid'] ==time_data[i,'pid'], 'z_spacing']
# }

