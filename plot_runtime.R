## import statistical functions from plot_mate.R for later use
data_dir = "/media/dejun/holder/datavisualizeR/code"
setwd(data_dir)
source("plot_mate.R")

##=================================================================================##
##########bar ploi of the time algorithm used to segment each organ##################
##=================================================================================##
rm(list=ls())  ##clear workspace

data_dir = "/media/dejun/holder/datavisualizeR/code"
setwd(data_dir)
source("plot_model_accuracy.R")

# first_category_name = list.files(data_dir)
# dir = paste(data_dir,first_category_name,sep="/")
# n = length(dir) 


###initialize an df object for later use
# timedata_one <- data.frame(ot= character(0), organs= character(0),
#                            operation = character(0), pid = character(0),
#                            time = numeric(0), hosp = character(0), bodypart = character(0))

# for(i in 1:n){
#   
#   ##split a string by certain sign
#   dir_name = strsplit(first_category_name[i], '[_]')
#   ##when calling, remember using double []
#   hosp_name = dir_name[[1]][1]
#   bodypart_name = dir_name[[1]][2]
#   ##using paste two concatenate several string to make a path
#   new_1<-read_xlsx(paste(dir[i], "time.xlsx", sep = "/"))
#   print(dir[i])
#   print(dim(new_1))
#   
#   ##choose part of a df
#   new_1 <- subset(new_1, select=-c(average,standard))
#   
#   ##a way to add new variables to a df
#   new_col = c("organs", "operation", "hosp", "bodypart")
#   new_1[, new_col] <- NA
#   
#   ##assign values to a variable for all samples
#   new_1$hosp = hosp_name
#   new_1$bodypart = bodypart_name
#   
#   for (j in 1:dim(new_1)[1]){
#     phrase_split = strsplit(toString(new_1[j,1]), '[_]')[[1]]
#     new_1$organs[j] = paste(phrase_split[1:length(phrase_split)-1], collapse = '')
#     new_1$operation[j] = phrase_split[length(phrase_split)]
#   }
#   stlong <- gather(new_1, pid, time, c(2:(dim(new_1)[2]-4)))
#   timedata_one <- rbind(timedata_one,  stlong)
# }
# # 
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
time_data <- read.csv(paste(data_dir, "dice_by_spacing.csv", sep = "/"))

# time_data$z_spacing <- NA

for(i in 1:dim(time_data)[1]){
  time_data[i, 'z_spacing'] = meta_data[ meta_data[,'patid'] ==time_data[i,'pid'], 'z_spacing']
}

time_data <- assignBodyPart(time_data)
time_sum_data  <- summarySE(time_data[!is.na(time_data$time), ],  measurevar = "time", groupvars = c("organs", "operation", 'z_spacing'))
time_sum_data
write.csv(time_sum_data, "operationtime_by_thickness_stat.csv")

organ_no_need = c("cochlea", "pulmonaryvessel")
time_sum_include = time_sum_data[!(time_sum_data$organs %in% organ_no_need),]


names(time_data)[2]<-"thickness"
time_data$thickness <- factor(time_data$thickness,
                              levels = c(1,3,5),
                              labels = c('1mm', '3mm','5mm')
                              )

pd <- position_dodge(0.9)
ggplot(data = time_data[time_data$TotalTime_Mean>0, ]   , 
       aes(x= Organs, y=TotalTime_Mean, fill=thickness), na.rm = TRUE)+
  geom_bar( stat="identity", position=position_dodge())+ #position=position_dodge(),
  # geom_errorbar(aes(ymin=ClassifyTime_Mean-ClassifyTime_Std, ymax=ClassifyTime_Mean+ClassifyTime_Std), width=.1, position=pd)+
  scale_fill_manual(values = c( "#88C542","#30499B", "#F0A32F" ))+
  # geom_errorbar(aes(ymin=time_mean-se, ymax=time_mean+se),
  #               position=position_dodge(0.9),
  #               width = .2)+  #size = 1,
  labs(y = "Total Time of Prediction")+
  # facet_grid(. ~ bodypart, scales = "free_x", space = "free_x")+ 
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
ggsave("totaltime_by_spacing.pdf")
