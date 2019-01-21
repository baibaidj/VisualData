## import statistical functions from plot_mate.R for later use
rm(list=ls())  ##clear workspace
setwd('/media/dejun/holder/gitlinux/VisualData')
source("plot_mate.R")

# data input and select
## set working directory where data is stored 
kRes.wd <- "/media/dejun/holder/algtest/info_board/1.1.2"  #change
setwd(kRes.wd)

# create an empty data frame object with necessary variables
acc_data_all <- read.csv("1.1.2.metric_final.csv")

###change the names of chosen variables
# names(acc_data_all)[1]<-"organs"
# names(acc_data_all)[6]<-"dice"

###add a column variable to the dataframe
acc_data_all <- assignBodyPart(acc_data_all, c('pred_rois'))
# unique(acc_data_all$bodypart)
# a = acc_data_all[acc_data_all$bodypart == 'Muscles', ]

# unique(others_data[,'organs'])

# selection dice and hausdorff distance data
# hd_dice_data_all <- acc_data_all[, c(1:2,17:18, 3,6)] #'AverageHausdorffDistance'
# write.csv(hd_dice_data_all, file = "AccData_detail.csv")

###do descriptive statistics, generating mean, std and standard error
#exclude invalid observations  

metric_list = c('dice', 'hd', 'rvd')
metric_names = c('Dice Coefficient', 'Hausdorff Distance', 'Volume Similarity')
metric_valid_range = matrix(c(0,0,-100, 1,100, 100), ncol= 2)

bp_name = c('HeadNeck', 'ChestAbdoPelvis', 'VessleMusle')
bp_include = list(HeadNeck = c('HeadNeck'), 
                  ChestAbdoPelvis = c('Chest', 'Abdomen', 'Pelvis'), 
                  VessleMusle = c('Vessels', 'Muscles')
                )
colors_on = 'hosp'
x_variable = 'pred_rois'
x_group = 'bodypart'


for (metric in metric_list){
  groupvars = c('pred_rois', 'hosp') #, 'hosp'
  valid_range = c(0,200)
  metric_summary <- metricStat(acc_data_all, metric, groupvars, valid_range)
}


for (i in seq(1,3)){
target_metric = metric_list[i]#'HausdorffDistance95'

  for (j in seq(1, 3)){
    bp_data <- acc_data_all[acc_data_all$bodypart %in% bp_include[[bp_name[j]]], ]
    AccuracyScatterPlot(bp_data, x_variable, target_metric, colors_on,
                      valid_range = metric_valid_range[i, ],fac1 = '.', fac2 = x_group,
                      x_label = 'Organs', y_label = metric_names[i])
    ggsave(paste(target_metric, 'by', x_variable, bp_name[j],"pdf", sep = '.'),
           width = 7, height = 5, units = "in")
  }
}
###########dice by algorithm version######################
# 
# rm(list=ls())  ##clear workspace
# 
# data_dir = "/media/dejun/holder/datavisualizeR/data/compare_version"
# setwd(data_dir)
# first_category_name = list.files(data_dir)
# dir = paste(data_dir,first_category_name,sep="/")
# n = length(dir) 
# dice_data = read.csv(dir[1])
# time_data = read.csv(dir[2])
# 
# dice_data_long <- gather(dice_data, version, dice, c(2:dim(dice_data)[2]))
# time_data_long <- gather(time_data, version, time, c(2:dim(dice_data)[2]))
# 
# data_valid <- dice_data_long[!is.na(dice_data_long$dice),]
# # data_valid <- time_data_long[!is.na(time_data_long$time),]
# write.csv(data_valid, "dice_by_version_valid.csv")
# ggplot(data=data_valid, aes(x=version, y=time, group=organ, colour = organ)) +
#   geom_line()+
#   geom_point()+
#   labs(x = "Segmentation Algorithm Versions", y = "Average Time spent in Prediction")+ #Dice Coefficient #Hausdorff Distance 
#   theme_bw()+
#   theme(axis.title.x = element_text(face="bold", size=16),
#         axis.text.x  = element_text(face = "bold", angle=0, vjust =0, size=12), #vjust =0,
#         axis.title.y = element_text(face="bold", size=16),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
#         legend.title=element_text(face="bold", size=12),
#         legend.text = element_text(size = 12)
#         # legend.position="top"  ## legend.position="none" if you don't need legend
#   )
# ggsave("time_by_version.pdf")




# old data formating, transform data structure to desierable format 
# load the names of all xlsx files in the directory
# acc_data_file = list.files(pattern = "*accuracy.xlsx")  #"\\.xlsx$"
# hosp_name <- NA
# 
# for(i in 1:length(acc_data_file)){
#   ##split a string by certain sign
#   hosp_name[i] = strsplit(acc_data_file[i], '[.]')[[1]][1]
# }
# 
# acc_data_all <- NA
# # read all xlsx files, make necessary transformation and
# # store in an df object for later analysis
# for(j in 1:length(acc_data_file)){     #对于每个一级目录(文件夹)下的每个xlsx文件
#   new_1<-read_xlsx(paste(kRes.wd,acc_data_file[j],sep='/')) #读取xlsx文件 #,sheetIndex=1,encoding='UTF-8'
#   ##### from wide data format to long data format
#   stlong <- gather(new_1, pid, value, c(3:dim(new_1)[2]))
#   ## from long data format to wide data format
#   stlong <- spread(stlong, Items, value)
#   stlong$hosp<- strsplit(hosp_name[j], '[_]')[[1]][1]
#   # stlong$bodypart<- strsplit(hosp_name[j], '[_]')[[1]][2]
#   print(acc_data_file[j])
#   print(dim(stlong))
#   ###concatenate several dataframe by rows, thus the name rbind = bind rows
#   acc_data_all <- rbind(acc_data_all, stlong)
# }
# acc_data_all <- acc_data_all[!is.na(acc_data_all[,1]),]