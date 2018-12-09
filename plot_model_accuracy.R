
#############scatter plot the dice or hausdorff distance of different organs by hospitals###################
AccuracyScatterPlot <- function(dataframe, x_variable, target_metric, colors_on,
                              valid_range = c(0,1), fac1 = '.', fac2 = 'bodypart',
                              x_label = 'Organs', y_label = 'Dice Coefficent'){
  # metricdata_valid = clean_data[!is.na(clean_data$dice) & clean_data$dice>0.1,]
  metricdata_valid = dataframe[!is.na(dataframe[target_metric])& 
                                 dataframe[target_metric]>=valid_range[1] &
                                 dataframe[target_metric]<=valid_range[2],]
  # metricdata_valid = metricdata_valid[!(metricdata_valid$organs=="liver" & metricdata_valid$hosp== "beiyi3"),]
  # x_variable <- enquo(x_variable)
  # target_metric <- enquo(target_metric)
  # colors_on <- enquo(colors_on)
  # x_group <- enquo(x_group)
  
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
          axis.text.x  = element_text(face = "bold", angle=90, vjust =0.7, size=12), #vjust =0,
          axis.title.y = element_text(face="bold", size=16),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
          legend.title=element_text(face="bold", size=12),
          legend.text = element_text(size = 12),
          legend.position="top"  ## legend.position="none" if you don't need legend
    )
  # +coord_flip()
  # guides(shape =  guide_legend(override.aes = list(size=4))) # change the text size of the legend
}



## import statistical functions from plot_mate.R for later use
getwd()
source("plot_mate.R")

# data input and select
## set working directory where data is stored 
kRes.wd <- "/media/dejun/holder/datavisualizeR/data/18.12.10"  #change
setwd(kRes.wd)

# create an empty data frame object with necessary variables
# dice_data <- read.csv("DiceData_detail.csv")

# load the names of all xlsx files in the directory
acc_data_file = list.files(pattern = "*accuracy.xlsx")  #"\\.xlsx$"
hosp_name <- NA

for(i in 1:length(acc_data_file)){
  ##split a string by certain sign
  hosp_name[i] = strsplit(acc_data_file[i], '[.]')[[1]][1]
}

acc_data_all <- NA
# read all xlsx files, make necessary transformation and
# store in an df object for later analysis
for(j in 1:length(acc_data_file)){     #对于每个一级目录(文件夹)下的每个xlsx文件
  new_1<-read_xlsx(paste(kRes.wd,acc_data_file[j],sep='/')) #读取xlsx文件 #,sheetIndex=1,encoding='UTF-8'
  ##### from wide data format to long data format
  stlong <- gather(new_1, pid, value, c(3:dim(new_1)[2]))
  ## from long data format to wide data format
  stlong <- spread(stlong, Items, value)
  stlong$hosp<- strsplit(hosp_name[j], '[_]')[[1]][1]
  # stlong$bodypart<- strsplit(hosp_name[j], '[_]')[[1]][2]
  print(acc_data_file[j])
  print(dim(stlong))
  ###concatenate several dataframe by rows, thus the name rbind = bind rows
  acc_data_all <- rbind(acc_data_all, stlong)
}
acc_data_all <- acc_data_all[!is.na(acc_data_all[,1]),]


###change the names of chosen variables
names(acc_data_all)[1]<-"organs"
names(acc_data_all)[6]<-"dice"

###add a column variable to the dataframe
acc_data_all <- assignBodyPart(acc_data_all)
others_data <- acc_data_all[acc_data_all[,'bodypart']=='Others',]
unique(others_data[,'organs'])

# selection dice and hausdorff distance data

hd_dice_data_all <- acc_data_all[, c(1:2,17:18, 3,6)] #'AverageHausdorffDistance'
write.csv(hd_dice_data_all, file = "AccData_detail.csv")


###save the processed data for later reference
# dice_data$z_spacing <- NA
# for(i in 1:dim(dice_data)[1]){
#   dice_data[i, 'z_spacing'] = meta_data[ meta_data[,'patid'] ==dice_data[i,'pid'], 'z_spacing']
# }
# write.csv(dice_data, file = "DiceData_detail.csv")

###do descriptive statistics, generating mean, std and standard error
#exclude invalid observations  


metric = c('dice')
groupvars = c('organs') #, 'hosp'
valid_range = c(0,1)
metricStat(acc_data_all, metric, groupvars, valid_range)


target_metric = 'HausdorffDistance95'
colors_on = 'hosp'
x_variable = 'organs'
x_group = 'bodypart'

AccuracyScatterPlot(acc_data_all, x_variable, target_metric, colors_on,
                    valid_range = c(0,100),fac1 = '.', fac2 = x_group,
                    x_label = 'Organs', y_label = 'Hausdorff Distance 95')

ggsave(paste(target_metric, 'by', x_variable, "pdf", sep = '.'))
###########dice by algorithm version######################

rm(list=ls())  ##clear workspace

data_dir = "/media/dejun/holder/datavisualizeR/data/compare_version"
setwd(data_dir)
first_category_name = list.files(data_dir)
dir = paste(data_dir,first_category_name,sep="/")
n = length(dir) 
dice_data = read.csv(dir[1])
time_data = read.csv(dir[2])

dice_data_long <- gather(dice_data, version, dice, c(2:dim(dice_data)[2]))
time_data_long <- gather(time_data, version, time, c(2:dim(dice_data)[2]))

data_valid <- dice_data_long[!is.na(dice_data_long$dice),]
# data_valid <- time_data_long[!is.na(time_data_long$time),]
write.csv(data_valid, "dice_by_version_valid.csv")
ggplot(data=data_valid, aes(x=version, y=time, group=organ, colour = organ)) +
  geom_line()+
  geom_point()+
  labs(x = "Segmentation Algorithm Versions", y = "Average Time spent in Prediction")+ #Dice Coefficient #Hausdorff Distance 
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(face = "bold", angle=0, vjust =0, size=12), #vjust =0,
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        legend.title=element_text(face="bold", size=12),
        legend.text = element_text(size = 12)
        # legend.position="top"  ## legend.position="none" if you don't need legend
  )
ggsave("time_by_version.pdf")
