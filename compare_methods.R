# statistics and plot controller

rm(list=ls())  ##clear workspace

# prepare fundamental dir
code_dir <- '/media/dejun/holder/gitlinux/VisualData'
result_dir <- "/media/dejun/holder/algtest/info_board"

# prepare raw file names
metric_file <- c("compare_chest_organs")
metric_file_path = paste(result_dir, paste(metric_file, 'csv', sep = '.'), sep = '/')

# import statistical functions from plot_mate.R for later use
setwd(code_dir)
source("plot_mate.R")
setwd(result_dir)
# set working directory where data is stored 

# plot segmentation accuracy
acc_data_all <- read.csv(metric_file_path)

# Error bars represent standard error of the mean
ggplot(acc_data_all, aes(x=pred_rois, y=dice_mean, fill=method)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+
  geom_errorbar(aes(ymin=dice_mean-dice_std, ymax=dice_mean+dice_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  theme_bw()+
  theme_classic()

ggsave(paste(result_dir, "dice_by_methods_chest", "pdf", sep = '.'),
       width = 5, height = 5, units = "in")