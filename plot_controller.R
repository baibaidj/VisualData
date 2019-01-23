# statistics and plot controller

rm(list=ls())  ##clear workspace

# prepare fundamental dir
code_dir <- '/media/dejun/holder/gitlinux/VisualData'
result_dir <- "/media/dejun/holder/algtest/info_board"
version = c('1.1.3.1')

# prepare raw file names
metric_file <- c("metric_final")
segtime_file <- c('segment_runtime')
classtime_file <- c('classify_runtime')
version_dir <- paste(result_dir, version, sep = '/')

metric_file_path = paste(version_dir, paste(version, metric_file, 'csv', sep = '.'), sep = '/')
segtime_file_path = paste(version_dir, paste(version, segtime_file, 'csv', sep = '.'), sep = '/')
classtime_file_path = paste(version_dir, paste(version, classtime_file, 'csv', sep = '.'), sep = '/')

# import statistical functions from plot_mate.R for later use
setwd(code_dir)
source("plot_mate.R")
source("plot_model_accuracy.R")
source("plot_runtime.R")
source("plot_classify_time.R")
setwd(version_dir)
# set working directory where data is stored 

# plot segmentation accuracy
model_accuracy_scatters(metric_file_path)

# plot segmentation efficiency
seg_runtime_plot(segtime_file_path)

# plot classification efficiency
class_runtime_plot(classtime_file_path)


