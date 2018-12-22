## import statistical functions from plot_mate.R for later use
rm(list=ls())  ##clear workspace
code.dir <- '/media/dejun/holder/gitlinux/VisualData'
setwd(code.dir)
source("plot_mate.R")

##=================================================================================##
##########bar plot of the time algorithm used to segment each organ##################
##=================================================================================##
# this.dir <- dirname(parent.frame(2)$ofile)

data_dir = "/media/dejun/holder/datavisualizeR/data"
setwd(data_dir)

file_names = list.files(data_dir, pattern = '.xls$')
file_path = paste(data_dir, file_names, sep="/")

n = length(dir)

new_1<-read_xls(file_path)

valid_data = new_1[-c(dim(new_1)[1]),]
stlong <- gather(valid_data, hospital, scores, c(2:(dim(new_1)[2])))

# Extend the regression lines beyond the domain of the data
ggplot(stlong, aes(x=organs, y=scores, color=hospital)) + 
  geom_point(shape=1) +
  scale_colour_hue(l=50) # Use a slightly darker palette than normal
  
