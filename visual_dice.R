# function: install new packages and their dependencies
ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

## new packages can be added here
packages <- c("reshape2", "plyr", "sandwich", "nlme", "effects", "binom", "doBy", "grid",
              "boot", "ggplot2", "scales", "lme4", "bootstrap", "car","arm","mfx","psych",
              "devtools", "data.table", "stringr", "stats", "QuantPsyc", "lsmeans", "tidyr",
              "multicon")
ipak(packages)
##========================================
# function: Get the descrptive statistics of a data set: 
## including mean, sd, sem, by between-subject variable and within-subject variable
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
##========================================
  
# data input and select
## set working directory where data is stored 
kRes.wd <- "/media/dejun/holder/datavisualizeR/data"  #change 
setwd(kRes.wd)

## read data file into the environment
rawdata <- read.csv("accuracy_all.csv") # vtphas.csv

valid_rawdata = rawdata[!is.na(rawdata$Value),]

## from wide data format to long data format
# stlong <- gather(rawdata[,c(1,2,18:20)], condition, measurement, STpct1:STpct3)

## from long data format to wide data format
stawide <- spread(valid_rawdata, Metric, Value)
stawide$Pid <- factor(stawide$Pid)
stawide$Organs <- factor(stawide$Organs)
stawide$hosp <- factor(stawide$hosp)
names(stawide)[5]<-"dice"

###plot means and error bars using ggplot
# stawide[,c(1:4)]
sumst <- summarySE(stawide, measurevar="dice", groupvars=c( "Organs"))
sumst
# dev.new()

# ggplot(data=sumst, aes(x=Organs, y=dice)) +
#   geom_bar(stat="identity")
# sumst[!is.na(sumst$dice), ]

ggplot(data = sumst, aes(x= Organs, y=dice, fill=Organs), na.rm = true)+
  geom_bar(colour="black", stat="identity")+
  geom_errorbar(aes(ymin=dice-se, ymax=dice+se),
                position=position_dodge(0.9),
                width = .2)+  #size = 1,
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        # legend.title=element_blank()
        # legend.position=c(0.8,0.9),
        # legend.justification=c(0.5,0.7)
  )


ggsave("Fig2.Screentime.pdf") 


##  scatter plot of RPI of two groups by two AOIs
dicedata <- stawide[, c(1:5)]
dicedata_valid = dicedata[dicedata$dice>0.5,]
ggplot(dicedata_valid, aes(Organs, dice))+
  geom_boxplot( outlier.shape = NA, colour = "#3366FF", fill = "white")+
  #outlier.shape = NA, colour = "#3366FF", aes(colour = Organs), outlier.colour = "black", outlier.shape = 1
  geom_jitter(aes(colour = hosp), size = 1.5, width = 0.2)+ # plot data points of all participants
  scale_shape_manual(values=c(1,17))+ # decide the shape of the data points for each group
  # stat_summary(fun.data="mean_se", fun.args = list(mult=1),   # plot mean and error bar in the middle
  #              geom="crossbar", width=0.3, color = "red")+
  labs(y = "Dice Coefficient")+
  scale_y_continuous(breaks=seq(0.4,1.2,by=0.1))  +  #  decide the data used for plotting
  # coord_cartesian(ylim = c(0.4,0.7))+  # only influence how much graph you present
  # geom_hline(yintercept = 0.5, lty="dashed",size = 1)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(face = "bold", angle=45, vjust =0.7, size=12), #vjust =0,
        
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        # legend.position="none",
        legend.title=element_text(face="bold", size=12),
        legend.text = element_text(size = 12),
        legend.position="top",
        # strip.text.x = element_text(face = "bold", vjust=0.5, size=16),
        strip.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )
  # coord_flip()

  guides(shape =  guide_legend(override.aes = list(size=4))) # change the text size of the legend

ggsave("dice_by_organ_hosp2.pdf")


