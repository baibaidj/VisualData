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
========================================
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
========================================

# data input and select
## set working directory where data is stored 
kRes.wd <- 
  "/Users/kalelshi/Desktop/repproject/Results"  #change 
setwd(kRes.wd)

## read data file into the environment
rawdata <- read.csv("newmatch.exclusion.3.7.csv") # vtphas.csv

## change the name of the columns/features/variables to call them later easily
names(rawdata)[1]<-"group"
names(rawdata)[3]<-"mv"
names(rawdata)[4]<-"RPI"

## change the values or data types of features 
mvlab = c("dfly", "bee", "bug", "frog", "turtle", "btfly", "bird", "car")
rawdata$mv <- factor(rawdata$mv,
                     levels = c(1:8),
                     labels = mvlab)
rawdata$group <- factor(rawdata$group,
                     levels = c(1:2),
                     labels = c("ASD", "TD"))
        

## transform the data between wide form (used by SPSS) and long form (used by R)
## http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

scrntime.data <- gather(rawdata[,c(1,2,18:20)], condition, measurement, STpct1:STpct3) 
stawide <- spread(rawdata, mv, RPI)


# Statistical analysis

## test if subjects' screen-looking time change with time 
scrntime.data <- gather(rawdata[,c(1,2,18:20)], condition, measurement, STpct1:STpct3) 
aov_time <- aov(measurement ~ condition * group + Error(sub/condition), data=scrntime.data)
summary(aov_time)
http://www.cookbook-r.com/Statistical_analysis/ANOVA/ 

###plot means and error bars using ggplot
stlong <- gather(rawdata[,c(1,2,18:20)], condition, measurement, STpct1:STpct3)
sumst <- summarySE(stlong[,c(1,3,4)], measurevar="measurement", groupvars=c("group","condition"))
p_sct<- ggplot(sumst, aes(x= condition, y=measurement , colour=group, group=group))+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se),position=position_dodge(0),
                size = 1, width = .1)+ 
  geom_point(aes(shape = group), size=5)+
  scale_colour_manual(values = c("#5394db","#fc8098"))+ #  scale_fill_manual
  scale_y_continuous(breaks=seq(0.35,0.75,by=0.1), limits = c(0.35,0.75)) +
  labs(x = "Phase", 
       y =  "Proportional Screen-looking Time")+ #"Repetitive Preference Index (RPI)")+
  scale_x_discrete(breaks=c("1", "2","3"),
                   labels=c("Early", "Middle", "Late"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        legend.position=c(0.8,0.9),
        legend.justification=c(0.5,0.7)
        )
ggsave("Fig2.Screentime.pdf") # Fig4.RPIbymotype, Fig2.screenlooking


## Compare subjects' RPI for the two motion types: linear and circular. 
motydata <- rawdata[, c(1:2)]
motydata$lineRPI <- apply(rawdata[,c(11:12)], 1, mean, na.rm = TRUE) 
motydata$circleRPI <- apply(rawdata[,c(8:10, 14:16)], 1, mean, na.rm = TRUE)
mtldata<- gather(motydata, condition, measurement, lineRPI:circleRPI)
### 2*2 mixed ANOVA
aov_motion <- aov(measurement ~ condition * group + Error(sub/condition), data=mtldata)
summary(aov_motion)

### Plotting means and error bars using ggplot
mtrpi <- summarySE(mtldata, measurevar="measurement", groupvars=c("group","condition"), na.rm= TRUE)
p_mo <- ggplot(mtrpi, aes(x = condition, y = measurement, fill = group))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=measurement-se, ymax=measurement+se),position=position_dodge(.9),
                size = 1, width = .1)+ 
  scale_fill_manual(values = c("#5394db","#fc8098"))+ #  scale_colour_manual
  scale_y_continuous(breaks=seq(0,0.7,by=0.1), limits = c(0,0.7)) +
  geom_hline(yintercept = 0.5, lty="dashed",size = 0.5)+
  labs(x = "Types of Motion",
       y = "Repetitive Preference Index (RPI)")+ 
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        legend.position="top",
        legend.justification=c(0.5,0.7)
        #legend.position=c(0.8,0.9)
        )

ggsave("Fig4.RPIbymotype.pdf") # Fig4.RPIbymotype, Fig2.screenlooking

##  scatter plot of RPI of two groups by two AOIs
RPIscatter <- rawdata[, c("group","sub","dynRPI","staRPI")]
RPIlong <- gather(RPIscatter, condition, measurement, dynRPI:staRPI, factor_key=TRUE)
RPIlong$V5 <- NULL
RPIlong[RPIlong$group ==1 & RPIlong$condition =="dynRPI" ,5] <- c(1.5)
RPIlong[RPIlong$group ==2 & RPIlong$condition =="dynRPI" ,5] <- c(2.5)
RPIlong[RPIlong$group ==1 & RPIlong$condition =="staRPI" ,5] <- c(3.5)
RPIlong[RPIlong$group ==2 & RPIlong$condition =="staRPI" ,5] <- c(4.5)
RPIlong$condition <- factor(RPIlong$condition,
        levels = c("dynRPI","staRPI"),
        labels = c("Dynamic AOI", "Static AOI"))
        
p_aoi <- ggplot(RPIlong, aes(group, measurement))+
  geom_jitter(aes(shape = group), size = 3, width = 0.1)+ # plot data points of all participants
  scale_shape_manual(values=c(1,17))+ # decide the shape of the data points for each group
  stat_summary(fun.data="mean_se", fun.args = list(mult=1), 
               geom="crossbar", width=0.3, color = "red")+ # plot mean and error bar in the middle
  facet_grid(. ~ condition, switch = "x") + # divide the plot into two parts by conditions 
  labs(y = "Repetitive Preference Index (RPI)", shape = "Group")+
  scale_y_continuous(breaks=seq(0.4,0.7,by=0.05))+  #  decide the data used for plotting
  coord_cartesian(ylim = c(0.4,0.7))+  # only influence how much graph you present
  geom_hline(yintercept = 0.5, lty="dashed",size = 1)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        legend.title=element_text(face="bold", size=12),
        legend.text = element_text(size = 12),
        legend.position="top",
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        strip.text.x = element_text(face = "bold", vjust=0.5, size=16),
        strip.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )+
  guides(shape =  guide_legend(override.aes = list(size=4))) # change the text size of the legend

ggsave("Fig3.RPIgrAOI.pdf")


## temporal analysis of RPI by three phases
tempodata <- read.csv("temporal_data.csv")
tempodata[,1] <- c(1,2,3,1,2,3)
tempodata$phase <- as.numeric(tempodata$phase)

pd <- position_dodge(0) # move them .05 to the left and right
ggplot(tempodata, aes(x=phase, y=Mean, colour =Group)) + 
  geom_hline(yintercept = 0.5, lty="dashed",size =1) +
  geom_point(aes(shape = Group), size = 5)+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),
                width=.1, size = 1) +
  scale_colour_manual(values = c("#5394db", "#fc8098"))+
  scale_shape_manual(values=c(1,17))+
  geom_line(position=pd, size = 1)+
  labs(x = "Phase", y="Repetitive Preference Index (RPI)", shape = "Group") +  # Use darker colors, lightness=40
  scale_y_continuous(breaks=seq(0.4,0.65,by=0.05), limits=c(0.4,0.65)) +
  #scale_x_continuous(breaks=seq(0, 4, 1), limits=c(0.5,3.5))+
  scale_x_discrete(limit = c(1, 2, 3),labels=c("Early","Middle","Late"))+# Set tick every 4
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        legend.title=element_blank(),
        legend.text = element_text(size = 12),
        legend.justification=c(1,0),
        legend.position=c(0.95,0.05),
        #legend.key.size = unit(1, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank() 
        #axis.line = element_line(colour = "black")
  )+
  guides(shape = guide_legend(override.aes = list(linetype = 0,size=5)))
  
  ggsave("Fig4.TempRPI.dyn.pdf")

## correlation between RPI and other measures
ASD.data <- rawdata[rawdata$group=="ASD", c("dynRPI","ADIR_RRB", "group", "age", 
                         "ADOS_RRB", "RBS_total","SCQ_RSB")]

### generate correlation matrix for multiple variables
corr_coef <- cor.test(ASD.data, method = "pearson", conf.level = 0.95)  
print(corr_coef, digits=4)

### only calculate a single correlation between two variables
RPI_RRB.co <- cor.test(ASD.data$dynRPI, ASD.data$ADIR_RRB, method = "pearson", conf.level = 0.95) 
print(RPI_RRB.co, digits=4)

r <- ggplot(ASD.data, aes(x=dynRPI, y=ADIR_RRB), na.rm=FALSE)+
  geom_point(size =2)+
  scale_shape(solid = FALSE)+
  geom_smooth(method="lm", formula = y~x)+
  labs(x = "Repetitive Preference Index (RPI)", #Repetitive Preference Index (RPI)", 
       y = "ADIR RRB subscale")+
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
        #scale_x_continuous(breaks=seq(0,6,by=1), limits = c(2,6)),
        #legend.justification=c(1,0),
        #legend.position=c(0.95,0.05),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank() 
  )+
  geom_hline(yintercept = 3)+
  geom_vline(xintercept = 0.5)
ggsave("RPI_ADIRRRB.pdf")



## reliability test #cronbach alphas  #split half
temp <- rawdata[1:19,c(8:15)]
psy::cronbach(temp)
psych::alpha(temp)

#split half
 
shreli <- splithalf.r(na.omit(relidata[,3:10]), sims = 1000, graph = TRUE, seed = 2)
