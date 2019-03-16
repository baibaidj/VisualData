# function to get the mean std and sem of a data frame  
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

# import necessary packages for later use
library(ggplot2)
library(ez)
library(reshape2)


# set working directory and read data from files 
setwd("/Users/kalelshi/Desktop/PsyLabs/dcnlstudy/result")#sets working directory to Desktop on a mac 
#the tilde replaces /Users/monicaburns on a mac
triadd<- read.csv("sub_behavior_by_trials.csv") #brings in the data file
head(triadd) #shows you the first six rows of data - quick check to see if there are any obvious problems
str(triadd) #shows the structure of each 


# adjust the dataframe to make it more usabel later
triadd[is.na(triadd)] <- 0
names(triadd)[1]<-"instruction"

triadd$instruction <- factor(triadd$instruction, 
                             levels= c(1,2),
                             labels = c("single_task", "duel_task"))
triadd$sub <- factor(triadd$sub)

triadd$trialtype <- factor(triadd$trialtype,
                           levels= c(1,2),
                           labels = c("salient_distractor", "nonsalient_distractor"))
triadd$instruction_order <- factor(triadd$instruction_order,
                       levels= c(1,2),
                       labels = c("single_duel", "duel_single"))

# propotion of fixation duration to distractors over the total fixation duration 
triadd$percent_fix2distractor <- triadd$fixduration_distractor/triadd$fixduration_total

# propotion of number of trials with fixation to distractor over the total number of trials with fixations
triadd$percent_trial2distractor <- triadd$num_trial2distractor/triadd$num_active_trials

write.csv(triadd, file = 'fixation2distraction_cleandata.csv')

# correlation between different gaze measure
codata <- triadd[,c(4:7,9)]
cor(codata, use="complete.obs", method = "pearson")


# test the difference between two trialtype, salience and nonsalience
t.test(percent_fix2distractor ~ trialtype, data = triadd, paired = TRUE) 
t.test(percent_trial2distractor ~ trialtype, data = triadd, paired = TRUE) 


# get the inferential statistics for the two dependent variables 
# doing 1*2 anova
fixation_anova = ezANOVA(
  data = triadd
  , dv = percent_fix2distractor
  , wid = sub
  , within = .(instruction, trialtype)
  , between = instruction_order
)
fixation_anova

trial_anova = ezANOVA(
  data = triadd
  , dv = percent_trial2distractor 
  , wid = sub
  , within = .(instruction, trialtype)
  , between = instruction_order
)
trial_anova



# get the descriptive statistics for the two dependent variables
duration_summary <- summarySE(triadd, measurevar="percent_fix2distractor", 
                              groupvars=c("instruction_order","instruction","trialtype"),na.rm = TRUE) # "order",
activetrial_summary  <- summarySE(triadd, measurevar="percent_trial2distractor", 
                                  groupvars=c("instruction_order","instruction","trialtype"),na.rm = TRUE) # "order",
duration_summary
activetrial_summary

# prepare for visualization. 
# replace activetrial_summary for duration_summary to get the corresponding result
activetrial_summary$condition <- c(1,1,2,2,4,4,3,3)
activetrial_summary$condition <- factor(activetrial_summary$condition, 
                       levels= c(1,2,3,4),
                       labels = c("order1.single", "order1.duel","order2.duel","order2.single")
                      )

names(activetrial_summary)[3] <- "Trial_Condition"
ggplot(activetrial_summary, aes(x=condition, y=percent_trial2distractor, color=Trial_Condition)) +  # color dictates the border
  geom_bar(position=position_dodge(), stat="identity", width = .5, fill = NA) + # fill dictates bodyfill=NA
  geom_errorbar(aes(ymin=percent_trial2distractor-se, ymax=percent_trial2distractor+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.5))+
  labs( 
    # x = "Instructions for the two runs",
    x ="Order of the two runs with different instructions",
    y = "Proportion of active trials with fixation on distractors"
    # y = "Proportion of fixation duration on distractors"
    )+
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=15),
        axis.text.x  = element_text(vjust=0.5, size=12), #angle=90, 
        axis.title.y = element_text(face="bold", size=15),
        axis.text.y  = element_text(vjust=0.5, size=12),
        legend.title=element_text(face="bold", size=12),
        legend.text = element_text(size = 12),
        #legend.justification=c(1,0),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.3,0.8))+
        #legend.title=element_blank()
  guides(fill = guide_legend(title = "Trial_Condition", title.position = "left"))

ggsave("trials_by_3DV.pdf")



# correlation between gaze measure with anxiety level and ef
# long format 2 wide format
temp<- triadd[,c(1:3,9)]
dvdata <- dcast(temp, sub~ instr+trialtype, value.var = "tria2dis.per")
names(dvdata)[1:5] <- c("sub","single.sal","single.nonsal","duel.sal","duel.nonsal")

library(psych)

selfrep<- read.csv("survey.csv") 
codata <- merge(selfrep, dvdata, by="sub")

temp <- codata[, c(2,3,7,8,11:14)]

str(temp)

cor(temp, use="complete.obs", method = "pearson")
corr.test(temp)
#

options(contrasts = c("contr.sum","contr.poly"))

aov.oit <-  aov(disfixlen ~ (order*instr*trialtype) + Error(sub/(instr*trialtype)), data=triadd, contrasts = contr.sum)

aov.oit <-  aov(tria2dis.per ~ (instr*trialtype) + Error(sub/(instr*trialtype)), data=triadd, contrasts = contr.sum)
summary(aov.oit)
model.tables(avo.oit, "means")
plot(avo.oit)

options(contrasts = c("contr.sum","contr.poly"))
aov.ot <- aov(disfixper ~ (order*instr*trialtype) +  Error(sub/(trialtype*instr)),
              data = triadd, contrasts = contr.sum)


model.tables(aov.ot, "means")
summary(aov.ot)

poshoc <- TukeyHSD(aov.ot)

triadd <- triadd[order(triadd$order), ]
salbyorder <- t.test(peri ~ order, data = subset(triadd, triadd$trialtype == 1))
nonsalbyorder <- t.test(peri ~ order, data = subset(triadd, triadd$trialtype == 2))
fibytype <- t.test(disfixper ~ trialtype, data = subset(triadd, triadd$instr == 2), paired=TRUE)
sebytype <- t.test(peri ~ trialtype, data = subset(triadd, triadd$order == 2), paired=TRUE)



# construct a distraction index using the number-of-trial-
# with-fixation-on-distractor difference between nonsalient and salient conditions
dvdata <- matrix(nrow = 32, ncol = 4)
m = 1
for(i in 1:2){
  temp <- subset(triadd, triadd$trialtype ==i)
  for(j in 201:216)
  {
    temp1 <- subset(temp, temp$sub ==j)
    dvdata[m,1] <- i # trialtype
    dvdata[m,2] <- j # sub
    dvdata[m,3] <- temp1[1,10] # order
    dvdata[m,4] <- temp1$tria2dis.per[temp1$instr==2]
    dvdata[m,5] <- temp1$tria2dis.per[temp1$instr==1]
    m = m+1
  }
}
dvdata <-data.frame(dvdata)
names(dvdata)[1:4] <- c("trialtype","sub","order","fixdif")
dvdata$instr <- factor(dvdata$instr)
dvdata$order <- factor(dvdata$order)
dvdata$sub <- factor(dvdata$sub)