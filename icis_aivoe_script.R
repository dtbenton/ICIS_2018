library(lme4)
library(boot)
library(car) 
library(nlme)
library(reshape2)
library(ggplot2)
library(ez)
library(plyr)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
options(scipen=9999)
#################################################################
#### load data and create data sets for all three age groups ####
#################################################################
# 4-month-old aivoe data frame
D = read.csv(file.choose(), header = TRUE)
D_tall = reshape(D, varying = 4:5, v.names = "measure", timevar = "test.trial", idvar = "ID", new.row.names = 1:26, direction = "long")
D_tall = D_tall[order(D_tall$ID),]
dim(D)

# 7-month-old aivoe data frame
J = read.csv(file.choose(), header = TRUE)
J_tall = reshape(J, varying = 4:5, v.names = "measure", timevar = "test.trial", idvar = "ID", new.row.names = 1:48, direction = "long")
J_tall = J_tall[order(J_tall$ID),]
dim(J)

# 7-month-old aivoe_manip data frame 
K = read.csv(file.choose(), header = TRUE)
K_tall = reshape(K, varying = 4:5, v.names = "measure", timevar = "test.trial", idvar = "ID", new.row.names = 1:34, direction = "long")
K_tall = K_tall[order(K_tall$ID),]
dim(K)

# 11-month-old aivoe-manip data frame
L = read.csv(file.choose(), header = TRUE)
L_tall = reshape(D, varying = 4:5, v.names = "measure", timevar = "test.trial", idvar = "ID", new.row.names = 1:90, direction = "long")
L_tall = L_tall[order(L_tall$ID),]
dim(L)

#############################
#### assumption checking ####
#############################
## homogeneity of variance

# formal test: 
# 4 mos aivoe
# range: 3 mos 18 days to 4 mos 14 days
levene.4mosaivoe.inanim = leveneTest(D_tall$measure[D_tall$condition==0], D_tall$test.trial[D_tall$condition==0],  center=mean)  
levene.4mosaivoe.anim = leveneTest(D_tall$measure[D_tall$condition==1], D_tall$test.trial[D_tall$condition==1],  center=mean)                                                                         


# 7 mos aivoe
# range: 6 mos 1 days to 7 mos 11 days
levene.7mosaivoe.inanim = leveneTest(J_tall$measure[J_tall$condition==0], J_tall$test.trial[J_tall$condition==0],  center=mean)  
levene.7mosaivoe.anim = leveneTest(J_tall$measure[J_tall$condition==1], J_tall$test.trial[J_tall$condition==1],  center=mean)                                                                         

# 7 mos aivoe_manip
levene.7mosaivoe.manip.inanim = leveneTest(K_tall$measure[K_tall$condition==0], K_tall$test.trial[K_tall$condition==0],  center=mean)  
levene.7mosaivoe.manip.anim = leveneTest(K_tall$measure[K_tall$condition==1], K_tall$test.trial[K_tall$condition==1],  center=mean)                                                                         

# 11 mos aivoe_manip
# range: 10 mos 16 days to 11 mos 6 days
levene.11mosaivoe.manip.inanim = leveneTest(L_tall$measure[L_tall$condition==0], L_tall$test.trial[L_tall$condition==0],  center=mean)  
levene.11mosaivoe.manip.anim = leveneTest(L_tall$measure[L_tall$condition==1], L_tall$test.trial[L_tall$condition==1],  center=mean)                                                                         

## Normality
normality_check_func = function(dataframe){
  par(mfrow=c(1,2))
  for(i in 1:2){
    hist(dataframe$measure[dataframe$test.trial==i & dataframe$condition==0], breaks=21)
  }
  shapiro_ps = rep(0,2)
  for(i in 1:2) {
    test = shapiro.test(dataframe$measure[dataframe$test.trial==i & dataframe$condition==0]) 
    shapiro_ps[i] = test$p.value
  }
  shapiro_ps
}


normality_func = function(dataframe){
  shapiro_ps = rep(0,4)
  k=1
  for(i in 0:1){
    for(j in 1:2){
      test = shapiro.test(dataframe$measure[dataframe$test.trial==j & dataframe$condition==i]) 
      shapiro_ps[k] = test$p.value
      k = k+1
    }
  }
  shapiro_ps
}


# 4 mos aivoe
normality_func(D_tall)

# 7 mos aivoe
normality_func(J_tall)

# 7 mos aivoe_manip
normality_func(K_tall)

# 11 mos aivoe_manip
normality_func(L_tall)

################################
####################################################################
###################################################################################
####  PRELIMINARY ANALYSES   ####
###################################################################################
####################################################################
################################
t_test_func = function(dataframe,condition_iter){
 t_test =  t.test(dataframe$measure[dataframe$condition==condition_iter & dataframe$test.trial==1], 
                  dataframe$measure[dataframe$condition==condition_iter & dataframe$test.trial==2], 
                  paired=TRUE)
 return(t_test$p.value)
 }


## 4 MOS AIVOE: C(inanimate_object, animate_object):
# Inanimate
  # Collision: 6.75
  # No-collision: 9.77
  # P-value: p < .001

# Animate
  # Collision: 5.29
  # No-collision: 7.2
  # P-value: p < .001


## 7 MOS AIVOE: C(inanimate_object, animate_object):
  # Inanimate
    # Collision: 6.5
    # No-collision: 8.4
    # P-value: p = .04

  # Animate
    # Collision: 6.33
    # No-collision: 6.01
    # P-value: p = .6


## 7 MOS AIVOE-MANIP: C(inanimate_object, animate_object):
  # Inanimate
    # Collision: 6.87
    # No-collision: 7.57
    # P-value: < .001

  # Animate
    # Collision: 8.29
    # No-collision: 5.79
    # P-value: p < .01


## 11 MOS AIVOE-MANIP: C(inanimate_object, animate_object):
  # Inanimate
    # Collision: 6.75
    # No-collision: 9.77
    # P-value:  p < .001

  # Animate
    # Collision: 5.29
    # No-collision: 7.2
    # P-value: p < .001

# 4 MOS AIVOE: C(inanimate_object, animate_object): 
t_test_func(D_tall,0)
t_test_func(D_tall,1)

# 7 MOS AIVOE: C(inanimate_object, animate_object): 
t_test_func(J_tall,0)
t_test_func(J_tall,1)

# 7 MOS AIVOE-MANIP: C(inanimate_object, animate_object): 
t_test_func(K_tall,0)
t_test_func(K_tall,1)

# 11 MOS AIVOE-MANIP: C(inanimate_object, animate_object): 
t_test_func(L_tall,0)
t_test_func(L_tall,1)



########################################################
########################################################
########################################################
#############                              #############
#############            Figures           #############
#############                              #############
########################################################
########################################################
########################################################
### 4 MONTH OLDS ###

# Create 'F_tall' data frame to use for ggplot
F_tall = D_tall

# rename levels of 'condition' and 'q.type.cat' factors
F_tall$condition = revalue(x = as.factor(F_tall$condition), 
                           c("0" = "Inanimate Object", "1"="People"))
F_tall$test.trial = revalue(x = as.factor(F_tall$test.trial), 
                            c("1" = "Collison", "2"="No Collision"))


# OMNIBUS ANALYSIS FIGURE
condition_barplot = ggplot(F_tall, aes(condition, measure, fill = test.trial)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  #facet_wrap(~q.type.cat, scales="free") + # create as many separate graphs as there are conditions 
  annotate("text",x=1,y=15.2,label="*", size=10) +
  geom_segment(aes(x = .8, y = 15, xend = 1.2, yend = 15)) +
  annotate("text",x=2,y=15.2,label="*", size=10) +
  geom_segment(aes(x = 1.8, y = 15, xend = 2.2, yend = 15)) +
  ylab("Looking time (s)") + # change the label of the y-axis
  # PERCEPTUAL SIGNIFICANCE LINES
  theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + # remove the major and minor grids
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 18)) +
  theme_classic() +
  scale_fill_manual(values=c("#000000", "#999999")) +
  theme(strip.background =element_rect(fill='black')) +
  theme(strip.text = element_text(colour = 'white', size = 12, face = "bold")) +
  theme(axis.title=element_text(size="12"),axis.text=element_text(size=12)) + 
  theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6)) +
  theme(legend.text = element_text(size = 12)) + 
  theme(legend.title=element_blank()) +
  labs(x = "Condition")




### 7 MONTH OLDS AIVOE ###
# Create 'F_tall' data frame to use for ggplot
F_tall = J_tall

# rename levels of 'condition' and 'q.type.cat' factors
F_tall$condition = revalue(x = as.factor(F_tall$condition), 
                           c("0" = "Inanimate Object", "1"="People"))
F_tall$test.trial = revalue(x = as.factor(F_tall$test.trial), 
                            c("1" = "Collison", "2"="No Collision"))


# OMNIBUS ANALYSIS FIGURE
condition_barplot = ggplot(F_tall, aes(condition, measure, fill = test.trial)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  #facet_wrap(~q.type.cat, scales="free") + # create as many separate graphs as there are conditions 
  annotate("text",x=1,y=11.8,label="*", size=10) +
  geom_segment(aes(x = .8, y = 11.6, xend = 1.2, yend = 11.6)) +
  ylab("Looking time (s)") + # change the label of the y-axis
  # PERCEPTUAL SIGNIFICANCE LINES
  theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + # remove the major and minor grids
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 18)) +
  theme_classic() +
  scale_fill_manual(values=c("#000000", "#999999")) +
  theme(strip.background =element_rect(fill='black')) +
  theme(strip.text = element_text(colour = 'white', size = 12, face = "bold")) +
  theme(axis.title=element_text(size="12"),axis.text=element_text(size=12)) + 
  theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6)) +
  theme(legend.text = element_text(size = 12)) + 
  theme(legend.title=element_blank()) +
  labs(x = "Condition")


### 7 MONTH OLDS AIVOE-Manip ###
# Create 'F_tall' data frame to use for ggplot
F_tall = K_tall

# rename levels of 'condition' and 'q.type.cat' factors
F_tall$condition = revalue(x = as.factor(F_tall$condition), 
                           c("0" = "Inanimate Object", "1"="People"))
F_tall$test.trial = revalue(x = as.factor(F_tall$test.trial), 
                            c("1" = "Collison", "2"="No Collision"))


# OMNIBUS ANALYSIS FIGURE
condition_barplot = ggplot(F_tall, aes(condition, measure, fill = test.trial)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  #facet_wrap(~q.type.cat, scales="free") + # create as many separate graphs as there are conditions 
  ylab("Looking time (s)") + # change the label of the y-axis
  # PERCEPTUAL SIGNIFICANCE LINES
  theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + # remove the major and minor grids
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 18)) +
  theme_classic() +
  scale_fill_manual(values=c("#000000", "#999999")) +
  theme(strip.background =element_rect(fill='black')) +
  theme(strip.text = element_text(colour = 'white', size = 12, face = "bold")) +
  theme(axis.title=element_text(size="12"),axis.text=element_text(size=12)) + 
  theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6)) +
  theme(legend.text = element_text(size = 12)) + 
  theme(legend.title=element_blank()) +
  labs(x = "Condition")


### 11 MONTH OLDS AIVOE-Manip ###
# Create 'F_tall' data frame to use for ggplot
F_tall = L_tall

# rename levels of 'condition' and 'q.type.cat' factors
F_tall$condition = revalue(x = as.factor(F_tall$condition), 
                           c("0" = "Inanimate Object", "1"="People"))
F_tall$test.trial = revalue(x = as.factor(F_tall$test.trial), 
                            c("1" = "Collison", "2"="No Collision"))


# OMNIBUS ANALYSIS FIGURE
condition_barplot = ggplot(F_tall, aes(condition, measure, fill = test.trial)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  #facet_wrap(~q.type.cat, scales="free") + # create as many separate graphs as there are conditions 
  annotate("text",x=1,y=15.5,label="*", size=10) +
  geom_segment(aes(x = .8, y = 15.3, xend = 1.2, yend = 15.3)) +
  ylab("Looking time (s)") + # change the label of the y-axis
  # PERCEPTUAL SIGNIFICANCE LINES
  theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + # remove the major and minor grids
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 18)) +
  theme_classic() +
  scale_fill_manual(values=c("#000000", "#999999")) +
  theme(strip.background =element_rect(fill='black')) +
  theme(strip.text = element_text(colour = 'white', size = 12, face = "bold")) +
  theme(axis.title=element_text(size="12"),axis.text=element_text(size=12)) + 
  theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6)) +
  theme(legend.text = element_text(size = 12)) + 
  theme(legend.title=element_blank()) +
  labs(x = "Condition")

