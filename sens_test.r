library(ggplot2)
library(httpgd)
library(data.table)
library(pwr)
library(MASS)
# make df of all data
## in the future, use MEAN bird-love and run 1000 times 
## regardless if birds die off
df <- fread("nik_files_large/10-24-2.csv", skip=6)
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("-", "_", names(df))
names(df) <- gsub("\\[|\\]", "", names(df))
names(df) <- gsub("count_patches_with_*", "", names(df))
names(df) <- gsub("*=_*", "", names(df))
head(df)
tail(df)
##  Average the replicate runs

avgDF <- aggregate(count_turtles ~ mean_vegetation_volume +
    max_bird+dispersal_distance+habitat_radius, df, mean)
head(avgDF)
tail(avgDF)

## works for just one y variable
##  Aggregate multiple y
avgDF <- aggregate(cbind(count_turtles, mean_habitat_of_patches) ~ step +
    mean_vegetation_volume + max_bird + dispersal_distance +
    habitat_radius, df, mean)
head(avgDF)
tail(avgDF)
avgDF$habitat_sum <- avgDF$mean_habitat_of_patches * 441 # turn avg into sum

# check for violations
ggplot(avgDF, aes(x=habitat_sum)) +
    geom_histogram()
##  Not normal, RIGHT SKEW
# transform
avgDF$hab_trans <- avgDF$habitat_sum
ggplot(avgDF, aes(x=hab_trans)) +
    geom_histogram()

ggplot(avgDF, aes(x=count_turtles)) +
    geom_histogram()
##  NORMAL


#   ANOVA   #
anova1 <- lm(count_turtles ~ max_bird + mean_vegetation_volume +
    max_bird * mean_vegetation_volume, data=avgDF)
summary(anova1)
anova_all <- anova(anova1)
anova_all
## can I simply add variables together to do an anova on each,
## or must it be done separately?

anova2 <- lm(count_turtles ~ mean_vegetation_volume, avgDF)
summary(anova2)
## plot x vs y
png(file = "nik_files/vegxbirdsy.png")
ggplot(avgDF, aes(x=mean_vegetation_volume,y=count_turtles)) +
    geom_point() +
    geom_smooth(method = lm)
dev.off()
## plot anova to pdf
png(file = "nik_files/plot_test.png")
layout(matrix(1:4, ncol = 2))
plot(anova2)
layout(1)
dev.off()

anova_start_veg <- anova(anova2)
anova_start_veg

anova3 <- lm(count_turtles ~ dispersal_distance, avgDF)
summary(anova3)
anova_disp <- anova(anova3)
anova_disp

anova4 <- lm(count_turtles ~ habitat_radius, avgDF)
summary(anova4)
anova_hab <- anova(anova4)
anova_hab




## works for multiple response variables at once including steps
test <- avgDF[mean_vegetation_volume==3.0 & max_bird==5 &
    dispersal_distance==2 & habitat_radius==3, step]

#   Birds over time | veg=3, dist=2,rad=2, max=5
ot <- df[mean_vegetation_volume==3 &
    dispersal_distance==2 &
    habitat_radius==2 &
    max_bird==5,
    .(step,count_turtles)]

ggplot(data = ot,aes(x=step,y=count_turtles)) +
    geom_point()

ot1 <- df[mean_vegetation_volume==5 &
    dispersal_distance==5 &
    habitat_radius==5 &
    max_bird==3,
    .(step,count_turtles)]

ggplot(data = ot1,aes(x=step,y=count_turtles)) +
    geom_point()

#   max bird on count_turtles
max <- df[mean_vegetation_volume==3 &
    dispersal_distance==2 &
    habitat_radius==2,
    .(max_bird,count_turtles)]
head(max)
max(max)


# plot of max bird vs bird count mean veg = 4 dispersal = 2
# radius = 2
ggplot(data=max,aes(x=max_bird, y=count_turtles)) +
    geom_point() +
    xlim(0,15) +
    ylim(0,3162)
## unlikely survivorship when max-bird = 1,2


#   How does mean bird_love affect habitat
# need to calculate distribution of bird love at each step
# this is really hard, have NetLogo calculate it next time......
meanBird <- df[1,8:18]/441
meanBird <- df[1,8:18] * meanBird[,1:11] 
print(meanBird)

## max of each row in relation to mean habitat of patches
birdLove <- df[mean_vegetation_volume==4 &
    dispersal_distance==2 &
    habitat_radius==2 &
    max_bird==5,
    .(pmax(bird_love_0,bird_love_1,
    bird_love_2,bird_love_3,
    bird_love_4,bird_love_5,bird_love_6,
    bird_love_7,bird_love_8,
    bird_love_9, bird_love_10),mean_habitat_of_patches)]
head(birdLove)
tail(birdLove)

ggplot(data=birdLove,aes(x=V1, y=mean_habitat_of_patches)) +
    geom_point() +
    xlim(0,400) +
    ylim(0,200)
## no relationship??

## Starting value of mean_veg on bird count
 
meanVeg <- df[dispersal_distance==2 &
    habitat_radius==2 &
    max_bird==5 &
    step==1000,
    .(mean_vegetation_volume,count_turtles)]

head(meanVeg)
meanVeg[,max(count_turtles)]
# plot
ggplot(data=meanVeg,aes(x=mean_vegetation_volume, y=count_turtles)) +
    geom_point() +
    xlim(0,5) +
    ylim(0,1000)

# LOGISTIC

meanVeg$logBird <- log10(meanVeg$count_turtles)
meanVeg$logVeg <- log(meanVeg$mean_vegetation_volume)
head(meanVeg)

## ggplot lm
ggplot(data=meanVeg,aes(x=logVeg, y=count_turtles)) +
    geom_point() +
    geom_smooth(method = lm)
cor.test(meanVeg$logVeg,meanVeg$count_turtles)
#         Pearson's product-moment correlation

# data:  meanVeg$logVeg and meanVeg$count_turtles
# t = 20.09, df = 158, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7974708 0.8863141
# sample estimates:
#       cor
# 0.8477354


## Test for multicolinearity btwn habitat and mean_veg??

reg2 <- lm(count_turtles ~ mean_vegetation_volume+max_bird+
    dispersal_distance+habitat_radius, df)
summary(reg2)
plot(reg2)
# Transform Y
plot(df$count_turtles,df$step)
hist(df$count_turtles)
df$turtle_t <- exp(df$count_turtles)
hist(df$turtle_t)


## pwr analysis to find minimal sample size to
## pull from larger data set because too many 
## observations, can't do regression
meanResponse <- df[step==1000,mean(count_turtles)]
meanResponse
meanCtrl <- df[step==0, mean(count_turtles)]
meanCtrl
birdCombine <- df[step==1000, count_turtles] + df[step==0, count_turtles]
birdCombine
## Cohen's d calculation for "f"
(meanResponse - meanCtrl) / sd(birdCombine)

pwr.anova.test(k=2,power=.80,f=2.019676)
#      Balanced one-way analysis of variance power calculation

#               k = 2
#               n = 2.397912
#               f = 2.019676
#       sig.level = 0.05
#           power = 0.8

# NOTE: n is number in each group
## not very useful...

