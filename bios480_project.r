library(ggplot2)
library(httpgd)
library(data.table)

# make df of all data
df <- fread("nik_files_large/10-24-2.csv", skip=6)
# and make headers readable
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("-", "_", names(df))
names(df) <- gsub("\\[|\\]", "", names(df))
names(df) <- gsub("count_patches_with_*", "", names(df))
names(df) <- gsub("*=_*", "", names(df))
head(df)
tail(df)

##  Aggregate the 10 replicate runs of 2 response variables
##  to all predictor variables
avgDF <- aggregate(cbind(count_turtles, mean_habitat_of_patches) ~ step +
    mean_vegetation_volume + max_bird + dispersal_distance +
    habitat_radius, df, mean)
head(avgDF)
## Only pick out runs with step 1000
newDF <- avgDF[avgDF$step == 1000, 1:7]
head(newDF)
tail(newDF)

## Check violations
ggplot(newDF, aes(x=count_turtles)) +
    geom_histogram() +
    xlab("Final_bird_population") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
    axis.title = element_text(size=12)) +
    ggtitle("Histogram of Bird Population")
    #normally distributed
#qqnorm(newDF$count_turtles)
#qqline(newDF$count_turtles)

ggplot(newDF, aes(x=mean_habitat_of_patches)) +
    geom_histogram() +
    xlab("final mean habitat of patches") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
    axis.title = element_text(size=12)) +
    ggtitle("Histogram of mean habitat of patches")
# right skew
qqnorm(newDF$mean_habitat_of_patches)
qqline(newDF$mean_habitat_of_patches)

##  Analysis of Mean_habitat_of_patches
# transformations 
hist(newDF$mean_habitat_of_patches)
hist(newDF$mean_habitat_of_patches^0.25)
hist(newDF$mean_habitat_of_patches^0.3)
hist(newDF$mean_habitat_of_patches^0.5)
hist(newDF$mean_habitat_of_patches^0.75)
hist(log10(newDF$mean_habitat_of_patches))
## ^0.25 transformation looks best
# plot residuals of combined model
## Multiple regression analysis
newDF$hab_t <- newDF$mean_habitat_of_patches^0.25
regHab <- lm(hab_t ~ mean_vegetation_volume + max_bird +
    dispersal_distance + habitat_radius, newDF)
plot(regHab)
summary(regHab)
## looks quite strange and violating assumptions
## check and transform x variables
hist(newDF$mean_vegetation_volume)
hist(newDF$max_bird)
hist(newDF$dispersal_distance)
hist(newDF$habitat_radius)
#transform
hist(log10(newDF$mean_vegetation_volume))
hist(((max(newDF$max_bird) - newDF$max_bird)^.3)+1)
hist(newDF$dispersal_distance)
hist(log10(newDF$habitat_radius))
# new transformed DF
mean_hab_DF <- data.frame(newDF$mean_habitat_of_patches^0.25)
mean_hab_DF$mean_veg_t <- log10(newDF$mean_vegetation_volume)
mean_hab_DF$max_bird_t <- ((max(newDF$max_bird) - newDF$max_bird)^.3)+1
mean_hab_DF$dispersal_distance <- newDF$dispersal_distance
mean_hab_DF$hab_rad_t <- log10(newDF$habitat_radius)
head(mean_hab_DF)
regHab2 <- lm(newDF.mean_habitat_of_patches.0.25 ~ mean_veg_t + max_bird_t +
    dispersal_distance + hab_rad_t, mean_hab_DF)
plot(regHab2)
summary(regHab2)
# still looks a bit strange, check for multicollinearity
##  Multicollinearity assumption check
library(ggcorrplot)
reduced <- subset(newDF, select = +
    c(-mean_habitat_of_patches, -step, -count_turtles, -hab_t))
corr_matrix = round(cor(reduced), 2)
ggcorrplot(corr_matrix, lab = TRUE)
# high correlation between mean_veg and max_bird
# new model without max_bird
regHab3 <- lm(newDF.mean_habitat_of_patches.0.25 ~ mean_veg_t +
    dispersal_distance + hab_rad_t, mean_hab_DF)
plot(regHab3)
summary(regHab3)
# model 1 R2/adj: 0.9789/0.9788
# model 2 R2/adj: 0.9533/0.9533
# model 3 R2/adj: 0.9512/0.9511
AIC(regHab, regHab2, regHab3)
#         df       AIC
# regHab   6 -6288.932
# regHab2  6 -2325.966
# regHab3  5 -2100.901
## model 1 is the best model

## ANOVA for fun
anova(regHab)


#   Analysis of count_turtles
bird_model_1 <- lm(count_turtles ~ mean_vegetation_volume + max_bird +
    dispersal_distance + habitat_radius, newDF)
plot(bird_model_1)
# doesn't quite look like the night sky
summary(bird_model_1)

## apply same X transformations as before
bird_DF <- data.frame(newDF$count_turtles)
bird_DF$mean_veg_t <- log10(newDF$mean_vegetation_volume)
bird_DF$max_bird_t <- ((max(newDF$max_bird) - newDF$max_bird)^.3)+1
bird_DF$dispersal_distance <- newDF$dispersal_distance
bird_DF$hab_rad_t <- log10(newDF$habitat_radius)
head(bird_DF)
bird_model_2 <- lm(newDF.count_turtles ~ mean_veg_t + max_bird_t +
    dispersal_distance + hab_rad_t, bird_DF)
plot(bird_model_2)
summary(bird_model_2)
# looks much worse

## multicollinearity check; we already know from previous
# results that max_bird is correlated
## new model without max_bird
bird_model_3 <- lm(count_turtles ~ mean_vegetation_volume +
    dispersal_distance + habitat_radius, newDF)
plot(bird_model_3)
summary(bird_model_3)
# the residuals look very good, but the R2 value is terrible
# model 1 R2/adj: 0.9933/0.9933
# model 2: 0.7503/0.7501
# model 3: 0.01207/0.01147

# AIC check
AIC(bird_model_1,bird_model_2,bird_model_3)
# model 1 has lowest AIC
# anova check
anova(bird_model_1,bird_model_2,bird_model_3)

# Only y var vs mean_veg | individual models
bird_vs_veg <- lm(count_turtles ~ mean_vegetation_volume, newDF)
summary(bird_vs_veg)
plot(bird_vs_veg)

newDF$hab_t <- newDF$mean_habitat_of_patches^0.25
hab_vs_veg <- lm(hab_t ~ mean_vegetation_volume, newDF)
summary(hab_vs_veg)
plot(hab_vs_veg)
