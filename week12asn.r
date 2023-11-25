# Assignment 12
# UIN: 671692891

#   Question 1
bumpus <- read.csv('data/bumpus.csv')
head(bumpus)
# a
ggplot(data=bumpus,aes(x=weight_g)) +
    geom_histogram() +
    facet_wrap(~ survival, ncol=1)
# distributions look approximately normal, variance is slightly higher in
# dead birds
# b
ttestwelch <- t.test(weight_g ~ survival, data = bumpus, var.equal=FALSE)
ttestwelch
# Welch Two Sample t-test

# data:  weight_g by survival
# t = 2.5161, df = 117.88, p-value = 0.01321
# alternative hypothesis: true difference in means between group died and group survived is not equal to 0
# 95 percent confidence interval:
#  0.1351376 1.1339597
# sample estimates:
#     mean in group died mean in group survived 
#               25.86094               25.22639 
# c
## 95% confidence interval is 0.1351376-1.1339597

#   Question 2
countries <- read.csv('data/countries.csv')
head(countries)
countries$footprintDif <- countries$ecological_footprint_2012 - countries$ecological_footprint_2000
print(footprintDif)
# a
ggplot(data=countries, aes(x=footprintDif)) +
    geom_histogram(na.rm=TRUE)
# b
ttestFoot <- t.test(countries$ecological_footprint_2012, countries$ecological_footprint_2000,
    paired = TRUE)
ttestFoot
#         Paired t-test

# data:  countries$ecological_footprint_2012 and countries$ecological_footprint_2000
# t = -2.7553, df = 45, p-value = 0.008434
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.7217469 -0.1121662
# sample estimates:
# mean of the differences 
#              -0.4169565 

# c
# There is a significant difference between the sample means
# with a p-value of 0.0084. There was an overall decrease in
# ecological footprint.

#   Question 3
legShaving <- read.csv('data/leg shaving.csv')
head(legShaving)
# a
ttestLeg <- t.test(legShaving$hair_width_change_test, legShaving$hair_width_change_control,
    paired=TRUE)
ttestLeg
#         Paired t-test

# data:  legShaving$hair_width_change_test and legShaving$hair_width_change_control
# t = -1.905, df = 4, p-value = 0.1295
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -21.625854   4.025854
# sample estimates:
# mean of the differences 
#                    -8.8 
# b
# Paired t-test shows an insignificant difference between
# the treatment and control. Difference between means is
# -8.8 with a 95% confidence interval of -21.625854-4.025854

#   Question 4
cuckoo <- read.csv('data/cuckooeggs.csv')
head(cuckoo)
# a
ggplot(data=cuckoo, aes(x=egg_length)) +
    geom_histogram() +
    facet_wrap(~ host_species, ncol=1)
# b
library(dplyr)
by_sp <- group_by(cuckoo, host_species)
summarise(by_sp, group_mean = mean(egg_length),
    group_sd = sd(egg_length))
# A tibble: 6 × 3
#   host_species  group_mean group_sd
#   <chr>              <dbl>    <dbl>
# 1 Hedge Sparrow       23.1    1.07 
# 2 Meadow Pipit        22.3    0.921
# 3 Pied Wagtail        22.9    1.07 
# 4 Robin               22.6    0.685
# 5 Tree Pipit          23.1    0.901
# 6 Wren                21.1    0.744

# c
# since the group standard deviations and means and distributions are similar,
# running an ANOVA would be a valid method

# d
cuckooANOVA <- lm(egg_length ~ host_species, cuckoo)
summary(cuckooANOVA)
# Call:
# lm(formula = egg_length ~ host_species, data = cuckoo)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.64889 -0.44889 -0.04889  0.55111  2.15111 

# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              23.12143    0.24301  95.147  < 2e-16 ***
# host_speciesMeadow Pipit -0.82254    0.27825  -2.956  0.00379 ** 
# host_speciesPied Wagtail -0.21810    0.33789  -0.645  0.51992    
# host_speciesRobin        -0.54643    0.33275  -1.642  0.10332    
# host_speciesTree Pipit   -0.03143    0.33789  -0.093  0.92606    
# host_speciesWren         -1.99143    0.33789  -5.894 3.91e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.9093 on 114 degrees of freedom
# Multiple R-squared:  0.313,     Adjusted R-squared:  0.2829 
# F-statistic: 10.39 on 5 and 114 DF,  p-value: 3.152e-08

anova(cuckooANOVA)
# Analysis of Variance Table

# Response: egg_length
#               Df Sum Sq Mean Sq F value    Pr(>F)    
# host_species   5 42.940  8.5879  10.388 3.152e-08 ***
# Residuals    114 94.248  0.8267                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## I conclude that at least one of the host species produces a significantly
## different egg length from 0

# e
TukeyHSD(aov(cuckooANOVA))
#  Tukey multiple comparisons of means
#     95% family-wise confidence level

# Fit: aov(formula = cuckooANOVA)

# $host_species
#                                   diff          lwr         upr     p adj
# Meadow Pipit-Hedge Sparrow -0.82253968 -1.629133605 -0.01594576 0.0428621
# Pied Wagtail-Hedge Sparrow -0.21809524 -1.197559436  0.76136896 0.9872190
# Robin-Hedge Sparrow        -0.54642857 -1.511003196  0.41814605 0.5726153
# Tree Pipit-Hedge Sparrow   -0.03142857 -1.010892769  0.94803563 0.9999990
# Wren-Hedge Sparrow         -1.99142857 -2.970892769 -1.01196437 0.0000006
# Pied Wagtail-Meadow Pipit   0.60444444 -0.181375330  1.39026422 0.2324603
# Robin-Meadow Pipit          0.27611111 -0.491069969  1.04329219 0.9021876
# Tree Pipit-Meadow Pipit     0.79111111  0.005291337  1.57693089 0.0474619
# Wren-Meadow Pipit          -1.16888889 -1.954708663 -0.38306911 0.0004861
# Robin-Pied Wagtail         -0.32833333 -1.275604766  0.61893810 0.9155004
# Tree Pipit-Pied Wagtail     0.18666667 -0.775762072  1.14909541 0.9932186
# Wren-Pied Wagtail          -1.77333333 -2.735762072 -0.81090459 0.0000070
# Tree Pipit-Robin            0.51500000 -0.432271433  1.46227143 0.6159630
# Wren-Robin                 -1.44500000 -2.392271433 -0.49772857 0.0003183
# Wren-Tree Pipit            -1.96000000 -2.922428738 -0.99757126 0.0000006

## Pairs with the Meadow Pipit and Wren had significant differences in all
## pairs in mean cuckoo egg length.