library(tidyverse)
library(readxl)

bass <- read_excel("BSB_tagging_data.xlsx")
head(bass)

#### Objective 1 ####

## Making new column for whether sex changed between capture
## 0 = no change; 1 = change
bass$sex.change <- ifelse(bass$Sex_at_capture == bass$Sex_at_recapture, 0, 1)

## filtering data based on objective
bass %>% filter(Sex_at_capture == "F", 
                month(Date_at_recapture) > 07)
## total female BSB that were recaptured after the end of July = 29

bass %>% filter(Sex_at_capture == "F", 
                sex.change == 1, 
                month(Date_at_recapture) > 07)
## total female BSB that changed sex = 9

9/29 
# proportion of female BSB that changed sex out of all female BSB
# that were recaptured after the end of spawning season = 0.31

### alpha = successes +1 = 10; beta = failures +1 = 21
x <- seq(0, 1, by = 0.01)
density<- dbeta(x, 10, 21)
plot(x, density, type = "l")

## getting 95% CI for the probability. Need the value at 2.5% and 97.5% 
qbeta(c(0.025,0.975), 10, 21)
## 95% confidence interval for the proportion of female BSB that changed
## sex out of all the females that were recaptured after the end of spawning
## season: 0.17 - 0.49


#### Objective 2 ####
length.mod <- glm(as.factor(sex.change) ~ Length_at_capture, 
    data = bass %>% filter(Sex_at_capture == "F", 
                           month(Date_at_recapture) > 07), 
    family = binomial(link= "logit"))
summary(length.mod)
## the length of a female at recapture does not significantly influence
## its probability of sex change given that it was recaptured after the
## end of spawning season

## For every millimeter increase in length, the log odds increases by 0.04

## predicting probabilities probabilities
Length_at_capture = seq(265, 400, by = 1)
new.data <- data.frame(Length_at_capture)
new.data$pred.proportion <- predict.glm(length.mod, new.data,
                               type = "")

## plotting data and predicted probabilities
ggplot()+
  geom_point(aes(x = Length_at_capture, y = sex.change), 
             data = bass %>% filter(Sex_at_capture == "F", 
                                                                                       month(Date_at_recapture) > 07))+
  geom_smooth(aes(x = Length_at_capture, y = pred.proportion), 
              data = new.data) +
  labs(x = "Length at capture (mm)", y = "Probability of sex change")+
  theme_classic()
## Caption: Probability that a female black sea bass captured after the end of 
## the spawning season has changed sex, predicted by the individual's length 
## (mm) at first capture. 