require(dplyr)
require(ggplot2)
require(tidyverse)

set.seed(12101988)

# Generate survey data from a survey with 5 participants,
# 100 questions that will be treated as observations of 5 "themes" with ordinal answers coded 1-5

SurveyData <- data.frame(Participant = c(rep(1, 100), rep(2, 100), rep(3, 100), rep(4, 100), rep(5, 100)),
                         Theme = seq(1, 20, 1),
                         Question = seq(1, 100, 1),
                         Response = round(runif(500,1,5), 0))

SurveyData <- data.frame(Participant = c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2),
                         Theme = c(1,1,2,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5,5),
                         Question = seq(1,2,1),
                         Response = c(-2.599,-18.21,-15.218,3.105,-.272,-2.586,2.54,-10.616,-15.743,-2.958,-5.528,1.566,-3.805,5.464,6.756,-9.071,11.635,-11.929,8.022,-7.669))

# Notes:
# In this case, the themes are equivalent to "parts" from traditional R&R,
# the participants are the "operators", and the questions are the "readings".
# The strict assumption is made that all questions within a theme should have the same score 
# (because they should test the same cause of variation OR be structurally linked)

Parts <- length(unique(SurveyData$Theme))
Operators <- length(unique(SurveyData$Participant))
Readings <- 2

DFpart <- Parts - 1
DFoperator <- Operators - 1
DFrepeat <- Parts * Operators * (Readings - 1)
DFtotal <- sum(c(DFparts, DFoperators, DFrepeat))
DFpartoperator <- (Parts - 1)*(Operators-1)

# With function piping, the initial calculations are straightforward
Differences <- SurveyData %>%
  # First, find the overall (grand) average response level
  mutate(GrandMean = mean(Response)) %>%
  # Next, find the mean response for each theme and the squared difference from the overall mean
  group_by(Theme) %>%
  mutate(PartAvg = mean(Response),
         Spart = (PartAvg - GrandMean)^2) %>% 
  # Now, find mean response by Participant and the squared difference from the overall mean
  ungroup() %>%
  group_by(Participant) %>%
  mutate(OperatorAvg = mean(Response),
         Soperator = (OperatorAvg - GrandMean)^2) %>% 
  # Find mean response by Participant and theme (Mean for Repeatability)
  ungroup() %>%
  group_by(Theme, Participant) %>%
  mutate(RepeatAvg = mean(Response)) %>%
  ungroup() %>%
  mutate(Srepeat = (Response - RepeatAvg)^2,
         Stotal = (Response - GrandMean)^2)

# Calculations broken up for readability
Significance <- Differences %>%
  # Calculate sums of squares (SS)
  summarize(SSpart = sum(Spart),
            SSoperator = sum(Soperator),
            SSrepeat = sum(Srepeat),
            SStotal = sum(Stotal)) %>%
  # Calculate SS for parts & operators and Mean Square Differences
  mutate(SSPartOperator = SStotal - SSpart - SSoperator - SSrepeat,
         MSpart = SSpart/DFpart,
         MSoperator = SSoperator/DFoperator,
         MSrepeat = SSrepeat/DFrepeat,
         MSPartOperator = SSPartOperator/DFpartoperator,
         MSRepeatOrInteraction = MSPartOperator,           # Term set to the interaction value before criticality test
         FStatPartOperatorInt = MSPartOperator/MSrepeat)

# Retrieve critical value of F-distribution utilizing numerator & denominator DOF
# Operational critical limit set to .8
Fcrit <- qf(.8, df1 = DFpartoperator, df2 = DFrepeat)

# If the part - operator interaction is not significant, attribute this variance to repeatability
# If the f-stat is not critical, the adjusted repeatability variance is used in the variance
# components calculations below (MSRepeatOrInteraction)
if(Significance$FStatPartOperatorInt < Fcrit){
  Significance <- Significance %>%
                      mutate(MSrepeat = (SSrepeat + SSPartOperator)/(DFrepeat + DFpartoperator),
                             MSRepeatOrInteraction = MSrepeat)              # Term set to repeatability value upon failure
}

Variance <- Significance %>%
  mutate(PartToPartVar = max(c(0, (MSpart - MSRepeatOrInteraction) / (Operators * Readings))),
         OperatorVar = max(c(0, (MSoperator - MSRepeatOrInteraction) / (Parts * Readings))),
         RepeatVar = MSrepeat,
         PartOperatorIntVar = ifelse(FStatPartOperatorInt < Fcrit, max(c(0, (MSPartOperator - MSrepeat) / Readings)), 0),
         ReproducibilityVar = OperatorVar + PartOperatorIntVar,
         TotalGaugeRR = RepeatVar + ReproducibilityVar,
         TotalProcessVar = TotalGaugeRR + PartToPartVar)






