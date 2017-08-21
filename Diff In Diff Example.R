require(lubridate)
require(ggplot2)
require(tidyverse)

### Sloppy creation of data
SampleData <- NULL
Draws <- NULL
Employment <- NULL
for(i in 1:6){
  x <- ifelse(i<=3, i, i*3)
  Draws <- NULL
  Employment <- data.frame(NJEmployment = rnorm(10,i), NYEmployment = rnorm(10, x))
  SampleData <- rbind(SampleData, Employment)
}

rm(Covariate)
rm(NJDummy)
rm(Treatment)

Employment <- data.frame(Employment = union_all(SampleData$NJEmployment, SampleData$NYEmployment))
Covariate <- union_all(data.frame(Covariate = rnorm(30,1)), data.frame(Covariate = rnorm(30,2)))
Covariate <- union_all(Covariate, data.frame(Covariate = rnorm(60,3))) 
                                            
NJDummy <- union_all(data.frame(NJDummy = rep(1, 60)), data.frame(NJDummy = rep(0, 60)))
Treatment <- union_all(data.frame(Treatment = rep(0, 30)), data.frame(Treatment = rep(1, 30)))
Treatment <- union_all(Treatment, data.frame(Treatment = rep(0, 30)))
Treatment <- union_all(Treatment, data.frame(Treatment = rep(1, 30)))
###

# Bringing it together
EmploymentData <- cbind(Employment, NJDummy, Covariate, Treatment) %>%
                      mutate(DiffInDiff = Treatment * NJDummy)

# Difference in differences estimator Y = a + B1(Main Effect Dummy) + B2(Treatment Time Period) + B3(Main Effect During Treatment Time Period)

### Interpretation of coefficients
# a: Effect of being in the region NOT indicated by the main effect dummy DURING the NON-treatment time period
      # Algebraically: a = a1(Main Effect = 0) + a2(Treatment = 0)
# B1: The average effect of Main Effect = 1 and Main Effect = 0 in non-treatment time
      # Algebraically: B1 = b1(Main Effect = 1) - a1(MainEffect = 0)
# B2: The average effect of the Treatment = 1 and Treatment = 0 in non-treatment main effect
      # Algebraically: B2 = b2(Treatment = 1) - a2(Treatment = 0)
# B3: The average effect of the treatment in the treated area, less external time effects and average fixed main effect differences

summary(lm(data = EmploymentData, Employment ~ NJDummy + Treatment + DiffInDiff))

#Plot Series Prior to Treatment
ggplot(EmploymentData %>% filter(Treatment == 0 & NJDummy == 0), aes(seq_along(Employment), Employment)) + 
  geom_line(color = "red") + 
  geom_line(data = EmploymentData %>% filter(Treatment == 0 & NJDummy == 1), aes(seq_along(Employment), Employment),color = "blue")

#plot Entire Series
ggplot(EmploymentData %>% filter(NJDummy == 0), aes(seq_along(Employment), Employment)) + 
  geom_line(color = "red") + 
  geom_line(data = EmploymentData %>% filter(NJDummy == 1), aes(seq_along(Employment), Employment),color = "blue") +
  geom_vline(xintercept = 30, size=.2, color = "black")















