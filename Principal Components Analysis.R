require(dplyr)
require(ggplot2)
require(tidyverse)

data(iris)

# Ln-transform continuous variables (Box-Cox more general)
IrisVariables <- iris %>% 
                    rename(SepalLength = Sepal.Length,
                           SepalWidth = Sepal.Width,
                           PetalLength = Petal.Length,
                           PetalWidth = Petal.Width) %>%
                    transmute(SepalLength = log(SepalLength),
                              SepalWidth = log(SepalWidth),
                              PetalLength = log(PetalLength),
                              PetalWidth = log(PetalWidth))

IrisCategory <- iris %>%
                  dplyr::select(Species)


PCAModel <- prcomp(IrisVariables, center = TRUE, scale = TRUE)     

# Print Rotation & Variance

print(PCAModel)

# Scree Plot

plot(PCAModel, type = "l")

# Simple Biplot

biplot(PCAModel)

# Component Summary

summary(PCAModel)

# Naive Prediction

NewObs <- tail(IrisVariables,5) 
  
predict(PCAModel, newdata=NewObs)

# Better Biplot Function - PC is a princomp object

PCBiplot <- function(PC, TextColor = c('black'), LineColor = c('red')){
                
                .e <- environment()
  
                Observations <- data.frame(PC$x) %>%
                                  mutate(ObsNames = rownames(.))         
  
                PCData <- data.frame(PC$rotation) %>%
                            mutate(FacetNames = rownames(.),
                                   VectorMultiplier = min((max(Observations$PC1) - min(Observations$PC1)/(max(PC1)-min(PC1))), (max(Observations$PC2) - min(Observations$PC2)/(max(PC2)-min(PC2)))),
                                   Vector1 = .7 * VectorMultiplier * PC1,
                                   Vector2 = .7 * VectorMultiplier * PC2)
                
              ggplot(data = Observations, aes(x = PC1, y = PC2), environment = .e) + 
                geom_text(alpha = .4, size = 3, aes(label = ObsNames), color = TextColor) + 
                geom_hline(yintercept = 0, size=.2) + 
                geom_vline(xintercept = 0, size=.2, color = TextColor) +
                coord_equal() + 
                geom_text(data = PCData, aes(x = Vector1, y = Vector2, label = FacetNames), size = 4, vjust = 1, color = LineColor) +
                geom_segment(data = PCData, aes(x = 0, y = 0, xend = Vector1, yend = Vector2), arrow = arrow(length = unit(0.2,"cm")), alpha = 0.75, color = LineColor)
                
}

PCBiplot(PCAModel)

