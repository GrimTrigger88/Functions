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

# Better Biplot Function

PCBiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
                # Where PC is a prcomp object
                PCData <- data.frame(PC$rotation) %>%
                            mutate(ObsNames = rownames(.))
  
                #data <- data.frame(obsnames=row.names(PC$x), PC$x)
                
                plot <- ggplot(PCData, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1]) + 
                        geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2, color=colors[2])
                datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
                mult <- min(
                  (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
                  (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
                )
                datapc <- transform(datapc,
                                    v1 = .7 * mult * (get(x)),
                                    v2 = .7 * mult * (get(y))
                )
                plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
                plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
                plot
}

PCBiplot(PCAModel)

