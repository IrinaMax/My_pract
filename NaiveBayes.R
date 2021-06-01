# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data is binary.csv 
data <- read.csv(file.choose(), header = T)
data <- read.csv('~//Documents/R/R_ML/binary.csv', header=T)
str(data)
xtabs(~admit+rank, data = data)  # cross tabulation, we need frequency more then 5 here
# convert rank and admit to factor
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)
str(data)

# Visualization
pairs.panels(data[-1])
boxplot(data$admit, data$gre, col = "blue")
data %>%
  ggplot(aes(x=admit, y=gre, fill = admit)) +
  geom_boxplot( alpha=.5) +
  ggtitle("Box Plot")

data %>%
         ggplot(aes(x=admit, y=gpa, fill = admit)) +
         geom_boxplot() +
         ggtitle("Box Plot")

# Density 
data %>% ggplot(aes(x=gre, fill = admit)) +
  geom_density(alpha=0.5, color= 'black') +
  ggtitle("Density Plot")
data %>% ggplot(aes(x=gpa, fill = admit)) +
         geom_density(alpha=0.5, color= 'black') +
         ggtitle("Density Plot")

# Data Partition
set.seed(1234)
# split on 80% and 20%
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(admit ~ ., data = train)
model$data
plot(model)
# impoving model with using kernel =True
model <- naive_bayes(admit ~ ., data = train, usekernel = T)
model %>% summary
model
train %>%
  filter(admit == "0") %>%
  summarise(mean(gre), sd(gre))
train %>%
         filter(admit == "1") %>%
         summarise(mean(gre), sd(gre))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
p %>% head
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)  # missclassification
