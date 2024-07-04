library("ggplot2")
library("dplyr")
library("gridExtra")
library(Simpsons)
library(GGally)
library(memisc)
library(pander)
library(corrplot)


#Loading the csv file
wine <- read.csv('C:\\Users\\Devanshi\\Downloads\\wineQualityReds (1).csv')

#Transforming Quality from an Integer to a Factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating, levels = c('bad', 'average', 'good'))

## Structure and summary of the Dataframe
#{r echo=FALSE, message=FALSE, warning=FALSE, packages}
str(wine)
#{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wine)

## Univariate Plots
#for report
ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'lavender',fill = I('pink'))
#for report
ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 1, color = 'pink',fill = I('lavender'))

#for report 2plots for fixed acidity
grid.arrange(
  ggplot(wine, aes(x = 1, y = fixed.acidity)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(4, 14)),

  ggplot(data = wine, aes(x = fixed.acidity)) +
    geom_histogram(binwidth = 1, color = 'pink', fill = I('lavender')) +
    scale_x_continuous(lim = c(4, 14)),

  ncol = 2
)

#for report 2plots for volatile acidity
grid.arrange(
  ggplot(wine, aes(x = 1, y = volatile.acidity)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(0, 1)),

  ggplot(data = wine, aes(x = volatile.acidity)) +
    geom_histogram(binwidth = 0.05, color = 'pink', fill = I('lavender')) +
    scale_x_continuous(lim = c(0, 1)),

  ncol = 2
)

#for report 2plots for citric acid
grid.arrange(
  ggplot(wine, aes(x = 1, y = citric.acid)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'lavender'),

  ggplot(data = wine, aes(x = citric.acid)) +
    geom_histogram(binwidth = 0.08, color = 'pink', fill = I('lavender')) +
    scale_x_continuous(breaks = seq(0, 1, 0.1), lim = c(0, 1)),

  ncol = 2
)

#for report 2plots for residual sugar
grid.arrange(
  ggplot(wine, aes(x = 1, y = residual.sugar)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(1, 8)),

  ggplot(data = wine, aes(x = residual.sugar)) +
    geom_histogram(binwidth = 0.1, color = 'lavender', fill = I('pink')) +
    scale_x_continuous(lim = c(1, 8)),

  ncol = 2
)

#for report 2plots for chlorides
grid.arrange(
  ggplot(wine, aes(x = 1, y = chlorides)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(0, 0.25)),

  ggplot(data = wine, aes(x = chlorides)) +
    geom_histogram(binwidth = 0.01, color = 'lavender', fill = I('pink')) +
    scale_x_continuous(lim = c(0, 0.25)),

  ncol = 2
)


#for report 2plots for freesulphur dioxide
grid.arrange(
  ggplot(wine, aes(x = 1, y = free.sulfur.dioxide)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(0, 45)),

  ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
    geom_histogram(binwidth = 1, color = 'lavender', fill = I('pink')) +
    scale_x_continuous(breaks = seq(0, 80, 5), lim = c(0, 45)),

  ncol = 2
)


#for report 2plots for total sulphur dioxide
grid.arrange(
  ggplot(wine, aes(x = 1, y = total.sulfur.dioxide)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'lavender') +
    scale_y_continuous(lim = c(0, 180)),

  ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
    geom_histogram(binwidth = 5, color = 'pink', fill = I('lavender')) +
    scale_x_continuous(lim = c(0, 180)),

  ncol = 2
)


#for report 2plots for density variable
grid.arrange(
  ggplot(wine, aes(x = 1, y = density)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'lavender'),

  ggplot(data = wine, aes(x = density)) +
    geom_histogram(binwidth = 0.001, color = 'pink', fill = I('lavender')),

  ncol = 2
)
#For the Density variable, we see something new for the first time. This Variable has almost a perfect Normal Distribution.


#for report 2plots for ph
grid.arrange(
  ggplot(wine, aes(x = 1, y = pH)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink'),

  ggplot(data = wine, aes(x = pH)) +
    geom_histogram(binwidth = 0.1, color = 'pink', fill = I('lavender')),

  ncol = 2
)


#for report 2plots for sulphates variable
grid.arrange(
  ggplot(wine, aes(x = 1, y = sulphates)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(0.3, 1.6)),

  ggplot(data = wine, aes(x = sulphates)) +
    geom_histogram(binwidth = 0.1, color = 'pink', fill = I('lavender')) +
    scale_x_continuous(lim = c(0.3, 1.6)),

  ncol = 2
)


#for report 2plots for alcohol
grid.arrange(
  ggplot(wine, aes(x = 1, y = alcohol)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'pink') +
    scale_y_continuous(lim = c(8, 14)),

  ggplot(data = wine, aes(x = alcohol)) +
    geom_histogram(binwidth = 0.1, color = 'pink', fill = I('lavender')) +
    scale_x_continuous(lim = c(8, 14)),

  ncol = 2
)









## Bivariate Plots


c <- cor(
  wine %>%
    # first we remove unwanted columns
    dplyr::select(-X) %>%
    dplyr::select(-rating) %>%
    mutate(
      # now we translate quality to a number
      quality = as.numeric(quality)
    )
)
emphasize.strong.cells(which(abs(c) > .3 & c != 1, arr.ind = TRUE))
pandoc.table(c)




#for report  "quality" and "fixed acidity"
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)
#for report "quality" and "volatile acidity" variables
ggplot(data = wine, aes(x = quality, y = volatile.acidity)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "lavender",
               shape = 8,
               size = 4)
#for report Box plots between "quality" and "citric acid" variables
ggplot(data = wine, aes(x = quality, y = citric.acid)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'pink') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "lavender",
               shape = 8,
               size = 4)
#for report Box plots between "quality" and "residual sugar" variables
ggplot(data = wine, aes(x = quality, y = residual.sugar)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'pink') +
  scale_y_continuous(lim = c(0, 5)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "lavender",
               shape = 8,
               size = 4)
#for report Box plots between "quality" and "chlorides" variables
ggplot(data = wine, aes(x = quality, y = chlorides)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  scale_y_continuous(lim = c(0, 0.2)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)
#for report Box plots between "quality" and "free sulphur dioxide" variables
ggplot(data = wine, aes(x = quality, y = free.sulfur.dioxide)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  scale_y_continuous(lim = c(0, 40)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)

#for report Box plots between "quality" and "total sulphur dioxide" variables
ggplot(data = wine, aes(x = quality, y = total.sulfur.dioxide)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  scale_y_continuous(lim = c(0, 150)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)
#for report Box plots between "quality" and "density" variables
ggplot(data = wine, aes(x = quality, y = density)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "lavender",
               shape = 8,
               size = 4)
#for report Box plots between "quality" and "ph" variables
ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'pink') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)

#for report linear regression line between"fixed.acidity" and "pH" variables
ggplot(data = wine, aes(x = fixed.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks = seq(5, 15, 1)) +
  xlab("Fixed Acidity in Log Scale") +
  geom_smooth(method = "lm")

#for report linear regression line between"volatile acidity" and "pH" variables
ggplot(data = wine, aes(x = volatile.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("Volatile Acidity in Log Scale") +
  geom_smooth(method="lm")

#for report linear regression line between"volatile acidity" and "pH" variables
ggplot(data = subset(wine, citric.acid > 0), aes(x = citric.acid, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  xlab("Citric Acid in Log Scale") +
  geom_smooth(method = "lm")

#here we are checking simpson's paradox
simpsons <- Simpsons(volatile.acidity, pH, data=wine)
plot(simpsons)



#for report  "quality" and "sulphates" variables
ggplot(data = wine, aes(x = quality, y = sulphates)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  scale_y_continuous(limits = c(0.25, 1)) +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)

#for report "quality" and "alcohol" variables
ggplot(data = wine, aes(x = quality, y = alcohol)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = 'lavender') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "pink",
               shape = 8,
               size = 4)

alcoholQualityLinearModel <- lm(as.numeric(quality) ~ alcohol,
                                data = wine)
summary(alcoholQualityLinearModel)

#here we define function for correlation test
simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

#here we check correlation
correlations <- c(
  simple_cor_test(wine$fixed.acidity, wine$quality),
  simple_cor_test(wine$volatile.acidity, wine$quality),
  simple_cor_test(wine$citric.acid, wine$quality),
  simple_cor_test(log10(wine$residual.sugar), wine$quality),
  simple_cor_test(log10(wine$chlorides), wine$quality),
  simple_cor_test(wine$free.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$total.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$density, wine$quality),
  simple_cor_test(wine$pH, wine$quality),
  simple_cor_test(log10(wine$sulphates), wine$quality),
  simple_cor_test(wine$alcohol, wine$quality))
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'log10.residual.sugar',
                         'log10.chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'log10.sulphates', 'alcohol')

correlations

#we decide from output that alcohol, citric acid , sulphates and voltile acidity are important variables





## Multivariate Plots


# for report scatterplot on  "quality" using "density" and "alcohol"
ggplot(data = wine, aes(y = density, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))

# for report scatterplot on  "quality" using "sulphates" and "alcohol"
ggplot(data = wine, aes(y = sulphates, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_y_continuous(limits = c(0.3, 1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))




# for report scatterplot on  "quality" using"volatile acidity" and "alcohol"
ggplot(data = wine, aes(y = volatile.acidity, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))



# for report scatterplot on  "quality" using "ph" and "alcohol"
ggplot(data = wine, aes(y = pH, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))


# for report scatterplot on  "quality" using "residual sugar" and "alcohol"
ggplot(data = wine, aes(y = residual.sugar, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))

# for report scatterplot on  "quality" using "total sulphur dioxide" and "alcohol"
ggplot(data = wine, aes(y = total.sulfur.dioxide, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))

# for report scatterplot on  "quality" variable with linear regression line, using the "citric acid" and "volatile acidity" variables
ggplot(data = wine, aes(y = citric.acid, x = volatile.acidity, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))

# for report scatterplot on  "quality" variable with linear regression line, using the "citric acid" and "fixed acidity" variables
ggplot(data = wine, aes(y = citric.acid, x = fixed.acidity, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))


# for report scatterplot "quality" using the "fixed acid" and "volatile acidity"
ggplot(data = wine, aes(y = fixed.acidity, x = volatile.acidity, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~rating) +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality'))

# linear modeling
set.seed(1221)
training_data <- sample_frac(wine, 0.6)
test_data <- wine[!wine$X %in% training_data$X, ]

m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, . ~ . + sulphates)
m3 <- update(m2, . ~ . + volatile.acidity)
m4 <- update(m3, . ~ . + citric.acid)
m5 <- update(m4, . ~ . + fixed.acidity)
m6 <- update(m2, . ~ . + pH)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)


#for report
wine_predict <- data.frame(
  quality = test_data$quality,
  error = predict(m5, test_data) - as.numeric(test_data$quality)
)

ggplot(data = wine_predict, aes(x = quality, y = error)) +
  geom_jitter(alpha = 0.3)


#for report plot1
ggplot(data = wine, aes(y = alcohol, x = quality)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.5, color = "pink") +
  stat_summary(fun.y = "mean", geom = "point", color = "lavender", shape = 8, size = 4) +
  xlab("Quality") +
  ggtitle("Influence of alcohol on wine quality")

#for report plot2
ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_y_continuous(limits = c(0.3, 1.5)) +
  ylab("Potassium Sulphate (g/dm3)") +
  xlab("Alcohol Percentage") +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = 'Quality')) +
  ggtitle("Alcohol and Sulphates over Wine Quality")

#for report plot 3
df <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(df) <- c("quality", "error")
ggplot(data = df, aes(x = quality, y = error)) +
  geom_jitter(alpha = 0.3) +
  ggtitle("Linear Model Errors vs Expected Quality")













