library(chron)
library(tidyverse)
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(varhandle)
library(plotly)
library(matrixStats)
library(corrplot)
library(caret)
library(caTools)
library(GGally)
library(Hmisc)
library(PerformanceAnalytics)
library(mltools)
library(data.table)
library(gghighlight)
library(tibble)
library(rpart)
library(randomForest)



heart_viz = read.csv("D:/JUPYTER/Projects/heart-attack/heart.csv")
view(heart_viz)
colSums(is.na(heart_viz))
head(heart_viz)
tail(heart_viz)
glimpse(heart_viz)
ncol(heart_viz)
nrow(heart_viz)
colnames(heart_viz)
summary(heart_viz)







heart_viz$sex = as.character(factor(heart_viz$sex, levels = c(0,1), labels = c("Female","Male")))
heart_viz$cp = as.character(factor(heart_viz$cp, levels = c(0,1,2,3), labels = c("Typical-Angina","Atypical-Angina","Non-Anginal","Asymptomatic")))
heart_viz$exng = as.character(factor(heart_viz$exng, levels = c(0,1), labels = c("NO","YES")))
heart_viz$caa = as.factor(heart_viz$caa)
heart_viz$fbs = as.character(factor(heart_viz$fbs, levels = c(0,1), labels = c("<=120",">120")))
heart_viz$restecg = as.character(factor(heart_viz$restecg, levels = c(0,1,2), labels = c("Normal","Abnormality","Probable-Ventricular-Hypertrophy")))
heart_viz$slp = as.factor(heart_viz$slp)
heart_viz$thall = as.factor(heart_viz$thall)

mat = data.matrix(heart_viz)
corrmat =cor(mat)
highCor = findCorrelation(corrmat, cutoff = 0.5)
fig_corr = ggcorr(heart_viz,label= TRUE)
annotate_figure(fig_corr, top = text_grob("Correlation among vairables", color = "Black", size = 14))
names(heart_viz)[highCor]



heart_viz$output = as.character(factor(heart_viz$output, levels = c(0,1), labels = c("NO","YES")))
summary(heart_viz)



fig1 = ggplot(heart_viz, aes(x = sex,fill = sex))+
  geom_histogram(stat = "count",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Sex distribution", x = "SEX", y = "Frequency")

fig2 = ggplot(heart_viz, aes(x = age, fill = sex))+
  geom_histogram(position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Age distribution", x = "Age", y = "Frequency")

fig3 = ggplot(heart_viz, aes(x = cp,fill = sex))+
  geom_histogram(stat = "count", position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Chest Pain distribution", x = "Pain Type", y = "Frequency")

fig4 = ggplot(heart_viz, aes(x = fbs,fill = sex))+
  geom_histogram(stat = "count", position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Blood Pressure distribution", x = "Blood Pressure(mg/dl)", y = "Frequency")

fig5 = ggplot(heart_viz, aes(x = trtbps,fill = sex))+
  geom_histogram(position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Resting Blood Pressure distribution", x = "Resting Blood Pressure(in mm Hg)", y = "Frequency")

fig6 = ggplot(heart_viz, aes(x = chol,fill = sex))+
  geom_histogram(position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Cholestrol distribution", x = "Cholestrol level(mg/dl)", y = "Frequency")

fig7 = ggplot(heart_viz, aes(x = thalachh,fill = sex))+
  geom_histogram(position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Max Heart Rate Achieved distribution", x = "heart_viz Rate", y = "Frequency")

fig8 = ggplot(heart_viz, aes(x = exng,fill = sex))+
  geom_histogram(stat = "count", position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Exercise Induced Angina distribution", x = "Exercise Induced Angina", y = "Frequency")

fig9 = ggplot(heart_viz, aes(x = oldpeak,fill = sex))+
  geom_histogram(position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Previous Peak distribution", x = "Previous Peak", y = "Frequency")

fig10 = ggplot(heart_viz, aes(x = slp,fill = sex))+
  geom_histogram(stat = "count" ,position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Slope distribution", x = "Slope", y = "Frequency")

fig11 = ggplot(heart_viz, aes(x = caa,fill = sex))+
  geom_histogram(stat = "count" ,position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Major vessels distribution", x = "Number of major vessels", y = "Frequency")

fig12 = ggplot(heart_viz, aes(x = thall,fill = sex))+
  geom_histogram(stat = "count" ,position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Thalium Stress Test result distribution", x = "Thalium Stress Test result", y = "Frequency")

fig13 = ggplot(heart_viz, aes(x = output,fill = sex))+
  geom_histogram(stat = "count" ,position = "dodge",colour=alpha("cornflowerblue", 0.7))+
  theme_stata()+
  labs(title = "Heart attack distribution", x = "Heart attack", y = "Frequency")

fig1to4 = ggarrange(fig1,fig2,fig3,fig4)
annotate_figure(fig1to4, top = text_grob("Sex, Age, Chest Pain, Blood Pressure distribution by Sex", color = "Black", size = 14))

fig5to8 = ggarrange(fig5,fig6,fig7,fig8)
annotate_figure(fig5to8, top = text_grob("Resting Blood Pressure, Cholestrol, Max Heart Rate, Exercise Induced Angina distribution by Sex", color = "Black", size = 14))

fig9to12 = ggarrange(fig9, fig10, fig11, fig12)
annotate_figure(fig9to12, top = text_grob("Previous Peak Distribution, Slope, Major vessels distribution, Thalium Stress distribution by Sex", color = "Black", size = 14))

annotate_figure(fig13, top = text_grob("Heart attack distribution by Sex", color = "Black", size = 14))

