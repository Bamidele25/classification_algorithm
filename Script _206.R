install.packages("tidyverse")
install.packages("corrplot")
install.packages("caTools")
install.packages("class")
install.packages("randomForest")
install.packages("DataExplorer")
install.packages("gridExtra")
install.packages("Boruta")

library(class)
library(tidyverse)
library(caret)
library(e1071)
library(corrplot)
library(MASS)
library(caTools)
library(randomForest)
library(DataExplorer)
library(gridExtra)
library(Boruta)

#set working directory
setwd('~/R')

#read in the data set 
thy = read.csv('AT_data.csv')
view(thy)
#record the attributes 
thy = thy %>%
  mutate(Class = recode(Class, P = 'positive',N = 'negative'),
         sex = recode(sex, F = 'female',M = 'Male'))
  
#replace ? with na 
thy = thy %>%
  mutate(sex = na_if(sex,'?'), 
         age = na_if(age, '?'),
         TSH = na_if(TSH, '?'),
         T3 = na_if(T3, '?'),
         TT4 = na_if(TT4, '?'),
         T4U = na_if(T4U, '?'),
         FTI = na_if(FTI,'?'),
         TBG = na_if(TBG, '?'))
glimpse(thy)
str(thy)
#convert to categorical attribute to factors and continus to numerical 
thy2 = thy %>%
  mutate(sex = as.factor(sex),
         Class = as.factor(Class),
         age = as.integer(age),
         on.thyroxine = as.factor(on.thyroxine),
         query.on.thyroxine = as.factor(query.on.thyroxine),
         on.antithyroid.medication = as.factor(on.antithyroid.medication),
         sick = as.factor(sick),
         pregnant = as.factor(pregnant),
         thyroid.surgery = as.factor(thyroid.surgery ),
         I131.treatment = as.factor(I131.treatment),
         query.hypothyroid = as.factor(query.hypothyroid),
         query.hyperthyroid = as.factor(query.hyperthyroid),
         lithium = as.factor(lithium),
         goitre = as.factor(goitre),
         tumor = as.factor(tumor),
         hypopituitary = as.factor(hypopituitary),
         TSH = as.integer(TSH),
         T3 = as.integer(T3),
         TT4 = as.integer(TT4),
         T4U = as.integer(T4U),
         FTI = as.integer(FTI))

str(thy2)
glimpse(thy2)
ncol(thy2)
nrow(thy2)
#Handling Missing Value 
is.na(thy2)
#display the chart of missing value 
plot_missing(thy2)
#drop 'TBG' colum since all the colum contain na 
thy2 <- dplyr::select(thy2,-(TBG))
#drop all the blood type measure 
#since the have value for measure in another colum
thy2 =dplyr::select(thy2,-(c(TT4.measured,TSH.measured,T4U.measured,FTI.measured,TBG.measured,referral.source))) 
thy2 = dplyr::select(thy2,-(T3.measured))
colnames(thy2)
#replace the missing value in the continous variable with the means
nw_thy <- thy2
for (i in which(sapply(nw_thy, is.numeric))) { 
  nw_thy[is.na(nw_thy[, i]), i] <- mean(nw_thy[, i],  na.rm = TRUE) 
}
#After the replacement with mean value then replot the missing chart 
plot_missing(nw_thy)
#we left with sex colum, since the percentage is just 3.98% missing value 
#we will drop he rows
cln_thy = na.omit(nw_thy)
#now replot the missing value 
plot_missing(cln_thy)
#DATA VISUALIZATION
#1. barplot of Thyroid Disease 
bar_target <- ggplot(cln_thy, aes(Class, fill = Class))+
  geom_bar()+
  theme_classic()+
  scale_color_brewer(palette = "Accent")+
  scale_fill_brewer(palette = "Accent")+
  theme(plot.background = element_rect(fill = "grey97"))+
  labs(title = "Bar Plot of Thyroid Disease", x = "Thyroid Disease", y = "Count")
bar_target

#Age Distribution 
ggplot(cln_thy,aes(x = age))+ 
geom_histogram(bins =30,fill ="dodgerblue4")+ 
theme_bw() + 
theme_classic()+
ggtitle("Age Distribution")+
ylab("Number of Patient")
#Age Density Distribution 
ggplot(cln_thy,aes(x = age)) + 
  geom_density(fill ="dodgerblue4") + 
  theme_bw() + theme_classic() +
  ggtitle("Age Distribution") +
  ylab("Number of Patient")
#Gender Bar Chart 
ggplot(cln_thy, aes(sex, fill = sex))+
  geom_bar()+
  xlab('Gender')+
  ylab('Count')+
  ggtitle('Barchart of the Gender')+
  scale_fill_discrete(name = 'Gender')
#Box plot for the Continous variable 
box_plot <- grid.arrange(ggplot(cln_thy, aes(age, age))+geom_boxplot(fill='red'), 
                         ggplot(cln_thy, aes(TSH, TSH))+geom_boxplot(fill = 'red'),
                         ggplot(cln_thy, aes(T3, T3))+geom_boxplot(fill='red'),
                         ggplot(cln_thy, aes(T4U, T4U))+geom_boxplot(fill='red'),
                         ggplot(cln_thy, aes(TT4, TT4))+geom_boxplot(fill='red'),
                         ggplot(cln_thy, aes(FTI, FTI))+geom_boxplot(fill='red')
)
box_plot

boxplot(cln_thy$T4U,main ="Boxplot of T4U for normality check",col ="dodgerblue4",notch = T)
boxplot(cln_thy$TSH,main ="Boxplot of TSH for normality check",col ="dodgerblue4",notch = T)

bar_graph <- grid.arrange(ggplot(cln_thy, aes(x = sex, fill = Class))+geom_bar(position = "fill"),
                          ggplot(cln_thy, aes(x = sick, fill = Class))+geom_bar(position = "fill"),
                          ggplot(cln_thy, aes(x = tumor, fill = Class))+geom_bar(position = "fill"),
                          ggplot(cln_thy, aes(x = pregnant, fill = Class))+geom_bar(position = "fill")
)
bar_graph
trt_graph = ggplot(cln_thy, aes(x=I131.treatment, fill= Class))+geom_bar(position = "fill")
trt_graph
#pregnanat chart 
women = cln_thy %>%
  filter(sex =="female")
view(women)
preg_graph = ggplot(women, aes(x= pregnant, fill= Class))+geom_bar(position = "fill")
preg_graph
#COrrelation for the continous varible 
cont_var = dplyr::select(cln_thy,c(age,TT4,T4U,T3,TSH,FTI))
corr_thy = cor(cont_var)
corr_thy

corrplot(corr_thy, method = 'square', type = 'full')
summary(cln_thy)
dim(cont_var)
#Handling Outliers 
boxplot(cont_var)
summary(cont_var)
#from the box plot and the statistics summary its obvious that TT4,T3,TSH,andFTI
#have outliers value
#hence it is necessary to treat the outliers before for the model in order to avoid
#spourious model 
#convert the classification category into binary 
view(cln_thy)
cln_thy2 <- cln_thy %>%
  mutate(Class = if_else(Class == 'positive', 1, 2),
         sex= if_else(sex == 't',1,2),
         on.thyroxine = if_else(on.thyroxine == 't',1,2),
         query.on.thyroxine = if_else(query.on.thyroxine == 't',1,2),
         on.antithyroid.medication = if_else(on.antithyroid.medication == 't',1,2),
         sick = if_else(sick == 't',1,2),
         pregnant = if_else(pregnant == 't',1,2),
         thyroid.surgery = if_else(thyroid.surgery == 't',1,2),
         I131.treatment = if_else(I131.treatment == 't',1,2),
         query.hypothyroid = if_else(query.hypothyroid == 't',1,2),
         query.hyperthyroid = if_else(query.hyperthyroid == 't',1,2),
         lithium = if_else(lithium == 't',1,2),
         goitre = if_else(goitre == 't',1,2),
         tumor = if_else(tumor == 't',1,2),
         hypopituitary = if_else(hypopituitary == 't',1,2),
         psych = if_else(psych == 't',1,2))
view(cln_thy2)
summary(cont_var)
#create bench_mark
bench_tt4 = 123.0 +1.5*IQR(cln_thy2$TT4)
bench_tt4
bench_t3 = 2.00 +1.5*IQR(cln_thy2$T3)
bench_t3
bench_tsh = 3.00 +1.5*IQR(cln_thy2$TSH)
bench_tsh
bench_fti = 122.0 +1.5*IQR(cln_thy2$FTI)
bench_fti

nw_thy2 = subset(cln_thy2,Class <= 2 & age <= 94 & sex <=2 & TT4<= 174 & T4U<=2 & T3<= 3.5 & TSH <= 7.5 & FTI<=164 & on.thyroxine <=2 & query.on.thyroxine<=2 & on.antithyroid.medication<=2 & sick<=2 & pregnant<=2 & thyroid.surgery<=2 & I131.treatment<=2 & query.hypothyroid<=2 & query.hyperthyroid<=2 & lithium<=2 & goitre<=2 & tumor<=2 & hypopituitary<=2 & psych<=2)
dim(nw_thy2)
view(nw_thy2)
boxplot(cont_var2)
cont_var2 = dplyr::select(Thyroid_data,c(age,TT4,T4U,T3,TSH,FTI))
#lower benchmark
bench_tt42 = 123.0-1.5*IQR(cln_thy2$TT4)
bench_tt42
bench_fti2 = 122.0-1.5*IQR(cln_thy2$FTI)
bench_fti2
#third clean data set 
Thyroid_data = subset(nw_thy2,Class <= 2 & age <= 94 & sex <=2 & TT4>= 72 & T4U<=2 & T3<= 3.5 & TSH <= 7.5 & FTI>=80 & on.thyroxine <=2 & query.on.thyroxine<=2 & on.antithyroid.medication<=2 & sick<=2 & pregnant<=2 & thyroid.surgery<=2 & I131.treatment<=2 & query.hypothyroid<=2 & query.hyperthyroid<=2 & lithium<=2 & goitre<=2 & tumor<=2 & hypopituitary<=2 & psych<=2)
dim(Thyroid_data)
#Box plot of COntinous Data set After Outlier Handling 
box_plot2 <- grid.arrange(ggplot(Thyroid_data, aes(age, age))+geom_boxplot(fill='yellow'), 
                         ggplot(Thyroid_data, aes(TSH, TSH))+geom_boxplot(fill = 'yellow'),
                         ggplot(Thyroid_data, aes(T3, T3))+geom_boxplot(fill='yellow'),
                         ggplot(Thyroid_data, aes(T4U, T4U))+geom_boxplot(fill='yellow'),
                         ggplot(Thyroid_data, aes(TT4, TT4))+geom_boxplot(fill='yellow'),
                         ggplot(Thyroid_data, aes(FTI, FTI))+geom_boxplot(fill='yellow')
)
box_plot2
#Feature Selection 
glimpse(Thyroid_data)
#convert to factor 
Thyroid_data = Thyroid_data %>%
  mutate(sex = as.factor(sex),
         Class = as.factor(Class),
         age = as.integer(age),
         on.thyroxine = as.factor(on.thyroxine),
         query.on.thyroxine = as.factor(query.on.thyroxine),
         on.antithyroid.medication = as.factor(on.antithyroid.medication),
         sick = as.factor(sick),
         pregnant = as.factor(pregnant),
         thyroid.surgery = as.factor(thyroid.surgery ),
         I131.treatment = as.factor(I131.treatment),
         query.hypothyroid = as.factor(query.hypothyroid),
         query.hyperthyroid = as.factor(query.hyperthyroid),
         lithium = as.factor(lithium),
         goitre = as.factor(goitre),
         tumor = as.factor(tumor),
         hypopituitary = as.factor(hypopituitary),
         TSH = as.integer(TSH),
         T3 = as.integer(T3),
         TT4 = as.integer(TT4),
         T4U = as.integer(T4U),
         FTI = as.integer(FTI))
summary(Thyroid_data)
#selecting the features using boruta 
set.seed(111)
brt_thy <- Boruta(Class ~ ., data = Thyroid_data, doTrace = 2, maxRuns = 500)
print(brt_thy)
plot(brt_thy, las = 2, cex.axis = 0.7)
plotImpHistory(brt_thy)
