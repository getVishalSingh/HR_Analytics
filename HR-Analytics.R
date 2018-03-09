#################====== HR ANALYTICS =====###################
#############################################################

########## Required library's ################

library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(scales)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)

## Employee Survey data:

employee_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
dim(employee_survey)
str(employee_survey)

## Employee General data:
generaldata <- read.csv("general_data.csv",stringsAsFactors = F)
dim(generaldata)
str(generaldata)

### Manager Survy Data :
manager_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
dim(manager_survey)
str(manager_survey)

## Check the duplicate Values survey'data and general data

table (duplicated(employee_survey$EmployeeID)) ### No Dulicates
table (duplicated(generaldata$EmployeeID)) ### No Dulicates
table (duplicated(manager_survey$EmployeeID)) ### No Dulicates

####  Identical EmployeeID across these datasets

setdiff(employee_survey$EmployeeID,y = generaldata$EmployeeID) #### Identical EmployeeID across these datasets
setdiff(employee_survey$EmployeeID,y = manager_survey$EmployeeID)### Identical EmployeeID across these datasets

#### Merge the Survey's data and General data

initial_data <- merge(employee_survey,generaldata,by="EmployeeID")
initial_data <- merge(initial_data,manager_survey,by="EmployeeID")

## Identify the Missing Values in initial_data
sum(is.na(initial_data)) #### 111 rows data missing
colSums(is.na(initial_data))
sort(colSums(is.na(initial_data)))

### Identified below Variables having Missing data

          # TotalWorkingYears - 9 rows missing
          # NumCompaniesWorked - 19 rowa Missing
          # JobSatisfaction- 20 rows missing
          # EnvironmentSatisfaction - 25 rows Missing
          # WorkLifeBalance - 38 Rows Missing
          # It means that 111/4410 = 0.02517007 i.e 2.5%, 
          # best is to remove these observations from the analysis

initial_data_1 = na.omit(initial_data)
dim(initial_data_1)

## ==================================================##
## Import the IN & Out time Data into R Envorinment
## Intime Data :

intime <- read.csv("in_time.csv",stringsAsFactors=F)
dim(intime)

## removing columns having all values as NA 
intime <- intime[colSums(!is.na(intime))>0]

##Checking for duplicate 
table(duplicated(intime))  ##no duplicate found
##converting to date format
intime <-  sapply(intime[,-1],function(x) as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S"))
intime <- data.frame(intime)
intime$X <- seq.int(nrow(intime))

#### Outtime Data :
outime <- read.csv("out_time.csv",stringsAsFactors=F)
dim(outime)

##Checking for NA
outime_na <- data.frame(colSums(is.na(outime)))  ## NA is present

##removing columns having all values as NA
outime <- outime[colSums(!is.na(outime))>0]
##converting to date format
outime <-  sapply(outime[,-1],function(x) as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S"))
outime <- data.frame(outime)
outime$X <- seq.int(nrow(outime))
 
#Creatigng a new dataframe having time spent in number of hours of each employee per day 
column_names <- names(intime)[-250]
timeinterval <- cbind(intime[250],outime[column_names]-intime[column_names])
timeinterval1 <- sapply(timeinterval,function(x) round(x,1))

#################################################################################
# Overall we have around 250 days of intime,outtime info of each employee
##Deriving new columns from this data namely
##min hr ,
#max hr ,
#mean hr , 
#no of days Present, 
#no of days Absent, 
#Perc Absent, 
#long leave taken by each employee in office

Time_interval_data <- timeinterval1[,c(2:250)]
EmployeeID <- timeinterval1[,1]
EmployeeTime2 <- NULL
EmployeeTime1 <- data.frame(matrix(vector(mode = 'numeric'), nrow = 1))
for(i in 1:nrow(Time_interval_data)){
  time <- Time_interval_data[i,]
  EmployeeTime1$MeanHr <- round(mean(time,na.rm=T),1)
  EmployeeTime1$MaxHr <- max(time,na.rm=T)
  EmployeeTime1$MinHr <- min(time,na.rm=T)
  EmployeeTime1$DaysPresent <- length(which(!is.na(time)))
  EmployeeTime1$DaysAbsent <- length(which(is.na(time)))
  EmployeeTime1$PercAbsent <- round(((length(which(is.na(time))))/length(time))*100,1)
  is.na.rle <- rle(is.na(time))
  is.na.rle$values <- is.na.rle$values & is.na.rle$lengths >=2
  EmployeeTime1$longLeave <- ifelse(sum(unlist(is.na.rle[2])) >=1,"Yes","No")
  EmployeeTime2 <- rbind(EmployeeTime2,EmployeeTime1)
}
time_data <- cbind(EmployeeID,EmployeeTime2)
View(time_data)

##### Merging the Inatial data and time data 
master_data <- merge(initial_data_1,time_data,by="EmployeeID")
View(master_data) ##master file 

##Checking for NA in master_data
colSums(is.na(master_data)) ##no NA present, data is good to start EDA

################################################################

### Data Preparation & Exploratory Data Analysis
##### Describe Data ########
##### Distribution of numeric data

theme <- theme(plot.title=element_text(color="red",face="bold",margin=margin(10,0,10,0)),axis.title.x=element_text(color="forestgreen",vjust=0.5,face="bold"),axis.title.y=element_text(color="forestgreen",vjust=0.5,face="bold"),panel.background= element_rect(fill="light blue"),legend.position="bottom",legend.direction="horizontal",strip.background = element_rect(fill="light blue"))
## Age : 
H1=ggplot(master_data,aes(x = Age))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue",binwidth = 1)+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of Age")+theme
  
## DistanceFromHome: 

H2= ggplot(master_data,aes(x = DistanceFromHome))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue",binwidth = 1)+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of DistanceFromHome")+theme

## MonthlyIncome :

H3= ggplot(master_data,aes(x = MonthlyIncome))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of MonthlyIncome")+theme


## PercentSalaryHike :

H4= ggplot(master_data,aes(x = PercentSalaryHike))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue",bins=15)+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of PercentSalaryHike")+theme


## TotalWorkingYears : 

H5= ggplot(master_data,aes(x = TotalWorkingYears))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of TotalWorkingYears")+theme

## YearsAtCompany : lot of Out layers in Company

H6= ggplot(master_data,aes(x = YearsAtCompany))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of YearsAtCompany")+theme


## MaxHr:

H7 = ggplot(master_data,aes(x = MaxHr))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of MaxHr")+theme


## YearsSinceLastPromotion

H8 = ggplot(master_data,aes(x = YearsSinceLastPromotion))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of YearsSinceLastPromotion")+theme

## YearsWithCurrManager

H9 = ggplot(master_data,aes(x = YearsWithCurrManager))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of YearsWithCurrManager")+theme


### Derived matrix Distribution
#MeanHr (Average Hours in a Day):

H10 = ggplot(master_data,aes(x = MeanHr))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue")+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of Average Hours in a Day")+theme

#DaysPresent:

H11 = ggplot(master_data,aes(x = DaysPresent))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue",bins = 10)+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of DaysPresent in a Year")+theme


#DaysAbsent:

H12 = ggplot(master_data,aes(x = DaysAbsent))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue",bins = 10)+
  geom_density(alpha = 0.2, fill = "red")+
  ggtitle(label = "Distribution of DaysAbsent in a Year")+theme

grid.arrange(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,
             top="Distribution of Contniuous Data")

#################### Categorical and Target Variable Relation #############
### Attrition Vs General Data

## Attrition~ Business Travel:

C1 = ggplot(master_data,aes(x = BusinessTravel,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ Business Travel ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = 0.1, 
           size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme

## Attrition~ Department:

C2 = ggplot(master_data,aes(x = Department,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ Department ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = 0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme


## Attrition~ Education:

C3 = ggplot(master_data,aes(x = Education,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ Department ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = 0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme

## Attrition~ EducationField:

C4= ggplot(master_data,aes(x = EducationField,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ EducationField ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = 0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme

## Attrition~ Gender:

C5 = ggplot(master_data,aes(x = Gender,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ Gender ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = 0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme

## Attrition~ JobLevel:

C6= ggplot(master_data,aes(x = JobLevel,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ JobLevel ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = 0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme

## Attrition~ MaritalStatus:

C7 = ggplot(master_data,aes(x = MaritalStatus,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ MaritalStatus ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = -0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme


## Attrition~ StockOptionLevel:

C8 = ggplot(master_data,aes(x = StockOptionLevel,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ StockOptionLevel ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = -0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme
  
## Attrition~ JobSatisfaction:

C9 = ggplot(master_data,aes(x = JobSatisfaction,fill=Attrition))+
  geom_bar(position = "dodge",width = 0.5)+
  ggtitle("Attrition~ JobSatisfaction ")+
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = -0.1, 
             size = 3,position = position_dodge(width = 0.5))+
  theme(legend.position="top")+theme
 
 
grid.arrange(C1,C2,C3,C4,C5,C6,C7,C8,C9,
             top="Bivariate analysis")
### Bi-Variate Analysis on Target Variable and Remaining Variables
colnames(master_data)

## Age :
B1 = ggplot(master_data,aes(x =Attrition,y=Age,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ Age Vs Attrition" )+guides(fill=FALSE)+theme

## DistanceFromHome: 

B2 = ggplot(master_data,aes(x =Attrition,y=DistanceFromHome,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ DistanceFromHome Vs Attrition" )+guides(fill=FALSE)+theme

## MonthlyIncome :

B3 = ggplot(master_data,aes(x =Attrition,y=MonthlyIncome,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ MonthlyIncome Vs Attrition" )+guides(fill=FALSE)+theme

## PercentSalaryHike :

B4 = ggplot(master_data,aes(x =Attrition,y=PercentSalaryHike,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ PercentSalaryHike Vs Attrition" )+guides(fill=FALSE)+theme

## TotalWorkingYears :

B5 = ggplot(master_data,aes(x =Attrition,y=TotalWorkingYears,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ TotalWorkingYears Vs Attrition" )+guides(fill=FALSE)+theme

## YearsAtCompany :

B6 = ggplot(master_data,aes(x =Attrition,y=YearsAtCompany,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ YearsAtCompany Vs Attrition" )+guides(fill=FALSE)+theme

## YearsSinceLastPromotion:

B7 = ggplot(master_data,aes(x =Attrition,y=YearsAtCompany,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ YearsSinceLastPromotion Vs Attrition" )+guides(fill=FALSE)+theme

## YearsWithCurrManager:

B8 = ggplot(master_data,aes(x =Attrition,y=YearsWithCurrManager,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ YearsWithCurrManager Vs Attrition" )+guides(fill=FALSE)+theme


## Derived matrix Distrubution:

## MeanHr (Average Hours in a Day):

B9 = ggplot(master_data,aes(x =Attrition,y=MeanHr,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ Average Hr in Year Vs Attrition" )+guides(fill=FALSE)+theme


## DaysPresent:

B10 = ggplot(master_data,aes(x =Attrition,y=DaysPresent,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ DaysPresent in Year Vs Attrition" )+guides(fill=FALSE)+theme

## DaysAbsent:

B11 = ggplot(master_data,aes(x =Attrition,y=DaysAbsent,fill=Attrition ))+
  geom_boxplot(outlier.colour = "red")+ggtitle("~ DaysAbsent in Year Vs Attrition" )+guides(fill=FALSE)+theme

grid.arrange(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,
             top="Relation Between Numbers Vs Attrition")
			 
#Attrition~MaritalStatus+Gender+JobRole
ggplot(master_data,aes(x = Attrition,fill=factor(MaritalStatus)))+
  geom_bar(position = "dodge")+
  ggtitle("Attrition ~ MaritalStatus+Gender+JobRole ") +
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = -0.2, 
             size = 3,position = position_dodge(width = 1))+
  theme(legend.position="bottom")+ facet_grid(Gender~JobRole,labeller=label_both)+guides(fill=guide_legend(title="MaritalStatus"))+theme

#Attrition~Education+JobLevel+EducationField

ggplot(master_data,aes(x = Attrition,fill=factor(Education)))+
  geom_bar(position = "dodge")+
  ggtitle("Attrition ~ JobLevel+EducationField+Education ") +
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = -0.2, 
             size = 3,position = position_dodge(width = 1))+
  theme(legend.position="bottom")+ facet_grid(JobLevel~EducationField,labeller=label_both)+guides(fill=guide_legend(title="Education"))+theme
  ### Attrition~ PerformanceRating+JobSatisfaction+EnvironmentSatisfaction:
ggplot(master_data,aes(x = Attrition,fill=factor(JobInvolvement)))+
  geom_bar(position = "dodge")+
  ggtitle("Attrition~ survey data Satisfaction Levels ") +
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  stat_count(aes(label = ..count..), geom = "text",vjust = -0.2, 
             size = 3,position = position_dodge(width = 1))+
  theme(legend.position="bottom")+ facet_grid(JobSatisfaction~EnvironmentSatisfaction,labeller=label_both)+guides(fill=guide_legend(title="JobInvolvement"))+theme
  
  ##############################################################################################
  ##Outlier treatment starts
  outliers <- sapply(master_data[,c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","MeanHr","MaxHr","MinHr","DaysPresent","DaysAbsent","PercAbsent")],function(x) quantile(x,seq(0,1,.01),na.rm = T))
  
  ##We can see outliers for MonthlyIncome,TotalWorkingYears,YearsAtCompany
  ##MonthlyIncome(There is a sharp rse from 137756.0 to 152020.0 b/w 90 to 91 % and the increment goes on till 100 with a value of 199990.0. Flooring all values above 90 to 137756.0(90% values).
   master_data$MonthlyIncome[which(master_data$MonthlyIncome >137756.0)] <- 137756.0
   
   ##TotalWorkingYears (There is a sharp rise in working years 32 to 35 and then to 40 from 98 to 100 %. Flooring all values above 98 to 98% value.
   master_data$TotalWorkingYears[which(master_data$TotalWorkingYears>32)] <- 32
   ##YearsAtCompany(There is a sharp rise from 22 to 40 b/w 96 to 100 %. Flooring all values above 96 to 96% value.
   master_data$YearsAtCompany[which(master_data$YearsAtCompany>22)] <- 22
   
##########################################################################
##missing values 
colSums(is.na(master_data)) ##no missing values present
# Feature standardisation

# Normalising continuous features 
master_data$Age <- scale(master_data$Age)
master_data$DistanceFromHome <- scale(master_data$DistanceFromHome)
master_data$MonthlyIncome <- scale(master_data$MonthlyIncome)
master_data$NumCompaniesWorked <- scale(master_data$NumCompaniesWorked)
master_data$PercentSalaryHike <- scale(master_data$PercentSalaryHike)
master_data$TotalWorkingYears <- scale(master_data$TotalWorkingYears)
master_data$TrainingTimesLastYear <- scale(master_data$TrainingTimesLastYear)
master_data$YearsAtCompany <- scale(master_data$YearsAtCompany)
master_data$YearsSinceLastPromotion <- scale(master_data$YearsSinceLastPromotion)
master_data$YearsWithCurrManager <- scale(master_data$YearsWithCurrManager)
master_data$MeanHr <- scale(master_data$MeanHr)
master_data$MaxHr <- scale(master_data$MaxHr)
master_data$MinHr <- scale(master_data$MinHr)
master_data$DaysPresent <- scale(master_data$DaysPresent)
master_data$DaysAbsent <- scale(master_data$DaysAbsent)
master_data$PercAbsent <- scale(master_data$PercAbsent)
# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
master_data$Attrition <- ifelse(master_data$Attrition=="Yes",1,0)

# Checking Attrition rate 

Attrition  <- sum(master_data$Attrition)/nrow(master_data)
Attrition # 16.16% Attrition rate.

# creating a dataframe of categorical features
master_data_chr <- master_data[,-c(1,5,9,12,17,18,19,20,21,23,24,25,26,27,30,31,32,33,34,35)]

# converting categorical attributes to factor
master_data_fact <- data.frame(sapply(master_data_chr, function(x) factor(x)))
str(master_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_data_fact))[,-1]))

# For variables having only two levels, longLeave, PerformanceRating "yes" is 1,
#gender "male" is 1

# Final dataset
HR_final<- cbind(master_data[,c(1,5,9,12,17,18,19,20,21,23,24,25,26,27,30,31,32,33,34,35)],dummies) 
View(HR_final) #4300 obs. of  66 variables 
##removing EMP ID, EmployeeCount, Over18, StandardHours
HR_final1 <- HR_final[,-c(1,4,7,9)]

# splitting the data between train and test
set.seed(100)

indices <- sample.split(HR_final1$Attrition, SplitRatio = 0.7)

train <- HR_final1[indices,]

test <- HR_final1[!(indices),]
###########################################################################
#model_creation starts
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2116.1....66 coeff..nullDev 2661.4...resDev 1996.1

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2) 
###AIC 2087.3..nullDev 2661.4...resDev 2013.3

##Checking multicollinearity by checking the value of VIF
vif(model_2)
##Removing MinHr
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr + MaxHr+ EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
    WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
    Department.xResearch...Development + Department.xSales + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 + JobRole.xSales.Representative + 
    Education.x2, family = "binomial", data = train)
##AIC: 2103.8..nullDev 2661.4...resDev 2033.8
##Checking multicollinearity by checking the value of VIF
vif(model_3)
##Removing MaxHr
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
    WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
    Department.xResearch...Development + Department.xSales + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 + JobRole.xSales.Representative + 
    Education.x2, family = "binomial", data = train)
#AIC: 2103.7..nullDev 2661.4...resDev 2035.7
##Checking multicollinearity by checking the value of VIF
vif(model_4)
##Removing BusinessTravel.xTravel_Rarely
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
    WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + Department.xSales + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 + JobRole.xSales.Representative + 
    Education.x2, family = "binomial", data = train)
  #AIC: 2110.3..nullDev 2661.4...resDev 2044.3
  ##Checking multicollinearity by checking the value of VIF
vif(model_5)
  ##Removing MaritalStatus.xMarried
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
    WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + Department.xSales + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 + JobRole.xSales.Representative + 
    Education.x2, family = "binomial", data = train)
#AIC: 2111..nullDev 2661.4...resDev 2047.0
##Checking multicollinearity by checking the value of VIF
vif(model_6)
##WorkLifeBalance.x2, WorkLifeBalance.x3,WorkLifeBalance.x4 have a VIF > than 2, checkinng for collinearilty b/w them 
ggpairs(train[,c("WorkLifeBalance.x2","WorkLifeBalance.x3","WorkLifeBalance.x4")])

##We can see WorkLifeBalance.x2, WorkLifeBalance.x3 are highly correlated with a value of -0.681. also in comparison to WorkLifeBalance.x3, WorkLifeBalance.x2 has a higher p value, so removing the same.

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + Department.xSales + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 + JobRole.xSales.Representative + 
    Education.x2, family = "binomial", data = train)
#AIC: 2127.8..nullDev 2661.4...resDev 2065.8
##Checking multicollinearity by checking the value of VIF
vif(model_7)
##We can see Department.xResearch...Development,Department.xSales have a high vif value, checking for collinearity b/w them 
ggpairs(train[,c("Department.xResearch...Development","Department.xSales")])
##Corr -0.904 i.e. they are highly correlated
##removing Department.xSales having relatively higher p value 
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 + JobRole.xSales.Representative + 
    Education.x2, family = "binomial", data = train)
#AIC: 2139.7..nullDev 2661.4...resDev 2079.7
## Now there is no multicollienarity as vif of all variables are around 2, eliminating variables based on p value 
##removing JobRole.xSales.Representative 
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 + 
    JobInvolvement.x2 + JobInvolvement.x3 +
    Education.x2, family = "binomial", data = train)

#AIC: 2137.9..nullDev 2661.4...resDev 2079.9
##removing JobInvolvement.x2
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3 +
    Education.x2, family = "binomial", data = train)
#AIC: 2136..nullDev 2661.4...resDev 2080.0
#removing PercentSalaryHike
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3 +
    Education.x2, family = "binomial", data = train)
#AIC: 2134.7..nullDev 2661.4...resDev 2080.7
#removing WorkLifeBalance.x4
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + EducationField.xMarketing + JobLevel.x5 + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3 +
    Education.x2, family = "binomial", data = train)
#AIC: 2134.3..nullDev 2661.4...resDev 2082.3
#removing EducationField.xMarketing
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + JobLevel.x5 +JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3 +
    Education.x2, family = "binomial", data = train)
#AIC: 2133.8..nullDev 2661.4...resDev 2083.8
#removing Education.x2
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
    Education.x5 + JobLevel.x5 +JobRole.xManufacturing.Director + JobRole.xResearch.Director+
    MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3
    , family = "binomial", data = train)
#AIC: 2134..nullDev 2661.4...resDev 2086.0
#removing Department.xResearch...Development
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently +
    Education.x5 + JobLevel.x5 +JobRole.xManufacturing.Director + JobRole.xResearch.Director+
    MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3
    , family = "binomial", data = train)
#AIC: 2135.1..nullDev 2661.4...resDev 2089.1
#removing Education.x5
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently +JobLevel.x5 +JobRole.xManufacturing.Director 
	+ JobRole.xResearch.Director+MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3
    , family = "binomial", data = train)
#AIC: 2137.1..nullDev 2661.4...resDev 2093.1
#removing JobLevel.x5
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director 
	+ JobRole.xResearch.Director+MaritalStatus.xSingle + StockOptionLevel.x1 +JobInvolvement.x3
    , family = "binomial", data = train)
#AIC: 2139.1..nullDev 2661.4...resDev 2097.1
#removing StockOptionLevel.x1 
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director 
	+ JobRole.xResearch.Director+MaritalStatus.xSingle+JobInvolvement.x3
    , family = "binomial", data = train)
#AIC: 2142.7..nullDev 2661.4...resDev 2102.7
#removing JobInvolvement.x3
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director 
	+ JobRole.xResearch.Director+MaritalStatus.xSingle
    , family = "binomial", data = train)
#AIC: 2147.4..nullDev 2661.4...resDev 2109.4
#removing JobRole.xResearch.Director
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + MeanHr +EnvironmentSatisfaction.x2 + 
    EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
    JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +
    BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director 
	+MaritalStatus.xSingle, family = "binomial", data = train)
#AIC: 2151.4..nullDev 2661.4...resDev 2115.4
##Now the model looks good as all variables have a low p value(all are ***) and vif below 2. Now it's time to use this model to predict attrition of test data set.
Final_model <- model_20

### Test Data ####

#predicted probabilities of attrition for test data

test_pred = predict(Final_model, type = "response", 
                              newdata = test[,-26])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 
library(e1071)
library(caret)
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
##[1] 0.1855556

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1855556, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
## so as sensitivity, specificity and accuracy of this model is around 77%, means it is a good model.
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
##0.5370581
################################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_cutoff_attrition, groups = 10)

##plotting gain chart 
Attrition_decile <- Attrition_decile%>%
 mutate(random_gain=Gain/Cumlift)

ggplot(Attrition_decile, aes(bucket)) + 
  geom_line(aes(y = Gain, colour="Gain"),size=1) + 
  geom_line(aes(y = random_gain, colour="random_gain"),size=1)+ggtitle("Gain Chart")+
  theme(plot.title=element_text(size=25,color="red",face="bold",margin=margin(10,0,10,0)),axis.title.x=element_text(size=25,color="forestgreen",vjust=0.5,face="bold"),axis.title.y=element_text(size=25,color="forestgreen",vjust=0.5,face="bold"),panel.background= element_rect(fill="light blue"),legend.position="bottom",legend.direction="horizontal",strip.background = element_rect(fill="light blue"))
##########################################################################################







