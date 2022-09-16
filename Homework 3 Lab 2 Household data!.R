#Valery Delgado 
#Homework 3 based on Lab2
#Intro to Econometrics and Statistics B2000
#Group members:Cassidy Drummond and Sule Zakaria
#September 15, 2022

load("~/Downloads/Stats and Econometrics/Household_Pulse_data.RData")
install.packages("gmodels")
install.packages("descr")
library(descr)
install.packages("margins")

#We chose to compare sex, vaccination status, and whether or not they have had Covid-19. We found that 88.3% of females were vaccinated and 9.89% had been told by a doctor they had Covid-19. We found that 87.95% of males were vaccinated and 7.49% had been told by a doctor they had Covid-19. I am not confident as we do not know what factors make up the sample. For instance, did all participants have equal access to testing sites?
#Looking at the crosstabs and marginal probabilities, we found that our dataset was not statistically significant as the p-value is much greater than .05 (males = .35 and females = .33). Our crosstab is not mutually exclusive as some individuals who were vaccinated had not had covid and vice versa, despite their sex. Our crosstab is exhaustive as all possible outcomes are represented. 
#There are many factors that could impact the difference in outcome as the data’s collection of information was limited, and there are many compounding factors. We found there was an increase in vaccination status among higher educated individuals, and that higher educated females were vaccinated at a higher rate than males. However, when looking at education less than high school, the percent of females vaccinated was 10% less than their male counterparts. 
#Some additional evidence or context we would love to look out would be: political status, frequency of voting, religion, location (especially in a dense area), most viewed news sources, people they knew who had Covid-19, etc.. This information could determine the impact misinformation had on vaccination rates; political parties can be closely correlated with vaccination rates, etc.. Regarding my confidence, it is limited because I think a person’s decision on whether or not they get vaccinated is influenced by an extraordinary amount of factors. There are social, political, equitable, cultural, religious, historical, and economic influences, and I do not think we can quantify or test for the intersectionality of these influences. 


#This is to restrict the household data in terms of gender at birth, recieved vaccination, and covid status.
restrict2 <- (Household_Pulse_data$EGENID_BIRTH == "male") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
restrict1 <- (Household_Pulse_data$EGENID_BIRTH == "female") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
data_newf <- subset(Household_Pulse_data,restrict1)
data_newm <- subset(Household_Pulse_data, restrict2)

#summary for the data_new for females and males regarding their vaccination status and covid status.
summary(data_newf$RECVDVACC)
summary(data_newm$RECVDVACC)
summary(data_newf$HADCOVID)
summary(data_newm$HADCOVID)


#This is a cross table for females and males for vaccination status.
RECVDVACC = sample(c("yes got vaxx","no did not get vaxx","NA"), 68962, replace = TRUE)
EGENID_BIRTH = sample(c("female", "male"), 68962, replace = TRUE)
x = data.frame(RECVDVACC,EGENID_BIRTH)
CrossTable(RECVDVACC,EGENID_BIRTH)


#This is a cross table for females and males for covid status.
HADCOVID = sample(c("yes doctor told had covid","no did not","NA"), 68962, replace = TRUE)
EGENID_BIRTH = sample(c("female", "male"), 68962, replace = TRUE)
x = data.frame(HADCOVID,EGENID_BIRTH)
CrossTable(HADCOVID,EGENID_BIRTH)

# All of these four sections of code below are used to determine sd(standard deviation) and t-test for the standard error formula.
#Standard error formula is the sample standard deviation/ number of samples(n=68962)
sd(summary(data_newm$RECVDVACC))
t.test(summary(data_newm$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$RECVDVACC))
t.test(summary(data_newf$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$HADCOVID))
t.test(summary(data_newf$HADCOVID),var.equal = TRUE)

sd(summary(data_newm$HADCOVID))
t.test(summary(data_newm$HADCOVID),var.equal = TRUE)
#The prop table is used to determine the marginal probabilities in term of data_new for females and males
prop.table(summary(data_newf$RECVDVACC), margin = NULL)
prop.table(summary(data_newm$RECVDVACC), margin = NULL)
prop.table(summary(data_newf$HADCOVID), margin = NULL)
prop.table(summary(data_newm$HADCOVID), margin = NULL)

