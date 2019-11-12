#Load libraries
library(lubridate)
library(chron)
library(dplyr)
library(binr)
library(MASS)
library(LICORS)
library(factoextra)
library(cluster)
library(ggplot2)
require(ggpubr)
library(reshape2)
library(InformationValue)
library(car)
library(randomForest)
library(tree)
library(ROSE)
library(survival)
library(survminer)
library(glmnet)
library(plotROC)
library(pROC)
library(ggthemes)
library(e1071) 
library(plotly)
library(rms)
library(survival)
library(psych)

#Data acquisition and preparation
#Read the data
data<-read.csv(file = "final_data_set.csv", 
               sep = ",", header=TRUE)
names(data) <- sub("final_data_set.","",names(data)) 
data$transaction_time <- sub(":","",data$transaction_time) 
data$transaction_time <- sub(":","",data$transaction_time) 
data$transaction_time <- paste0(substr(data$transaction_time,1,2),":",substr(data$transaction_time,3,4),":", substr(data$transaction_time,5,6))
data$transaction_time<-chron(times=data$transaction_time)
data<-transform(data, transaction_date = as.Date(as.character(transaction_date), "%Y%m%d"))



#GROUP BY CUSTOMER ID
data_new <- data %>%
  group_by(cust_id) %>%
  dplyr::summarise(trans_num=n(),tot_units=sum(total_units),priv_items=sum(new_max_mdd),recency=as.numeric(as.Date('2019-01-01')-max(transaction_date)),
                   frequency=n(), monetary= sum(total_spend), stores=n_distinct(venue_number), un_cat=mean(unique_categories),un_brand=mean(unique_brands), un_upcs= sum(unique_upcs), promo=sum(new_max_promo),
                   prc_uniq_prod=un_upcs/tot_units*100)


#Skewness of recency, frequency and monetary variables
skew<-data.frame(skewness(data_new$recency),
                 skewness(data_new$frequency),
                 skewness(data_new$monetary))

#BOX-COX TRANSFORMATION
x<-data_new$recency
out <- boxcox(lm(x~1))
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
y1<- (x**(-0.1010101)-1)/(-0.1010101)
hist(y1)


x<-data_new$frequency
out <- boxcox(lm(x~1))
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
y2<-((x**(0.1414141)-1)/0.1414141)


x <- data_new$monetary
out <- boxcox(lm(x~1))
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
y3<-((x**(0.1414141)-1)/0.1414141)
hist(y3)

#BINNING
df_RFM<-data_new
bins_rec<-cut(y1,5, include.lowest = TRUE, labels = c("5", "4", "3", "2", "1"), dig.lab=10)
df_RFM$rec_score<-bins_rec
df_RFM$rec_score<-as.numeric(levels(df_RFM$rec_score))[df_RFM$rec_score]
barplot(table(df_RFM$rec_score))


bins_freq<-cut(y2,5, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"), dig.lab=10)
cut(y2,5, include.lowest = TRUE, dig.lab=10)
df_RFM$freq_score<-bins_freq
df_RFM$freq_score<-as.numeric(levels(df_RFM$freq_score))[df_RFM$freq_score]
barplot(table(df_RFM$freq_score))

bins_mon<-cut(y3,5, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"), dig.lab=10)
cut(y3,5, include.lowest = TRUE, dig.lab=10)
df_RFM$mon_score<-bins_mon
cut(y3,5, include.lowest = TRUE)
df_RFM$mon_score<-as.numeric(levels(df_RFM$mon_score))[df_RFM$mon_score]
barplot(table(df_RFM$mon_score))

rfm_scores<-df_RFM[,c('rec_score','freq_score',"mon_score")]
rfm_scores_copy<-rfm_scores
#ELBOW METHOD
fviz_nbclust(rfm_scores, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")

#Read saved clustering results
clf=readRDS("clf_final.rds")
#Cluster plot
fviz_cluster(clf, data = rfm_scores,geom='point',ellipse = TRUE, ellipse.type = "convex") + theme_minimal() +geom_point()
#Assign to each customer the corresponding segment
df_RFM$class<-clf$cluster
#Calculate general means
mean_rec<-as.numeric(format(round(mean(df_RFM$recency), 2), nsmall = 2))
mean_freq<-as.numeric(format(round(mean(df_RFM$frequency), 2), nsmall = 2))
mean_mon<-as.numeric(format(round(mean(df_RFM$monetary), 2), nsmall = 2))
print(paste(mean_rec,mean_freq,mean_mon))


#Compare clusters centroids with general means (output:arrows)
n<-length(clf$size)
for (i in 1:n)    {
  print(paste('Class ',i))
  if (mean(df_RFM$recency[df_RFM$class==i])<=mean_rec) {
    print('rec ^')
  }
  else {print('rec v')}
  if (mean(df_RFM$frequency[df_RFM$class==i])>=mean_freq) {
    print('freq ^')
  }
  else {print('freq v')}
  if (mean(df_RFM$monetary[df_RFM$class==i])>=mean_mon) {
    print('mon ^')
  }
  else {print('mon v')}
}   

#Means of Recency, yearly Frequency and monthly Monetary & Money per transaction by cluster
for (i in 1:n)    {
  print(paste('Class ',i))
  print(mean(df_RFM$recency[df_RFM$class==i]))
  print(mean(df_RFM$frequency[df_RFM$class==i]/2))
  print(mean(df_RFM$monetary[df_RFM$class==i]/24))
  print(sum(df_RFM$monetary[df_RFM$class==i])/sum(df_RFM$frequency[df_RFM$class==i]))
}   


#DEFINITION OF CUSTOMER CHURN. FIVE DIFFERENT APPROACHES ARE PROPOSED.

#Definition based on Trend
tr_data<- data[,c('cust_id','transaction_date','total_spend')]
tr_data<-tr_data[order(tr_data$cust_id,tr_data$transaction_date),]
tr_data$ord <- ave(tr_data$total_spend, tr_data$cust_id, FUN = seq_along)

a<-vector()
for (i in 1:5130) {
  b<-tr_data[tr_data$cust_id==i,]
  co<-cor(b$total_spend,b$ord,method="spearman")
  a<-rbind(a,co)
}

df_RFM_copy<-df_RFM
df_RFM$co<-c(a)

i=-0.4
df_RFM$churn1<-ifelse(df_RFM$co<= i & df_RFM$rec_score<=3 & df_RFM$freq_score<=4, 1,0)   
df_RFM$churn1[df_RFM$trans_num <= 5 & df_RFM$rec_score ==1] <- 1
na<-which(is.na(df_RFM$co))
df_RFM<-df_RFM[!is.na(df_RFM$co),]
add_nan<-df_RFM_copy[df_RFM_copy$cust_id %in% na,]
if (nrow(add_nan)!=0){
  add_nan$co<- -1
  add_nan$churn1<-1}
df_RFM<-rbind(df_RFM,add_nan)

table(df_RFM$class[df_RFM$churn1==1])
table(df_RFM$churn1)
prop.table(table(df_RFM$churn1))


#Definition based on inter-purchase time
med<-vector()
churn<-vector()

#For each customer, if the recency is greater than the 99th percentile of personal inter-purchase distribution, then churner. Otherwise, no-churner. 
for (i in 1:5130){
  df_cust1<-data[data$cust_id==i,]
  df_cust1 <- df_cust1 %>% 
    group_by(cust_id,transaction_date) %>% 
    dplyr::summarise(trans_num=n(),tot_units=sum(total_units),priv_items=sum(new_max_mdd),tot_spend=sum(total_spend))
  df_cust1<-df_cust1[order(df_cust1$cust_id,df_cust1$transaction_date),]
  df_cust1$time<-as.numeric(as.Date(df_cust1$transaction_date)-lag(df_cust1$transaction_date))
  df_cust1$time[is.na(df_cust1$time)]<-0
  churn[i]<-ifelse(df_RFM$recency[df_RFM$cust_id==i]>unname(quantile(df_cust1$time,0.99)),1,0)
}

df_RFM<-df_RFM[order(df_RFM$cust_id),]
#Assign to each customer the corresponding churn value
df_RFM$churn2<-churn
#Customer churn by segment
table(df_RFM$class[df_RFM$churn2==1])
#Customer churn
table(df_RFM$churn2)
#Customer churn percentage
prop.table(table(df_RFM$churn2))

#Example: Histogram of inter-purchase time for a specific customer.
hist(df_cust1$time,main='Inter-purchase time frequency \n customer id = 18',xlab = 'inter-purchase time (days)',col = 'grey')


#SURVIVAL DATES INSTEAD OF TRANSACTIONS

#For each customer, we calculate the number of different visits (unique transaction dates=visits)
data_diff_trans <- data %>% 
  group_by(cust_id) %>% 
  dplyr::summarise(trans_num=n(),tot_units=sum(total_units),priv_items=sum(new_max_mdd),recency=as.numeric(as.Date('2019-01-01')-max(transaction_date)),
                   frequency=n(), monetary= sum(total_spend),diff_dates=length(unique(transaction_date)))

for (i in 1:5130){
  df_RFM$uniq_dates[df_RFM$cust_id==i]=data_diff_trans$diff_dates[data_diff_trans$cust_id==i]
}

#RULE 1
#Transactions<=5 and recency>129 days
churners<-df_RFM$cust_id[df_RFM$frequency<=5 & df_RFM$rec_score==1]
#RULE 3
#Last transaction within the last 8 days and frequency score >=3
non_churners<-df_RFM$cust_id[df_RFM$rec_score>=4 & df_RFM$freq_score>=3]
#Customers that were not selected by Rules 1 & 2.
ids<-df_RFM$cust_id[!(df_RFM$cust_id %in% churners) & !(df_RFM$cust_id %in% non_churners)]
df_RFM<-df_RFM[order(df_RFM$cust_id),]
df<-data.frame()
med<-vector()
churn<-vector()
k<-1
#Calculate execution algorithm time
start_time <- Sys.time()
#time=inter-purchase time vector, futstat=0, if the observation is consored (last observation "recency" is considered as censored)
#No-Churn (churn value=0), if recency (referred as last_time) <= median
  for (i in ids){
  df_cust1<-data[data$cust_id==i,]
  df_cust1 <- df_cust1 %>% 
    group_by(cust_id,transaction_date) %>% 
    dplyr::summarise(trans_num=n(),tot_units=sum(total_units),priv_items=sum(new_max_mdd),tot_spend= sum(total_spend))
  df_cust1<-df_cust1[order(df_cust1$cust_id,df_cust1$transaction_date),]
  df_cust1$time<-as.numeric(as.Date(df_cust1$transaction_date)-lag(df_cust1$transaction_date))
  df_cust1$time[is.na(df_cust1$time)]<-0
  df_cust1$fustat<-1
  time<-append(df_cust1$time,df_RFM$recency[df_RFM$cust_id==i])
  fustat<-append(df_cust1$fustat,0)
  surv.obj<-Surv(time,fustat)
  fit1 <- survfit(surv.obj~ 1)
  med[i]<-unname((summary(fit1)$table['median']))
  last_time<-df_RFM$recency[df_RFM$cust_id==i]
  ggsurvplot(fit1,data=df_cust1)
  t<-fit1$time
  s<-fit1$surv
  d<-abs(s-0.05)
  if (last_time <= med[i]) {churn[i]=0
  } else if (last_time >= min(t[d==min(d)])) {churn[i]=1
  } else {churn[i]=2}
  k<-k+1}
end_time <- Sys.time()
end_time - start_time
table(churn)
#no-churn
for(i in non_churners){
  churn[i]<-0
}
#churn
for (i in churners){
  churn[i]<-1
}

#Assign to each customer the corresponding churn value
df_RFM$churn3<-churn

#ids for churners, no-churners and not-sure(neither churners nor no-churners as defined earlier)
ids_ch<-which(churn==1)
ids_no_ch<-which(churn==0)
not_sure<-which(churn==2)
table(df_RFM$class[df_RFM$cust_id %in% not_sure])

#Analysis for not sure customers
#Split data set into two subsets: bf_sept (trabsactions before September 2018) and af_sept (transactions after September 2018)
bf_sept<-data[data$transaction_date <= as.Date('2018-08-31'),]
af_sept<-data[data$transaction_date > as.Date('2018-08-31'),]
#Customer id by subset
bf_sept_id<-unique(bf_sept$cust_id)
af_sept_id<-unique(af_sept$cust_id)
#Find customers that belong to both of subsets
inters<-intersect(bf_sept_id,af_sept_id)

#If customers do not belong into both subsets, then churn
df_RFM$churn3[df_RFM$churn3==2 & !(df_RFM$cust_id %in% inters)]<-1
#The following Dormant customers have completed 6-8 transactions in the period of 2 years and they are assumed as churners
df_RFM$churn3[df_RFM$cust_id %in% inters & df_RFM$class==1 & df_RFM$churn3==2]<-1
#The rest customers are considered as no-churners.
df_RFM$churn3[df_RFM$churn3==2]<-0

#Customer churn by segment
table(df_RFM$class[df_RFM$churn3==1])
#Customer churn
table(df_RFM$churn3)
#Customer churn percentage
prop.table(table(df_RFM$churn3))


#Definition based on Survival probability

survival<-vector()
for (i in 1:5130){
  df_cust1<-data[data$cust_id==i,]
  df_cust1 <- df_cust1 %>% 
    group_by(cust_id,transaction_date) %>% 
    dplyr::summarise(trans_num=n(),tot_units=sum(total_units),priv_items=sum(new_max_mdd),tott_spend= sum(total_spend))
  df_cust1<-df_cust1[order(df_cust1$cust_id,df_cust1$transaction_date),]
  df_cust1$time<-as.numeric(as.Date(df_cust1$transaction_date)-lag(df_cust1$transaction_date))
  df_cust1$time[is.na(df_cust1$time)]<-0
  df_cust1$fustat<-1
  time<-append(df_cust1$time,df_RFM$recency[df_RFM$cust_id==i])
  fustat<-append(df_cust1$fustat,0)
  
  surv.obj<-Surv(time,fustat)
  fit1 <- survfit(surv.obj~ 1)
  s<-summary(fit1,times=df_RFM$recency[df_RFM$cust_id==i])
  survival[i]<-s$surv
}

#Assign to each customer the corresponding survival probability
df_RFM$surv<-survival
#RULE 1
churners<-df_RFM$cust_id[df_RFM$frequency<=5 & df_RFM$rec_score==1]
#RULE 3
non_churners<-df_RFM$cust_id[df_RFM$rec_score>=4 & df_RFM$freq_score>=3]

#Customers that were selected by Rule 1, are churners and their survival probability is assigned as 0.01
#Customers that were selected by Rule 3, are no-churners and their survival probability is assigned as 0.99
df_RFM$surv[df_RFM$cust_id %in% churners]<-0.01
df_RFM$surv[df_RFM$cust_id %in% non_churners]<-0.99

#Customers with survival probability<=0.1 are churners, survival probability>=0.5 are no-churners and the rest are customers at risk
df_RFM$churn4[df_RFM$surv<=0.1]<-1
df_RFM$churn4[df_RFM$surv>=0.5]<-0
df_RFM$churn4[df_RFM$surv>0.1 & df_RFM$surv<0.5]<-2


#Customer churn
table(df_RFM$churn4)
#Customer churn percentage
prop.table(table(df_RFM$churn4))
#Customers by segment and churn value
table(df_RFM$class[df_RFM$churn4==2])
table(df_RFM$class[df_RFM$churn4==1])
table(df_RFM$class[df_RFM$churn4==0])


#Definition based on customer reappearance

#Recompute customer features, taking as reference date "01-09-2018"
tr_bf_sept<-bf_sept %>% group_by(cust_id) %>% summarise(tot_units=sum(total_units),recency=as.numeric(as.Date('2018-09-01')-max(transaction_date)),frequency=n(),monetary=sum(total_spend),
                                                        priv_items=sum(new_max_mdd),stores=n_distinct(venue_number),  un_cat=mean(unique_categories),un_brand=mean(unique_brands),
                                                        un_upcs= sum(unique_upcs), promo=sum(new_max_promo))


#Group by customer
tr_af_sept <- af_sept %>% group_by(cust_id)

#Recompute segments 
x<-tr_bf_sept$recency
y1<- (x**(-0.1010101)-1)/(-0.1010101)
hist(y1)

x<-tr_bf_sept$frequency
y2<-((x**(0.1414141)-1)/0.1414141)
hist(y2)

x <- tr_bf_sept$monetary
y3<-((x**(0.1414141)-1)/0.1414141)
hist(y3)

#Binning
df_RFM1<-tr_bf_sept
bins_rec<-cut(y1,5, include.lowest = TRUE, labels = c("5", "4", "3", "2", "1"), dig.lab=10)
df_RFM1$rec_score<-bins_rec
df_RFM1$rec_score<-as.numeric(levels(df_RFM1$rec_score))[df_RFM1$rec_score]
barplot(table(df_RFM1$rec_score))

bins_freq<-cut(y2,5, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"), dig.lab=10)
df_RFM1$freq_score<-bins_freq
df_RFM1$freq_score<-as.numeric(levels(df_RFM1$freq_score))[df_RFM1$freq_score]
barplot(table(df_RFM1$freq_score))

bins_mon<-cut(y3,5, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"), dig.lab=10)
df_RFM1$mon_score<-bins_mon
cut(y3,5, include.lowest = TRUE)
df_RFM1$mon_score<-as.numeric(levels(df_RFM1$mon_score))[df_RFM1$mon_score]
barplot(table(df_RFM1$mon_score))

#Rfm scores of customers for the period before September 2018.
rfm_scores<-df_RFM1[,c('rec_score','freq_score',"mon_score")]

#Compute mean values of Recency, yearly Frequency and monthly Monetary (Note that the months before September 2018 are 20)
for (i in 1:6)    {
  print(paste('Class ',i))
  print(mean(df_RFM$recency[df_RFM$class==i]))
  print(mean(df_RFM$frequency[df_RFM$class==i]/(1.66)))
  print(mean(df_RFM$monetary[df_RFM$class==i]/20))
  print(sum(df_RFM$monetary[df_RFM$class==i])/sum(df_RFM$frequency[df_RFM$class==i]))
}  

#RFM scores by segment (i.e Segment 1 contains rfm scores like "111","112","211", etc.)
df_RFM$rfm<-paste(df_RFM$rec_score,df_RFM$freq_score,df_RFM$mon_score)
class1<-unique(df_RFM$rfm[df_RFM$class==1])
class2<-unique(df_RFM$rfm[df_RFM$class==2])
class3<-unique(df_RFM$rfm[df_RFM$class==3])
class4<-unique(df_RFM$rfm[df_RFM$class==4])
class5<-unique(df_RFM$rfm[df_RFM$class==5])
class6<-unique(df_RFM$rfm[df_RFM$class==6])

#Customer segmentation by rfm score (e.g. a customer which at the end of August 2018, held rfm score "111" is classified to Dormant segment)
df_RFM1$rfm<-paste(df_RFM1$rec_score,df_RFM1$freq_score,df_RFM1$mon_score)  
df_RFM1$class<-ifelse(df_RFM1$rfm %in% class1, 1, ifelse(df_RFM1$rfm %in% class2, 2, ifelse(df_RFM1$rfm %in% class3, 3, ifelse(df_RFM1$rfm %in% class4, 4,
                                                                                                                               ifelse(df_RFM1$rfm %in% class5, 5, 6)))))
df_RFM1$class2<-df_RFM$class[df_RFM$cust_id %in% df_RFM1$cust_id]

#Customers by segment
table(df_RFM1$class)

tr_af_sept_id<-unique(tr_af_sept$cust_id)
#Customers that did not complete any transaction the last 4 months of the whole period, are churners
churners_2<-setdiff(unique(bf_sept$cust_id),unique(af_sept$cust_id))
#Customers that completed at least one transaction the last 4 months of the whole period, are no-churners
no_churners_2<-intersect(unique(bf_sept$cust_id),unique(af_sept$cust_id))

#Assign to each customer the corresponding churn value
df_RFM$churn<-ifelse(df_RFM$cust_id %in% churners_2,1,0)
#Customer churn
table(df_RFM$churn)
#Customer churn percentage
prop.table(table(df_RFM$churn))

#Assign churn value to customers that belong to "Before September 2018 transactions" set.
for (i in 1:5130){
  if (i %in% df_RFM1$cust_id){
    df_RFM1$churn[df_RFM1$cust_id==i]=df_RFM$churn[i]}
}


#Customer churn by segment (segments correspond to those at the end of August 2018)
table(df_RFM1$class[df_RFM1$churn==1])

#save the data for visualization
#write.csv(df_RFM, file = "data_tableau.csv")


#PREDICTIVE MODELS

#MODEL 1

#The procedure below was followed to derive the results in master thesis
#Split Data set into train and test data
tr_rows<-sample(1:nrow(df_RFM), 0.7*nrow(df_RFM))
train<-df_RFM[tr_rows,]
test<-df_RFM[-tr_rows,]
#GLM model
logitMod <- glm(churn2 ~ tot_units+priv_items+un_cat+un_brand+un_upcs+promo+frequency+monetary+stores, data=train, family=binomial('logit'))
predicted <- predict(logitMod, test, type="response")  # predicted scores
optCutOff <- optimalCutoff(test$churn2, predicted)[1] 
plotROC(test$churn2, predicted)
fitted.results <- ifelse(predicted > optCutOff,1,0)
confusionMatrix(test$churn2, fitted.results)
misClasificError <- mean(fitted.results != test$churn2)
print(paste('Accuracy',1-misClasificError))
summary(logitMod)

#To result the same model described in thesis, run the following code:
#read the saved glm model

z=readRDS("./model1.rds")
summary(z)

#Latex code for summary table of model
xtable(z)
#ODDS
cbind(exp(coef(z)))


#MODEL 2

#The same procedure is followed (Note that the explanatory variables of the model are different for two proposed models)
tr_rows<-sample(1:nrow(df_RFM1), 0.7*nrow(df_RFM1))
train<-df_RFM1[tr_rows,]
test<-df_RFM1[-tr_rows,]

logitMod <- glm(churn ~ tot_units +priv_items+un_brand+un_upcs+recency+frequency+monetary+stores, data=train, family=binomial('logit'))
predicted <- predict(logitMod, test[,-17], type="response")  # predicted scores
optCutOff <- optimalCutoff(test$churn, predicted)[1] 
plotROC(test$churn, predicted)
fitted.results <- ifelse(predicted > optCutOff,1,0)
confusionMatrix(test$churn, fitted.results)
misClasificError <- mean(fitted.results != test$churn)
print(paste('Accuracy',1-misClasificError))
summary(logitMod)

#To result the same model described in thesis, run the following code:
#read the saved glm model
x=readRDS("./model2.rds")
summary(x)

#Latex code for summary table of model
xtable(x)
#ODDS
cbind(exp(coef(x)))


#Definitions agreement
#Agreement matrices, Accuracy, Cohen's coefficient kappa
#Definitions 1 & 2
confusionMatrix(df_RFM$churn1,df_RFM$churn2)
(confusionMatrix(df_RFM$churn1,df_RFM$churn2)[1,1]+confusionMatrix(df_RFM$churn1,df_RFM$churn2)[2,2])/5130
x12<-data.frame(df_RFM$churn1,df_RFM$churn2)
cohen.kappa(x12)
#Definitions 1 & 3
confusionMatrix(df_RFM$churn1,df_RFM$churn3)
(confusionMatrix(df_RFM$churn1,df_RFM$churn3)[1,1]+confusionMatrix(df_RFM$churn1,df_RFM$churn3)[2,2])/5130
x13<-data.frame(df_RFM$churn1,df_RFM$churn3)
cohen.kappa(x13)
#Definitions 1 & 5
confusionMatrix(df_RFM$churn1,df_RFM$churn)
(confusionMatrix(df_RFM$churn1,df_RFM$churn)[1,1]+confusionMatrix(df_RFM$churn1,df_RFM$churn)[2,2])/5130
x15<-data.frame(df_RFM$churn1,df_RFM$churn)
cohen.kappa(x15)
#Definitions 2 & 3
confusionMatrix(df_RFM$churn2,df_RFM$churn3)
(confusionMatrix(df_RFM$churn2,df_RFM$churn3)[1,1]+confusionMatrix(df_RFM$churn2,df_RFM$churn3)[2,2])/5130
x23<-data.frame(df_RFM$churn2,df_RFM$churn3)
cohen.kappa(x23)
#Definitions 2 & 5
confusionMatrix(df_RFM$churn2,df_RFM$churn)
(confusionMatrix(df_RFM$churn2,df_RFM$churn)[1,1]+confusionMatrix(df_RFM$churn2,df_RFM$churn)[2,2])/5130
x25<-data.frame(df_RFM$churn2,df_RFM$churn)
cohen.kappa(x25)
#Definitions 3 & 5
confusionMatrix(df_RFM$churn3,df_RFM$churn)
(confusionMatrix(df_RFM$churn3,df_RFM$churn)[1,1]+confusionMatrix(df_RFM$churn3,df_RFM$churn)[2,2])/5130
x45<-data.frame(df_RFM$churn4,df_RFM$churn)
cohen.kappa(x45)








#Clustering with input k=5 and Box-Cox transformation (Compare results)
clf<-kmeans(rfm_scores,5)
fviz_cluster(clf, data = rfm_scores,geom='point',ellipse = TRUE, ellipse.type = "convex") + theme_minimal() +geom_point()
df_RFM$class<-clf$cluster
mean_rec<-as.numeric(format(round(mean(df_RFM$recency), 2), nsmall = 2))
mean_freq<-as.numeric(format(round(mean(df_RFM$frequency), 2), nsmall = 2))
mean_mon<-as.numeric(format(round(mean(df_RFM$monetary), 2), nsmall = 2))
print(paste(mean_rec,mean_freq,mean_mon))


#Clustering without transformation (Compare results) and input k=6
y1<-data_new$recency
y2<-data_new$frequency
y3<-data_new$monetary
df_RFM<-data_new
bins_rec<-cut(y1,5, include.lowest = TRUE, labels = c("5", "4", "3", "2", "1"), dig.lab=10)
cut(y1,5, include.lowest = TRUE, dig.lab=10)
df_RFM$rec_score<-bins_rec
df_RFM$rec_score<-as.numeric(levels(df_RFM$rec_score))[df_RFM$rec_score]
barplot(table(df_RFM$rec_score))

bins_freq<-cut(y2,5, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"), dig.lab=10)
cut(y2,5, include.lowest = TRUE, dig.lab=10)
df_RFM$freq_score<-bins_freq
df_RFM$freq_score<-as.numeric(levels(df_RFM$freq_score))[df_RFM$freq_score]
barplot(table(df_RFM$freq_score))

bins_mon<-cut(y3,5, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"), dig.lab=10)
cut(y3,5, include.lowest = TRUE, dig.lab=10)
df_RFM$mon_score<-bins_mon
cut(y3,5, include.lowest = TRUE)
df_RFM$mon_score<-as.numeric(levels(df_RFM$mon_score))[df_RFM$mon_score]
barplot(table(df_RFM$mon_score))

rfm_scores<-df_RFM[,c('rec_score','freq_score',"mon_score")]

clf<-kmeans(rfm_scores,6)
fviz_cluster(clf, data = rfm_scores,geom='point',ellipse = TRUE, ellipse.type = "convex") + theme_minimal() +geom_point()
df_RFM$class<-clf$cluster
mean_rec<-as.numeric(format(round(mean(df_RFM$recency), 2), nsmall = 2))
mean_freq<-as.numeric(format(round(mean(df_RFM$frequency), 2), nsmall = 2))
mean_mon<-as.numeric(format(round(mean(df_RFM$monetary), 2), nsmall = 2))
print(paste(mean_rec,mean_freq,mean_mon))


