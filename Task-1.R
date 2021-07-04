#Install the 'Metrics' package if you do not have that installed

#Loading the required libraries
library(Metrics)

#Reading the dataset
s_data<-read.csv('http://bit.ly/w-data',header=T)

#Dimensions of the dataset
dim(s_data)

#A glimpse of the dataset
head(s_data,n=5)

#Column names of the dataset
names(s_data)

#Plotting the dataset to get a clear view of the dataset
par(bg='#CCFFFF')
plot(s_data$Hours,s_data$Scores,pch=16,col='dark blue',
	xlab='Hours studied',ylab='Precentage Score',
	main='Hours vs. Percentage')
legend(x=1,y=95,'Scores',pch=16,col='dark blue')

#Seperate the train and test data
X<-s_data$Hours;Y<-s_data$Scores
t_sample<-sample(nrow(s_data),floor(0.8*nrow(s_data)),replace=F)
X_train<-X[t_sample];Y_train<-Y[t_sample]
X_test<-X[-t_sample];Y_test<-Y[-t_sample]

#Fit a linear model
model<-lm(Y_train~X_train)
par(bg='#CCFFCC')
plot(s_data$Hours,s_data$Scores,pch=16,col='dark blue',
	xlab='Hours studied',ylab='Precentage Score',
	main='Hours vs. Percentage')
abline(model,col='red',lwd=2)

#Predictions
X_test#Testing data
Y_predicted<-predict(model,newdata=data.frame(X_train=X_test))#Predicting the scores
df<-data.frame('Hours'=X_test,'Actual score'=Y_test,'Predicted score'=Y_predicted)
df
pred<-predict(model,newdata=data.frame(X_train=9.25))
pr_data<-data.frame('Hours'=9.25,'Predicted Score'=pred)
pr_data

#Accuracy
mae(df$Actual,df$Predicted)