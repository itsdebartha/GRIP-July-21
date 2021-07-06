#Reading the dataset
df<-read.csv('D:\\Important Documents\\Internship\\Iris.csv',header=T)
dim(df)#dimensions of the dataset
names(df)#column names of the dataset
head(df)#a brief preview of the dataset
summary(df)#a brief summary of the dataset

#Finding the optimum value of clusters
twss<-sapply(1:10,function(k){kmeans(df[,-c(1,6)],k,iter.max=300,nstart=20)$tot.withinss})
plot(c(1:10),twss,type='b',lwd=2,col='dark blue',pch=16,
	ylab='Total within Sum of Squares',
	xlab='No. of clusters',
	main='The Elbow Method')

#Applying the kmeans to our dataset
KMeans<-kmeans(df[,-c(1,6)],3,iter.max=300,nstart=20)
cluster<-fitted(KMeans,'classes')
centroids<-as.data.frame(unique(fitted(KMeans,'centers')))

#1st scatterplot
par(bg='light blue')
plot(df[cluster==1,]$SepalLengthCm,
	df[cluster==1,]$SepalWidthCm,
	xlim=c(min(df$SepalLengthCm),max(df$SepalLengthCm)),
	ylim=c(min(df$SepalWidthCm),max(df$SepalWidthCm)),
	col='red',pch=16,main='Sepal Width vs. Sepal Length',
	xlab='Sepal Length',ylab='Sepal Width')
par(new=T)
plot(df[cluster==2,]$SepalLengthCm,
	df[cluster==2,]$SepalWidthCm,
	xlim=c(min(df$SepalLengthCm),max(df$SepalLengthCm)),
	ylim=c(min(df$SepalWidthCm),max(df$SepalWidthCm)),
	col='blue',pch=16,xaxt='n',yaxt='n',xlab=NA,ylab=NA)
par(new=T)
plot(df[cluster==3,]$SepalLengthCm,
	df[cluster==3,]$SepalWidthCm,
	xlim=c(min(df$SepalLengthCm),max(df$SepalLengthCm)),
	ylim=c(min(df$SepalWidthCm),max(df$SepalWidthCm)),
	col='dark green',pch=16,xaxt='n',yaxt='n',xlab=NA,ylab=NA)
par(new=T)
plot(centroids$SepalLengthCm,centroids$SepalWidthCm,
	xlim=c(min(df$SepalLengthCm),max(df$SepalLengthCm)),
	ylim=c(min(df$SepalWidthCm),max(df$SepalWidthCm)),
	col='yellow',pch=16,cex=2,xaxt='n',yaxt='n',xlab=NA,ylab=NA)
legend(x=7.1,y=4.4,c('Cluster 1','Cluster 2','Cluster 3','Centroids'),
		col=c('red','blue','dark green','yellow'),pch=16)

#2nd scatterplot
par(bg='light green')
plot(df[cluster==1,]$PetalLengthCm,
	df[cluster==1,]$PetalWidthCm,
	xlim=c(min(df$PetalLengthCm),max(df$PetalLengthCm)),
	ylim=c(min(df$PetalWidthCm),max(df$PetalWidthCm)),
	col='red',pch=16,main='Petal Width vs. Petal Length',
	xlab='Petal Length',ylab='Petal Width')
par(new=T)
plot(df[cluster==2,]$PetalLengthCm,
	df[cluster==2,]$PetalWidthCm,
	xlim=c(min(df$PetalLengthCm),max(df$PetalLengthCm)),
	ylim=c(min(df$PetalWidthCm),max(df$PetalWidthCm)),
	col='blue',pch=16,xaxt='n',yaxt='n',xlab=NA,ylab=NA)
par(new=T)
plot(df[cluster==3,]$PetalLengthCm,
	df[cluster==3,]$PetalWidthCm,
	xlim=c(min(df$PetalLengthCm),max(df$PetalLengthCm)),
	ylim=c(min(df$PetalWidthCm),max(df$PetalWidthCm)),
	col='dark green',pch=16,xaxt='n',yaxt='n',xlab=NA,ylab=NA)
par(new=T)
plot(centroids$PetalLengthCm,centroids$PetalWidthCm,
	xlim=c(min(df$PetalLengthCm),max(df$PetalLengthCm)),
	ylim=c(min(df$PetalWidthCm),max(df$PetalWidthCm)),
	col='yellow',cex=2,pch=16,xaxt='n',yaxt='n',xlab=NA,ylab=NA)
legend(x=1,y=2.5,c('Cluster 1','Cluster 2','Cluster 3','Centroids'),
		col=c('red','blue','dark green','yellow'),pch=16)

#Summary Table
table(KMeans$cluster,df$Species)