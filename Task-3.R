#Importing the necessary libraries
library(RColorBrewer)

#Reading the dataset
df<-read.csv('D:\\Important Documents\\Internship\\Task-3\\SampleSuperstore.csv',header=T)
dim(df)#the dimensions of the dataset
head(df)#a brief preview of the dataset
names(df)#colimn names of the dataset
summary(df)#a brief summary of the dataset
df<-df[!duplicated(df),]#removing duplicate entries

#Correlation Matrix
cor_mat<-round(cor(df[,sapply(df,class)!='character']),3)
cor_mat#The correlation matrix
cor_mat[lower.tri(cor_mat,diag=T)]<-0
max(abs(cor_mat))

#EDA
cat_div<-table(df$Category)
pie(cat_div,col=c('#FF6666','#0099CC','#5eb58a'),main='Division by Category')

seg_div<-table(df$Segment)
pie(seg_div,col=c('#549dc7','#cc689a','#c9b36d'),main='Division by Segment')

state_div<-table(df$State)
barplot(state_div,las=2,cex.names=0.5,
		col=hcl.colors(n=length(unique(df$State)),"dynamic"),
		ylab='Counts',main='Division by States')

cat_Subcat<-table(df$Category,df$Sub.Category)
barplot(cat_Subcat,las=2,col=c('#FF6666','#0099CC','#5eb58a'),
		cex.names=0.75,main='Division by Subcategory',ylab='Count')
legend('topright',rownames(cat_Subcat),fill=c('#FF6666','#0099CC','#5eb58a'),cex=0.75)

par(mfcol=c(3,2))
for(i in 1:ncol(df))
{
	if (class(df[,i])!='character')
	{
		hist(df[,i],xlab=names(df)[i],main=paste('Histogram of ',
			names(df)[i]),col='#313b69')
	}
}

table(df$Ship.Mode,df$Quantity)

pairs(~df$Postal.Code+df$Sales+df$Quantity+df$Discount+df$Profit,
		labels=c('Postal Code','Sales','Quantity','Discount','Profit'),
		col=hcl.colors(length(unique(df$Sub.Category)),'dark 3'),
		pch=19,oma=c(3,5,3,12))
par(xpd=T)
legend("bottomright",fill=hcl.colors(length(unique(df$Sub.Category)),'dark 3'),
		legend=c(unique(df$Sub.Category)),cex=0.5,title='Sub-Category')

cat_city_sales<-ftable(with(df,tapply(Sales,list(Category,City),sum)))
cat_city_sales[which(is.na(cat_city_sales)==T)]<-0
data<-as.matrix(cat_city_sales)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Sales in City by Category')

cat_state_sales<-ftable(with(df,tapply(Sales,list(Category,State),sum)))
cat_state_sales[which(is.na(cat_state_sales)==T)]<-0
data<-as.matrix(cat_state_sales)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Sales in States by Category')

cat_city_pro<-ftable(with(df,tapply(Profit,list(Category,City),sum)))
cat_city_pro[which(is.na(cat_city_pro)==T)]<-0
data<-as.matrix(cat_city_pro)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Profit in City by Category')

cat_state_pro<-ftable(with(df,tapply(Profit,list(Category,State),sum)))
cat_state_pro[which(is.na(cat_state_pro)==T)]<-0
data<-as.matrix(cat_state_pro)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Profit in State by Category')

seg_city_pro<-ftable(with(df,tapply(Profit,list(Segment,City),sum)))
seg_city_pro[which(is.na(seg_city_pro)==T)]<-0
data<-as.matrix(seg_city_pro)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Profit in City by Segment')

seg_state_pro<-ftable(with(df,tapply(Profit,list(Segment,State),sum)))
seg_state_pro[which(is.na(seg_state_pro)==T)]<-0
data<-as.matrix(seg_state_pro)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Profit in State by Segment')

subcat_city_pro<-ftable(with(df,tapply(Profit,list(Sub.Category,City),sum)))
subcat_city_pro[which(is.na(subcat_city_pro)==T)]<-0
data<-as.matrix(subcat_city_pro)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Profit in City by Sub-Category')

subcat_state_pro<-ftable(with(df,tapply(Profit,list(Sub.Category,State),sum)))
subcat_state_pro[which(is.na(subcat_state_pro)==T)]<-0
data<-as.matrix(subcat_state_pro)
heatmap(t(data),Colv=NA,Rowv=NA,cexCol=0.8,
		col=colorRampPalette(brewer.pal(9,"Blues"))(25),
		main='Heatmap for Profit in State by Sub-Category')

Profit<-as.data.frame(tapply(df$Profit,df$Sub.Category,sum))
Sales<-as.data.frame(tapply(df$Sales,df$Sub.Category,sum))
barplot(t(as.matrix(cbind(Profit,Sales))),las=2,col=c('#eb9113','#1b68bf'),
		beside=T,main='Sales and Profit by Sub-Category',cex.names=0.75)
legend(x=0,y=320000,c('Sales','Profit'),fill=c('#eb9113','#1b68bf'))