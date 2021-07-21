#Importing libraries and visualising the data
#Fortunately, we do not need to load any other library for this work
df<-read.csv('D:\\Important Documents\\Internship\\Task-4\\globalterrorismdb_0718dist.csv',
			header=T)#reading the dataset
dim(df)#dimensions of the dataset
length(is.na(df))#number of 'na' values
names(df)


#Exploratory Data Analysis

#Attacks by Year:
attack_year<-table(df$iyear)
plot(attack_year,type='b',main='Terrorism by Year',
	xlab='Year',ylab='No. of Incidents',pch=19)

#Type of Attack by Year:
type_year<-cbind(unique(df$iyear),table(df$iyear,df$attacktype1_txt))
type_year<-type_year[order(type_year[,1]),]
par(mfrow=c(3,3))
for(i in 2:ncol(type_year))
{
	plot(x=type_year[,1],y=type_year[,i],main=colnames(type_year)[i],
		xlab='Year',ylab='Incidents',type='l',cex.main=0.95,
		col=hcl.colors(9,'dark 2')[i-1])
}

#Target type:
par(mar=c(10,4,4,1)+.1)
targ_type<-table(df$targtype1_txt)
max_targ_type<-tail(sort(targ_type),5)
barplot(max_targ_type,col=hcl.colors(5,'warm'),las=2,
		cex.names=0.8,main='Top 5 Targets')

#Attacks by Region over the years:
reg_year<-table(df$region_txt,df$iyear)
plot(x=colnames(reg_year),y=reg_year[1,],type='l',lwd=1.5,
	ylim=c(min(reg_year),max(reg_year)),xlim=c(1970,2017),
	col=hcl.colors(nrow(reg_year),'dark 2')[1],
	xlab='Year',ylab='No. of Incidents',
	main='Incidents by Region')
for(i in 2:nrow(reg_year))
{
	par(new=T)
	plot(x=colnames(reg_year),y=reg_year[i,],type='l',lwd=1.5,
		ylim=c(min(reg_year),max(reg_year)),xlim=c(1970,2017),
		xaxt='n',yaxt='n',xlab=NA,ylab=NA,
		col=hcl.colors(nrow(reg_year),'dark 2')[i])
}
legend(x=1970,y=7000,rownames(reg_year),lwd=1.5,
		col=hcl.colors(nrow(reg_year),'dark 2'),
		cex=0.5,title='Region')

#Affected Country:
country<-table(df$country_txt)
max_country<-tail(sort(country),5)
barplot(max_country,col=hcl.colors(5,'warm'),
		las=2,cex.names=0.8,main='Top 5 Countries by Attacks')

#Weapons Used:
weapon<-table(df$weaptype1_txt)
barplot(weapon,xaxt='n',col=hcl.colors(length(unique(df$weaptype1_txt)),'dynamic'),
		main='Weapon Type')
legend('topright',rownames(weapon),
		fill=hcl.colors(length(unique(df$weaptype1_txt)),'dynamic'),
		cex=0.5,title='Weapon Type')

#Casualties by Year:
df[is.na(df$nkill),]$nkill<-0
year_kill<-tapply(df$nkill,df$iyear,sum)
year_kill<-as.table(year_kill)
barplot(year_kill,las=2,cex.names=0.7,main='No. of kills',
		xlab='Year',ylab='Kills',col='#a1383a')

#Correlation:
attack_kill<-cbind(year_kill,attack_year)
atk_cor<-cor(attack_kill)
atk_cor
atk_cor[lower.tri(atk_cor,diag=T)]<-0
max(atk_cor)

#Database Source:
source<-as.data.frame(table(df$dbsource))
colnames(source)<-c('Source','Freq')
source
