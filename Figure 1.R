#-------------------------------------------------
#Summary of public health responses efficiencies
#-------------------------------------------------
library(ggplot2) ; library(hrbrthemes) ; library(corrplot) ; library(reshape); library(lme4); library(dplyr);library(tidyr)
load('matResults.RData')
temp <- read.csv("inst/extdata/COVID_time_series_v4_2020-06-26.csv",stringsAsFactors = FALSE)
countryVec <- sort(unique(temp$Country))
gdp<-c(50222,45175,9518,14949,27719,23313,59795,23523,48868,41760,46563,
       19974,17463,67037,77771,32946,18171,173356,19266,113196,30650,
       52367,77975,14901,23030,12482,19547,26170,29961,51241,83716,41030);
dfMat<-data.frame(matResults)

# Get country names
ii=match(dfMat$GDP, gdp)
dfMat$country=countryVec[ii]

# Get duration of intervention
dfMat$duration=dfMat$DayEnd-dfMat$DayStart
hist(dfMat$duration)

# Prepare plot for number of countries and duration of intervention
pos=0
country.duration=dfMat[,1:24]*dfMat$duration
country.duration.db=array(NA, c(length(unique(dfMat$country)),24))
country.implementation.db=array(NA, c(length(unique(dfMat$country)),24))
for (i in unique(dfMat$country)){
  pos=pos+1
  # Number of days implemented
  my.db=country.duration[dfMat$country==i,]
  country.duration.db[pos,]=apply(my.db, 2, sum)
  # Implementation or not
  my.db2=dfMat[dfMat$country==i,]
  country.implementation.db[pos,]=apply(my.db2[,1:24], 2, function(x){ifelse(any(x)==1, 1, 0)})
}
country.duration.db=as.data.frame(country.duration.db) ; colnames(country.duration.db)=colnames(dfMat)[1:24] ; row.names(country.duration.db)=unique(dfMat$country)
country.implementation.db=as.data.frame(country.implementation.db) ; colnames(country.implementation.db)=colnames(dfMat)[1:24] ; row.names(country.implementation.db)=unique(dfMat$country)

# % countries implementing each NPI
country.implementation=apply(country.implementation.db,2,sum)/length(unique(dfMat$country))
country.implementation=data.frame(NPI=names(country.implementation), implementation=country.implementation, row.names = 1:24)
# Estimate boxplot of NPI duration
country.duration.stats=array(NA, c(24,5))
for (i in 1:24){
  pos=which(country.duration.db[,i]>0)
  country.duration.stats[i,]=boxplot(country.duration.db[pos,i], plot = F)$stats[,1]
}
country.duration.stats=as.data.frame(country.duration.stats) ;
colnames(country.duration.stats)=c('low.whisk','Q1','median','Q3', 'up.whisk')
country.duration.stats$NPI=colnames(dfMat)[1:24]

country.duration.stats_long <- gather(country.duration.stats, key='stat',value='value', low.whisk:up.whisk, factor_key=TRUE)
colnames(country.duration.stats_long)[1]='NPI'

#--------------------------
# Create plot - Version 1
#--------------------------
library(tidyr) ; library(ggplot2) ; library(grid) ; library(gridExtra)

implementation.plot=ggplot(data=country.implementation, aes(x=implementation*100, y=NPI)) +
  geom_bar(stat="identity")+
  labs(x='% Countries Implemented',y='')+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
implementation.plot

duration.plot= ggplot(subset(country.duration.stats_long,stat %in% c('Q1','median','Q3')),aes(x=value,y=NPI,colour=stat,group=NPI)) +
  geom_line(colour='black')+
  geom_point(size=4)+
  scale_colour_manual(values = c('darkgrey','black','darkgrey'))+
  labs(x='Duration (days)',y='')+
  theme(legend.position='right',axis.text=element_text(size=12),axis.text.y=element_blank(),
        axis.title=element_text(size=14,face="bold"))
duration.plot
grid.arrange(implementation.plot,duration.plot, ncol=2)

#--------------------------
# Create plot - Version 2
#--------------------------
library(tidyr) ; library(ggplot2) ; library(ggcorrplot); library(grid) ; library(gridExtra)
npi.group=c(5,5,5,5,5,5,5,5,5,5,4,4,4,4,3,3,3,2,2,2,2,2,1,1)
#axis.col=factor(npi.group, labels=c('black','grey40','black','grey40','black'))
axis.face=factor(npi.group, labels=c('bold','italic','bold','italic','bold'))
country.implementation$`Median duration (days)`=country.duration.stats$median
implementation.plot=ggplot(data=country.implementation, aes(x=implementation*100, y=NPI, fill=`Median duration (days)`)) +
  geom_bar(stat="identity")+
  #scale_color_continuous()+
  labs(x='% Countries Implemented',y='')+ theme_bw()+
  theme(plot.margin = margin(0.2, -1, 3, 0, "cm"),legend.position=c(0.8,0.79),legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),axis.text=element_text(size=12),axis.title=element_text(size=12),
        axis.text.y = element_text(face=as.character(axis.face)))
implementation.plot

ii=match(unique(country.implementation$NPI),colnames(dfMat)[1:24])
dfMatcor=dfMat[,ii] ;
tabRes=cor(dfMatcor);
corplot=ggcorrplot(tabRes,hc.order = F,
           type = "lower",
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           ggtheme=theme_bw,insig = "pch",
           legend.title = "Correlation")
corplot=corplot+theme(plot.margin = margin(0.9, 0, 0, 0, "cm"), legend.position=c(0.1,0.79),legend.background = element_blank(),
                      legend.box.background = element_rect(colour = "black"),axis.text.y=element_blank(),
                      axis.text.x = element_text(face=as.character(axis.face)[2:24]))
corplot
# Save into a high resolution image (doesn't work by saving directly; I need to zoom, then print and change in GIMP)
jpeg('Figure 1.jpeg', units="px",width = 1100, height = 450)
grid.arrange(implementation.plot,corplot, ncol=2)
dev.off()

#-----------------------------------
# Other information for the article
#-----------------------------------
length(which(dfMat$Efficiency>0.66))/length(dfMat$Efficiency)
