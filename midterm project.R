library(tidyr)
library(plyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
library(scales)
library(maps)

any(is.na(oilgascounty))
oilgascounty<-na.omit(oilgascounty)
oil<-subset(oilgascounty,select = c(geoid:Metro_Micro_Noncore_2013,
oil2000:oil2011,oil_change_group:oil_gas_change_group))

gas<-subset(oilgascounty,select=c(County_Name, Stabr,Metro_Micro_Noncore_2013,
gas2000:gas2011))

others<-subset(oilgascounty,select=c(FIPS,County_Name:Metro_Nonmetro_2013,oil_change))
#clean data

oil<-melt(oil,id.vars =  c("geoid","Stabr","County_Name","Rural_Urban_Continuum_Code_2013","Urban_Influence_2013","Metro_Nonmetro_2013","Metro_Micro_Noncore_2013","oil_change_group","gas_change_group","oil_gas_change_group"),
          variable.name = "year",value.name="oil")
oil$year<-substr(oil$year,4,7)
gas<-melt(gas,id.vars = c("County_Name","Stabr","Metro_Micro_Noncore_2013"),variable.name = "year",value.name="gas")
gas$year<-substr(gas$year,4,7)

#oil<-join(x=oil,y=others,by="County_Name")

oil<-data.table(oil)
gas<-data.table(gas)

# oil_gas_group
oilgasgroup<-function(x){
  for(i in 1:length(x)){
    if(x[i]=="H_Growth"){
      x[i]<-">=20"
    }else if(x[i]=="H_Decline"){
      x[i]<-"<=-20"
    }else{
      x[i]<-"-20/+20"
    }
  }
  i=i+1
  return(x)
}
oil$oil_change_group<-as.character(oil$oil_change_group)
oil$oil_change_group<-oilgasgroup(oil$oil_change_group)

oil$gas_change_group<-as.character(oil$gas_change_group)
oil$gas_change_group<-oilgasgroup(oil$gas_change_group)

oil$oil_gas_change_group<-as.character(oil$oil_gas_change_group)
oil$oil_gas_change_group<-oilgasgroup(oil$oil_gas_change_group)

oil<-data.table(oil)
gas<-data.table(gas)

#whole tidy data set

oilgastidy<- join(oil,gas)
oilgastidy$gas<-gas$gas

#show this data set
#total oil in different state and different year

stabroil<-oil[,sum(oil),by=list(year,Metro_Micro_Noncore_2013)]
head(stabroil)
#total gas indifferent state and different year

stabrgas<-gas[,sum(gas),by=list(year,Metro_Micro_Noncore_2013)]
head(stabrgas)

#plot 

stabrgas<-data.table(stabrgas)
stabrgas$V1<-stabrgas$V1/10^6
ggplot(stabrgas,aes(x=year,y=V1))+
  geom_line(aes(color=factor(Metro_Micro_Noncore_2013),group=Metro_Micro_Noncore_2013),lwd=2)+
  ylab("gas")+geom_smooth()+ggtitle("Nature gas production in the 48 states,bymetropolitan status(cubic feet(billion))")

stabroil<-data.table(stabroil)
stabroil$V1<-stabroil$V1/10^6
ggplot(stabroil,aes(x=year,y=V1))+
  geom_line(aes(color=factor(Metro_Micro_Noncore_2013),group=Metro_Micro_Noncore_2013),lwd=2)+
  ylab("oil")+geom_smooth()+ggtitle("Nature oil production in the 48 states,bymetropolitan status(cubic feet(billion))")

#map
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
map("county",col=colors[oil$oil[match(mapnames,oil$County_Name)]],fill = TRUE,resolution = 0,lty = 0,projection = "polyconic")
map("state",col = "white",fill=FALSE,add=TRUE,lty=1,lwd=1,projection="polyconic")

mapcounties <- map_data("county")
mapnames<-mapcounties$subregion



#mean oil and gas in different state

stateoil<-oil[,mean(oil),by=list(Stabr)]
stategas<-gas[,mean(gas),by=list(Stabr)]

par(mfrow=c(1,2))
ggplot(stateoil,aes(x=Stabr,y=V1/10^3))+geom_bar(stat="identity",fill=heat.colors(49))+ylab("mean oil")+xlab("state")+ggtitle("mean oil in different state")
ggplot(stategas,aes(x=Stabr,y=V1/10^3))+geom_bar(stat="identity",fill=heat.colors(49))+ylab("mean gas")+xlab("state")+ggtitle("mean gas in different state")



