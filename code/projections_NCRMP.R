##Projections for all regions##
#22/06/2023
#Alice Webb
library(reshape2)
library(plyr)
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(wesanderson)
library(seacarb)
#import latest carbonate budget data for each regions 
CR_2022 <- read.csv("data/CHEECA_2022.csv")
CR_2022<-ddply(CR_2022,.(Taxon, Transect,Code,kg_m2_yr,Rugosity,Planar,Urchin, Micro,PF,genus,morphology,class), summarize,IniCover=sum(value, na.rm=T))
CR_2022$Site<-c('Cheeca_Rocks')
CR_2022$Year<-c('2022')

#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 78, nrow = 71))
colnames(years)<-c(2023:2100)

CR2022<-cbind(CR_2022,years)
mCR<-melt(CR2022, id.vars=c(1:14))
mCR<-mCR[c(1:15)]
names(mCR)[names(mCR) == 'variable'] <- 'Time'

mCR$Time<-as.character(mCR$Time)
mCR$Time[mCR$Time=="Year"]<-"2022"
omCR<- mCR[order(mCR$Taxon,mCR$Transect),]

#------------------
DRTO_2021 <- read.csv("data/DRTO_2021.csv")

DRTO_2021<-ddply(DRTO_2021,.(Taxon, Transect,Code,kg_m2_yr,Planar,Rugosity,Urchin, Micro,PF,genus,morphology,class), summarize,IniCover=sum(value, na.rm=T))
DRTO_2021$Site<-c('Dry_tortugas')
DRTO_2021$Year<-c('2021')

#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 79, nrow = 86))
colnames(years)<-c(2022:2100)

DRTO2021<-cbind(DRTO_2021,years)
mDRTO<-melt(DRTO2021, id.vars=c(1:14))
mDRTO<-mDRTO[c(1:15)]
names(mDRTO)[names(mDRTO) == 'variable'] <- 'Time'

mDRTO$Time<-as.character(mDRTO$Time)
mDRTO$Time[mDRTO$Time=="Year"]<-"2021"
omDRTO<- mDRTO[order(mDRTO$Transect,mDRTO$Taxon),]

#------------------
FGB_2022 <- read.csv("data/FGB_2022.csv")

FGB_2022<-ddply(FGB_2022,.(Taxon, Transect,Code,kg_m2_yr,Planar,Rugosity,Urchin, Micro,PF,genus,morphology,class), summarize,IniCover=sum(value, na.rm=T))
FGB_2022$Site<-c('Flower_Banks')
FGB_2022$Year<-c('2022')

#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 78, nrow = 72))
colnames(years)<-c(2023:2100)

FGB2022<-cbind(FGB_2022,years)
mFGB<-melt(FGB2022, id.vars=c(1:14))
mFGB<-mFGB[c(1:15)]
names(mFGB)[names(mFGB) == 'variable'] <- 'Time'

mFGB$Time<-as.character(mFGB$Time)
mFGB$Time[mFGB$Time=="Year"]<-"2022"
omFGB<- mFGB[order(mFGB$Transect,mFGB$Taxon),]


#------------------
PR_2023 <- read.csv("data/PR_2023.csv")

PR_2023<-ddply(PR_2023,.(Taxon, Transect,Code,kg_m2_yr,Planar,Rugosity,Urchin, Micro,PF,genus,morphology,class), summarize,IniCover=sum(value, na.rm=T))
PR_2023$Site<-c('Puerto_Rico')
PR_2023$Year<-c('2023')

#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 80, nrow = 51))
colnames(years)<-c(2024:2100)

PR2023<-cbind(PR_2023,years)
mPR<-melt(PR2023, id.vars=c(1:14))
mPR<-mPR[c(1:15)]
names(mPR)[names(mPR) == 'variable'] <- 'Time'

mPR$Time<-as.character(mPR$Time)
mPR$Time[mPR$Time=="Year"]<-"2023"
omPR<- mPR[order(mPR$Transect,mPR$Taxon),]

#------------------
STT_2023<- read.csv("data/STT_2023.csv")

STT_2023<-ddply(STT_2023,.(Taxon, Transect,Code,kg_m2_yr,Planar,Rugosity,Urchin, Micro,PF,genus,morphology,class), summarize,IniCover=sum(value, na.rm=T))
STT_2023$Site<-c('St_Thomas')
STT_2023$Year<-c('2023')

#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 83, nrow = 103))
colnames(years)<-c(2024:2100)

STT2017<-cbind(STT_2017,years)
mSTT<-melt(STT2017, id.vars=c(1:14))
mSTT<-mSTT[c(1:15)]
names(mSTT)[names(mSTT) == 'variable'] <- 'Time'

mSTT$Time<-as.character(mSTT$Time)
mSTT$Time[mSTT$Time=="Year"]<-"2017"
omSTT<- mSTT[order(mSTT$Transect,mSTT$Taxon),]
#------------------
STX_2022 <- read.csv("data/STX_2022.csv")
STX_2022<-ddply(STX_2022,.(Taxon, Transect,Code,kg_m2_yr,Planar,Rugosity,Urchin, Micro,PF,genus,morphology,class), summarize,IniCover=sum(value, na.rm=T))
STX_2022$Site<-c('St_Croix')
STX_2022$Year<-c('2022')

#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 78, nrow =88))
colnames(years)<-c(2023:2100)

STX2022<-cbind(STX_2022,years)
mSTX<-melt(STX2022, id.vars=c(1:14))
mSTX<-mSTX[c(1:15)]
names(mSTX)[names(mSTX) == 'variable'] <- 'Time'

mSTX$Time<-as.character(mSTX$Time)
mSTX$Time[mSTX$Time=="Year"]<-"2022"
omSTX<- mSTX[order(mSTX$Transect,mSTX$Taxon),]

#bind data together
CB_data<-rbind(omCR,omDRTO,omFGB,omPR,omSTT,omSTX)
#Remove lines where value of cover is 0
CB_data<-CB_data[CB_data$IniCover!=0,]

#get climate projections for both sites
all_carbonate <- read.csv("data/all_carbonate.csv")
names(all_carbonate)[names(all_carbonate) == 'Year'] <- 'Time'


#merge climate and budget data
FF<-merge(all_carbonate,CB_data,by=c('Time','Site'))
ordy <- FF[order(FF$Scenario,FF$Site,FF$Taxon,FF$Transect),]

#Check SST projections 
# ggplot(ordy,aes(Time,SST))+
#   geom_point()+
#   facet_grid(Scenario~Site)

##loop to add OA and Temp effect to calcification/bioerosion rates  (rates from Webb et al., 2023)
for (i in seq(from=1,to=nrow(ordy),by=1)) { 
  if ((ordy$Time[i] <= 2023) && (ordy$class[i] == "HC")) {    #rates pre_2023 stays as is
    ordy$rates[i]<- ordy$kg_m2_yr[i] 
    
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC")&& (ordy$genus[i] == "Orbicella")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(14.5 -0.522*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Siderastrea siderea")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(26.4 -0.922*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Siderastrea radians")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(26.4 -0.922*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Porites astreoides")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(7.38 -0.24*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.0236)
    
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Porites porites")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(2.98 -0.0934*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.0329)
  } 
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Colpophyllia natans")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(57.2 -1.99*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Diploria labyrinthiformis")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(57.2 -1.99*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC") && (ordy$Taxon[i] == "Montastraea cavernosa")) {
    ordy$rates[i]= ordy$rates[i-1] 
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC")&& (ordy$genus[i] == "Agaricia")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(14.5 -0.522*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
    
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC")&& (ordy$Taxon[i] == "Mycetophyllia")) {
    ordy$rates[i]= ordy$rates[i-1] + (ordy$SST[i]-ordy$SST[i-1])*(14.5 -0.522*ordy$SST[i-1])+((ordy$OmAr[i]-ordy$OmAr[i-1])*0.15*(ordy$rates[i-1]))
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC")&& (ordy$Taxon[i] == "Madracis decactis")) {
    ordy$rates[i]= ordy$rates[i-1]
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i] == "HC")&& (ordy$Taxon[i] == "Millepora complanata")) {
    ordy$rates[i]= ordy$kg_m2_yr[i]
  }
  else{
    ordy$rates[i]<- ordy$kg_m2_yr[i] 
    
  }}

##add OA and Temp effect to CCA calcification rates
for (i in seq(from=1,to=nrow(ordy),by=1)) { ##keep data as it is for 2012 to 2019
  if ((ordy$Time[i] <= 2023) && (ordy$Code[i]== "CCA"))  {    
    ordy$rates[i]<- ordy$kg_m2_yr[i] 
  }
  else if ((ordy$Time[i]>2023) && (ordy$class[i]== "CCA")) {
    ordy$rates[i]<-ordy$rates[i-1]+(ordy$SST[i]-ordy$SST[i-1])*(2.12-0.0792*ordy$SST[i-1])+(ordy$OmAr[i]-ordy$OmAr[i-1])*(1.7- 0.506*ordy$OmAr[i-1])
  }}
for (i in seq(from=1,to=nrow(ordy),by=1)) { ##keep data as it is for 2012 to 2019
  if ((ordy$Time[i] <= 2023) && (ordy$Code[i]== "MCCA"))  {    
    ordy$rates[i]<- ordy$kg_m2_yr[i] 
  }
  else if ((ordy$Time[i]>2019) && (ordy$Code[i]== "MCCA")) {
    ordy$rates[i]<-ordy$rates[i-1]+(ordy$SST[i]-ordy$SST[i-1])*(2.12-0.0792*ordy$SST[i-1])+(ordy$OmAr[i]-ordy$OmAr[i-1])*(1.7- 0.506*ordy$OmAr[i-1])
  }}


#OA effect on sand dissolution rates
for (i in seq(from=1,to=nrow(ordy),by=1)) { ##keep data as it is for 2012 to 2019
  if (ordy$class[i]== "S")  {    
    ordy$rates[i]<- -1.09+0.376*ordy$OmAr[i] 
  }}


#Calculate carbonate budgets

ordy$SubCover<- ordy$Rugosity*ordy$Planar
ordy$PercCoverI<-ordy$IniCover/ordy$SubCover
ordy$Planar_G_R<- ordy$rates*(ordy$PercCoverI/100)
ordy$G_R<- ordy$Planar_G_R*ordy$Rugosity

dff<-ordy

#-----------Sponge bioerosion ----------
SP_2022 <- read.csv("data/SP_2022.csv")


#add empty cells for years 2023 to 2100
years<-data.frame(matrix(ncol = 78, nrow = 21))
colnames(years)<-c(2023:2100)

SP2022<-cbind(SP_2022,years)
m2022<-melt(SP2022, id.vars=c(1:6,8:12))
m2022<-m2022[c(1:12)]
names(m2022)[names(m2022) == 'variable'] <- 'Time'

m2022$Time<-as.character(m2022$Time)
m2022$Time[m2022$Time=="year"]<-"2022"
om2022<- m2022[order(m2022$Transect,m2022$Taxon),]

SP_2021 <- read.csv("data/SP_2021.csv")

#add empty cells for years 2022 to 2100
years<-data.frame(matrix(ncol = 79, nrow = 14))
colnames(years)<-c(2022:2100)

SP2021<-cbind(SP_2021,years)
m2021<-melt(SP2021, id.vars=c(1:6,8:12))
m2021<-m2021[c(1:12)]
names(m2021)[names(m2021) == 'variable'] <- 'Time'

m2021$Time<-as.character(m2021$Time)
m2021$Time[m2021$Time=="year"]<-"2021"
om2021<- m2021[order(m2021$Transect,m2021$Taxon),]

SP_2017 <- read.csv("data/SP_2017.csv")

#add empty cells for years 2022 to 2100
years<-data.frame(matrix(ncol = 83, nrow = 3))
colnames(years)<-c(2018:2100)

SP2017<-cbind(SP_2017,years)
m2017<-melt(SP2017, id.vars=c(1:6,8:12))
m2017<-m2017[c(1:12)]
names(m2017)[names(m2017) == 'variable'] <- 'Time'

m2017$Time<-as.character(m2017$Time)
m2017$Time[m2017$Time=="year"]<-"2017"
om2017<- m2017[order(m2017$Transect,m2017$Taxon),]

SP_2023 <- read.csv("data/SP_2023.csv")

#add empty cells for years 2022 to 2100
years<-data.frame(matrix(ncol = 77, nrow = 5))
colnames(years)<-c(2024:2100)

SP2023<-cbind(SP_2023,years)
m2023<-melt(SP2023, id.vars=c(1:6,8:12))
m2023<-m2023[c(1:12)]
names(m2023)[names(m2023) == 'variable'] <- 'Time'

m2023$Time<-as.character(m2023$Time)
m2023$Time[m2023$Time=="year"]<-"2023"
om2023<- m2023[order(m2023$Transect,m2023$Taxon),]

#bind all sponge data
sponge<-rbind(om2017,om2021,om2022,om2023)

#merge climate and budget data
SP<-merge(all_carbonate,sponge,by=c('Time','Site'))
SP <- SP[order(SP$Scenario,SP$Site,SP$Taxon,SP$Transect),]


##loop 5 to add OA effect to bioeroding sponges
for (i in seq(from=1,to=nrow(SP),by=1)) { ##keep data as it is for 2012 to 2019
  if ((SP$Time[i] <= 2023))  {    
    SP$rates[i]<- SP$kg_m2_yr[i] 
  }
  else if (SP$Time[i]>2023) {
    SP$rates[i]<-SP$rates[i-1] +(SP$OmAr[i]-SP$OmAr[i-1])*0.699#(0.731+0.318*ordy$OmAr[i-1])
  }}
SP$erosion<-((SP$valuecm2/10000)*SP$rates/SP$Planar)
dSP<-ddply(SP,.(Time,Transect,Scenario,Site),summarize,Macro=sum(erosion,na.rm=T))

#Check by plot
ggplot(dSP,aes(Time,Macro))+
  geom_point()+
  facet_grid(Scenario~Site)

# #Check by plot
# ggplot(SP,aes(Time,rates))+
#  geom_point()+
#  facet_grid(Scenario~Site)

mff<-merge(dff,dSP,by=c("Time","Transect","Site","Scenario"),all=TRUE)
mff <- mff[order(mff$Scenario,mff$Site,mff$Taxon,mff$Transect),]
mff<- mff[!duplicated(mff), ]

mff$Macro[is.na(mff$Macro)]<-0


#Total bioerosion TB every year 
# TBS<-subset(dff,class=="BBS")
# TBS<-ddply(TBS,.(Time,variable),summarize,TB=sum(NCB,na.rm=T)-0.08-0.69)##for urchins and parrots
# 
# mff<-merge(dff,TBS,by=c("Time","variable"))
# dff <- mff[order(mff$taxon,mff$variable),]
#----------------------------------------------------

#------ increase sedimentation every year by a percentage of bioerosion (to do later)------
# for (i in seq(from=1,to=nrow(dff),by=1)) { ##keep data as it is until 2023
#   if ((dff$Time[i] <= 2023) && (dff$class[i]=="S"))  {    
#     dff$PercCoverI[i]<- dff$PercCoverI[i]
#     
#   }
#   else if ((dff$Time[i]>2023) && (dff$class[i]== "S")) {
#     dff$PercCoverI[i]<-dff$PercCoverI[i-1] -(dff$TB[i]*10*0.01)
#   }}
#------------------------------------------------------------------------------------------
dff<-mff
#Dissolution for sand
for (i in seq(from=1,to=nrow(dff),by=1)) { 
  if ((dff$class[i]=="S"))  {    
    dff$G_R[i]<- dff$rates[i]*( dff$PercCoverI[i]/100)
  }}


#calculate available substrate = 100 - %cover of sand
AS<-subset(dff,class=="S")
#Check rates
ggplot(AS,aes(Time,IniCover,colour=Transect))+
  geom_point(size=1.2)+
  facet_grid(Scenario~Site)

AS$PercAS<-100-AS$PercCoverI
AS<-AS[c(1,2,3,4,28)]

Aff<-merge(dff,AS,by=c("Site","Scenario","Time","Transect"),all=TRUE)
Aff$PercAS[is.na(Aff$PercAS)]<-100
dff <- Aff[order(Aff$Scenario,Aff$Site,Aff$Taxon,Aff$Transect),]


#calculate AAI available index
dff$AAI<-(dff$Rugosity)*(dff$PercAS/100)
dff$MicroE<--0.240*dff$AAI

#add OA effect to micro erosion
for (i in seq(from=1,to=nrow(dff),by=1)) { 
  if (dff$Time[i] <= 2023)  {    
    dff$MicroE[i]<-dff$MicroE[i]
  }
  else if (dff$Time[i]>2019){
    dff$MicroE[i]<- dff$MicroE[i-1] +(dff$OmAr[i]-dff$OmAr[i-1])*0.566##slope measured previously for microerosion
  }}

#Total bioerosion
dff$TB<--dff$Urchin-dff$PF+dff$MicroE-dff$Macro
TBS<-ddply(dff,.(Time,Transect,Site,Scenario),summarize,TB=mean(TB,na.rm=T))

#check
ggplot(TBS,aes(Time,TB,colour=Transect))+
  geom_point(size=1.2)+
  facet_grid(Scenario~Site,scales='free')
##make sure all non carbonate budget components have netProd=0

dff$G_R[dff$taxon=="Dead coral"]<-0
dff$G_R[dff$taxon=="Cyanobacteria"]<-0
dff$G_R[dff$taxon=="Halimeda"]<-0
dff$G_R[dff$taxon=="Macroalgae"]<-0
dff$G_R[dff$taxon=="Rubble"]<-0
dff$G_R[dff$taxon=="Soft coral"]<-0
dff$G_R[dff$taxon=="Sponge"]<-0
dff$G_R[dff$taxon=="Turf algae"]<-0

##measure total G
dff<- dff[!duplicated(dff), ]
G<-ddply(dff,.(Time,Transect,Scenario,Site,MicroE,PF,Urchin),summarize,NC=sum(G_R,na.rm=T))
#G$G<-G$NC+G$MicroE-G$Urchin-G$PF

G$Transect<-as.character(G$Transect)
ggplot(G,aes(Time,NC,colour=Transect))+
  geom_line()+
  facet_grid(Scenario~Site)+
  theme_bw()


G1<-merge(G,TBS,by=c("Site","Scenario","Time","Transect"), all=T)
G1$TG<-G1$NC+G1$TB
#create means for each site
mean_G<-ddply(G1,.(Time,Scenario,Site),summarize,mean_G=mean(TG,na.rm=T),sd_G=sd(TG,na.rm=T))

mean_G$Site <- factor(mean_G$Site, levels = c("Flower_Banks","Dry_tortugas","Cheeca_Rocks","Puerto_Rico","St_Croix","St_Thomas"),
                      labels = c("Flower G Banks","Dry Tortugas","Cheeca Rocks","La Pargera","St Croix","St Thomas"))

G1$Site <- factor(G1$Site, levels = c("Flower_Banks","Dry_tortugas","Cheeca_Rocks","Puerto_Rico","St_Croix","St_Thomas"),
                  labels = c("Flower G Banks","Dry Tortugas","Cheeca Rocks","La Pargera","St Croix","St Thomas"))
ggplot(mean_G,aes(Time,mean_G,colour=Scenario))+
  geom_line()+
  facet_wrap(~Site,ncol=3)+
  #geom_ribbon(aes(ymin=mean_G-sd_G, ymax=mean_G+sd_G,fill=Scenario),data=mean_G,alpha=0.1)+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))+
  xlab("Year")+
  ylab(expression(paste( " G [ kg m"^"-2"," yr"^"-1","]")))+ 
  scale_x_continuous(breaks=seq(2020, 2100, 20))

ggsave("figures/meanG_nobleaching.pdf",width=7,height=3)

G1$Transect<-as.character(G1$Transect)

ggplot(G1,aes(Time,TG,colour=Transect))+
  geom_line()+
  facet_grid(Scenario~Site)+
  theme_bw()+
  xlab("Year")+
  ylab(expression(paste( " G [ kg m"^"-2"," yr"^"-1","]")))+ 
  scale_x_continuous(breaks=seq(2030, 2100, 30))

ggsave("figures/ALLG_nobleaching.pdf",width=8,height=3)


ggplot(G1,aes(Time,TG,spilt=Transect,colour=Scenario))+
  geom_line()+
  facet_grid(~Site)+
  theme_bw()+
  xlab("Year")+
  ylab(expression(paste( " G [ kg m"^"-2"," yr"^"-1","]")))+
  scale_color_manual(values=c('blue','red'))



# geom_ribbon(aes(ymin=mean2-sd, ymax=mean2+sd),data=mean2,alpha=0.1,fill="red")