#Description: Script to merge dataframes
#Author: Heliana Teixeira
#Date: 15 February 2021
##############################
library(here)
library(tidyverse)

source(here("R","MyFunctV2.R")) # Functions used for plotting

#### Get data ####
#revised data
FName.dat <-here("Data","QE_GMbound_new.csv")
new <- read.csv(file = FName.dat, header = TRUE, sep = ";")

#2014 data
FName <- here("Data","QE_GMbound_old.csv")
old <- read.csv(file = FName, header = TRUE, sep = ";")

str(new)
names(new)
str(old)
names(old)

#### Edit data before merge ####
names(old)[names(old) == "Category"] <- "Cat"
names(old)[names(old) == "National.type.code"] <- "NatType"
names(old)[names(old) == "unit_used"] <- "UnitUsed"

old$Measurement.Type<- recode_factor(old$Measurement.Type, `5th percentile`="5th percentile",`10th percentile`="10th percentile",
                   `10th percentile\n`="10th percentile",`90th percentile`="90th percentile",
                   `90th Percentile`="90th percentile",`95th pecentile`="95th percentile", 
                   `95th percentile`="95th percentile",`99th percentile`="99th percentile",
                   `maximum`="maximum",`Maximum`="maximum",`mean`="AA-EQS",`Mean`="AA-EQS",
                   `mean `="AA-EQS",`mean+I1109:I1126`="AA-EQS",`median`="Median",`Median`="Median",
                   `Minimum`="minimum",`Regression`="Regression",
                   `Concentration standardizing value to the salinity 33`= "other")
levels(old$Measurement.Type)

names(old)[names(old) == "Measurement.Type"] <- "SumMetric"

#### enter text for standard and label for plotting #### 
dec <- 2 # number of decimals to show
Cat <- "CW" # select water category: TW, CW
# Category.u <- "CW" # select water category: TW, CW
physChemText.u <- "Nitrate as N" # select QE
ylb <- "Nitrate N (mg/L) GM boundary"
  # "% oxygen saturation" Oxygen saturation (%)
  # "Dissolved oxygen" DO (mg/L)
  # "Secchi disk depth"  Secchi depth (m)
  # "TN" Total N (mg/L)    
  # "Total Inorganic N" Total Inroganic N (mg/L)
  # "Orthophosphate" PO4-P (ug/L)
  # "TP"  Total P (ug/L)
# "Nitrate as N" Nitrate N (mg/L)

#Subset of data by selecting water category and Parameter code
#from revised data (new)
new.s <- new %>% 
  filter(Cat == Category.u, physChemText==physChemText.u,!is.na(ValueStd)) %>%
  droplevels()
dim(new.s)
#Issue with ranges
filter(new.s,!is.na(ValueStd),ValueUpStd<ValueStd)

#from 2014 WFD reported data (old)
old.s<-old%>%
  select(Cat,Country,physChemText,SumMetric,NatType,ValueStd.old,ValueUpStd.old,UnitUsed,GIG)%>%
  filter(Cat==Category.u, physChemText==physChemText.u,!is.na(ValueStd.old))%>%
           droplevels()

dim(old.s)
#Issue with ranges
filter(old.s,!is.na(ValueStd.old),ValueUpStd.old<ValueStd.old)

#CheckDataPlot
# Check data, modify units if plot suggests issues
df<-(new.s)
plot1.n<-BPChk(df) 

names(old.s)[names(old.s) == "ValueUpStd.old"] <- "ValueUpStd"
names(old.s)[names(old.s) == "ValueStd.old"] <- "ValueStd"

old.s<- old.s%>%             #edit values likely not ug/l instead of mg/L NO3-N
  mutate(ValueStd=ifelse(ValueStd>3,ValueStd/1000,ValueStd))%>%
  mutate(ValueUpStd=ifelse(ValueUpStd>3,ValueUpStd/1000,ValueUpStd)) 
old.s%>%filter(ValueStd>3) #check

df<-(old.s) 
plot1.o<- BPChk(df)
gridExtra::grid.arrange(plot1.n, plot1.o)

#combine old and new data
#return to old names
names(old.s)[names(old.s) == "ValueUpStd"] <- "ValueUpStd.old"
names(old.s)[names(old.s) == "ValueStd"] <- "ValueStd.old"

dat.Comb <- full_join(new.s,old.s) 

#### fig1 ####
count(new.s,SumMetric)
  #Set up the plot symbols to match those in data set, use 
  #8 for 95th PC, 4 90th PC, 6 max
  #19 AA-EQS, 17 median, 7 summer, 12 spring, 10 winter, 13 autumn, 14 growth
  #5 5th PC, 9 10th PC, 2 min
symb1 <- c(4,8,19,18,13,12,11,10,3)

#Create plot of standards v country including old and new data

# 1. Open png file
png(paste0(Cat,physChemText.u,".jpg"),units="in", width=11, height=8.5, res=300)

# 2. Create the plot
plotGMoldnew <- ggplot(data=dat.Comb, aes( x = Country, y = ValueStd.old, shape = SumMetric))+
  geom_point(colour = "green",position = position_nudge(x=-0.2 ,y = 0))+
  geom_point(data = subset(dat.Comb,!is.na(ValueUpStd)), mapping = 
               aes(x = Country, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.05 ,y = 0))+ 
  geom_point(data = dat.Comb, mapping = aes(x=Country, y=ValueUpStd,shape=SumMetric),color="red",position = position_nudge(x= 0.05 ,y = 0))+
  
  geom_point(data=subset(dat.Comb,is.na(ValueUpStd)),aes(x = Country, y = ValueStd, shape = SumMetric),position = position_nudge(x=+0.20 ,y = 0))+
  labs(y = ylb)+
  scale_shape_manual(values=symb1,name  ="")+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())

# 3. Close the file
dev.off()

#Check plot
plotQEoldnew  

#### captions ####
#labels for legends
LCap1 <- "Figure 4.1" #plot.NO3N CW
LCap2 <- "Figure 4.2" #plot.NO3N TW
LCap3 <- "Figure 4.3" #plot.TN CW
LCap4 <- "Figure 4.4" #plot.TN TW
LCap5 <- "Figure 4.5" #plot.TIN CW
LCap6 <- "Figure 4.6" #plot.TIN TW
LCap7 <- "Figure 4.7" #plot.TP CW
LCap8 <- "Figure 4.8" #plot.TP TW
LCap9 <- "Figure 4.9" #plot.PO4P CW
LCap10 <- "Figure 4.10" #plot.PO4P TW
LCap11 <- "Figure 4.11" #plot.Osat CW
LCap12 <- "Figure 4.12" #plot.Osat TW
LCap13 <- "Figure 4.13" #plot.DO CW
LCap14 <- "Figure 4.14" #plot.DO TW
LCap15 <- "Figure 4.15" #plot.Transp CW
LCap16 <- "Figure 4.16" #plot.Transp TW

#Fig legends for report
Cap1 <- paste(LCap1,"Comparison of coastal",physChemText.u,"standards by country and IC type. Green symbols represent values reported in 2014, others colours those reported in 2019 (single value black, minimum blue, maximum red symbols.)")
Cap2 <- paste(LCap2,"Comparison of Transitional",physChemText.u,"standards  by country and IC type. Green symbols represent values reported in 2014, others colours those reported in 2019 (single value black, minimum blue, maximum red symbols.)")

summary stats QE GM boundaries

#### summary stats QE GM boundaries ####
#edit table name
tableQE <- dat.Comb %>%
  select(ValueStd, ValueStd.old, ValueUpStd, ValueUpStd.old)%>%
  map_df(.f = ~ broom::tidy(summary(.x)), .id = physChemText.u)
  

#### Save the objects ####
#edit the next line to reflect directory used to store data file generated
OutFile <- here("Data",paste0(Cat,physChemText.u,".RData"))

save(dat.u.d,Metrics,Temp,Temp1,Temp2,OverV4,
     RangeCntry,ValCntry,TypeAllCntry,NinGroup,
     CntryInGroup,GrpGT2,symb,ylb,dec,physChemText.u,
     dat.u.d,dat.u,coP1,coP2,coP3,coP4,
     file=OutFile)

