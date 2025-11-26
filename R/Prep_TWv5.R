
# This script is a common chunk used to structure the data used for plotting, it is called from each
# R notebook used for different quality elements.  It also creates a number of summary objects
# used in markdown to document various characteristics of the extracted data and for the tables

# Geoff Phillips 11 August 2019
# V2: Updated 12 August, added TypeInGroup and StndInGroup, objects with number of types and 
#     distinct standards in group (Broad Type)
# V3: Included reason for range in dat.u.d
# V5: Included AggType in dat.u.d
# V5tw: adapted to Transitional waters data (by HT): names of var and IC common types & Marine regions

#---------------------------------------------------------------------------------------
#Prepare data by extracting distinct records
dat.u.d <- dat.u %>%
  filter(!is.na(ICcode.TRAC),SumMetric !="EQR") %>%   # remove types not mapped & EQRs
  select(All.obs,Country,physChemText,SumMetric,ValueStd,ValueUpStd,ICcode.TRAC,MarineRegion,GIG,NatType,RangeRep,ReasonRange,MetricType) %>% 
  distinct() %>% 
  droplevels()
#-------------------------------------------------------------------------------------------

#Which metrics are used by each country?
Metrics <- table(dat.u.d$Country,dat.u.d$SumMetric )
#Which is the metric type and the no. of unique standards?
Metrics.b <- dat.u.d%>%
  count(Country, MetricType, SumMetric,ValueStd, useNA='ifany')%>%
  count (Country, MetricType, SumMetric)
#------------------------------------------------------------------------------------------------

#Which national types occur in each IC type?
NinGroup <- group_by(dat.u.d,ICcode.TRAC) %>%  # national types in group
  count()
#------------------------------------------------------------------------------------------------

#Which national types occur in each GIG?
NinAggGroup <- group_by(dat.u.d,GIG) %>%  # national types in group
  count()
#------------------------------------------------------------------------------------------------

#Create list of countries contributing to each IC type
# number of types in each group (IC Type)
CntryInGroup <- dat.u %>%  
  filter(!is.na(ICcode.TRAC),SumMetric !="EQR") %>%   # remove types not mapped & EQRs
  select(Country,ICcode.TRAC) %>% 
  group_by(ICcode.TRAC,Country) %>% 
  count %>% 
  group_by(ICcode.TRAC) %>% 
  count() 
#-----------------------------------------------------------------------------------------------------

#Create list of countries contributing to each marine region
# number of types in each group (GIG)
CntryInAggGroup <- dat.u %>%  
  filter(!is.na(GIG),SumMetric !="EQR") %>%   # remove types not mapped & EQRs
  select(Country,GIG) %>% 
  group_by(GIG,Country) %>% 
  count %>% 
  group_by(GIG) %>% 
  count() 
#-----------------------------------------------------------------------------------------------------

#List of IC types with >2 countries
GrpGT2 <- CntryInGroup[CntryInGroup$n>2 & CntryInGroup$ICcode.TRAC !="inapplicable","ICcode.TRAC"] # Groups with >2 countries
#-------------------------------------------------------------------------------------------------------

#List of GIG with >2 countries
AggGrpGT2 <- CntryInAggGroup[CntryInAggGroup$n>2 & CntryInAggGroup$GIG !="All","GIG"] # AggGroups with >2 countries
#-------------------------------------------------------------------------------------------------------

# account for salinity grandient
dat.shrt <- dat.u %>%
  select(All.obs,GIG,MarineRegion,ICcode.TRAC,Country,
         NatType,physChemText,SumMetric,ValueStd,ValueUpStd,UnitUsed,
         RangeRep,ReasonRange,DepthZone,MetricType) %>%
  droplevels()

dim(dat.shrt)

#Create long format by putting both ValueStd and ValueUpStd into single var "Value"
dat.shrt.l <- pivot_longer(dat.shrt,cols = starts_with("Val"),names_to = "col",values_to = "Value")

#add Salinity to correspondent Value
salinities <- select(dat.u, c("All.obs","Salinity1","Salinity2"))
dat.shrt.l <- left_join(dat.shrt.l,salinities, by = "All.obs")
dat.shrt.l$Salinity2<-(as.numeric(dat.shrt.l$Salinity2))
dat.shrt.l <- dat.shrt.l %>%
  mutate(Salinity = case_when(
    col=="ValueStd" ~ Salinity1,
    col=="ValueUpStd" ~ Salinity2))
#--------------------------------------------------------------------------------------------------------

#Create an object to provide a table showing which countries use values and which ranges

#Extract records (i.e. RBDs) with standards presented as a range (ValueUp is not NA)
Temp1 <- dat.u %>%     # use dat.u.d for number of distinct standards
  filter(!is.na(ValueUpStd) & ReasonRange!="salinity range",!is.na(ICcode.TRAC)) %>%
  droplevels() %>% 
  group_by(Country) %>% 
  count() %>%
  droplevels() %>% 
  rename(range=n)

#Extract records (i.e. RBDs) with standards presented as a value (ValueUp is  NA)
Temp2 <- dat.u %>%     # use dat.u.d for number of distinct standards 
  filter(is.na(ValueUpStd) | ReasonRange=="salinity range",!is.na(ICcode.TRAC)) %>%
  group_by(Country) %>% 
  count()%>%
  droplevels() %>% 
  rename(value=n)

#Extract records with standards presented as a range
Temp3 <- dat.u %>%     # use dat.u.d for number of distinct standards
  filter(!is.na(ValueUpStd) & ReasonRange!="salinity range",!is.na(ICcode.TRAC)) %>%
  droplevels() %>% 
  group_by(ReasonRange) %>% 
  count() %>%
  droplevels() %>% 
  rename(range=n)

#Join to form a table
Temp <- full_join(Temp2,Temp1,by="Country") # the table
#-------------------------------------------------------------------------------------------------------
#Following objects used for in line code
AllCntry <- as.factor(Temp$Country)# list of countries
RangeCntry <- as.factor(Temp1$Country)# list of countries using a range
RangeReas <- as.factor(Temp3$ReasonRange)# list of countries using a range
ValCntry <- as.factor(Temp2$Country)# list of countries using a value
#-----------------------------------------------------------------------------------------------------------
# which countries use All
TypeAll <- dat.u.d %>% filter(NatType=="All") %>% group_by(Country) %>% count() %>% droplevels()
TypeAllCntry <- as.factor(TypeAll$Country)# list of countries using "all"" rather than a type specific value
#----------------------------------------------------------------------------------------------------------
# number of types in each group (IC Type)
TypeInGroup <- dat.u %>%  
  filter(!is.na(ICcode.TRAC),SumMetric !="EQR") %>%   # remove types not mapped & EQRs
  select(NatType,ICcode.TRAC) %>% 
  group_by(ICcode.TRAC,NatType) %>% 
  count %>% 
  group_by(ICcode.TRAC) %>% 
  count() %>% 
  rename(NatType=n)
#--------------------------------------------------------------------------------------------------------
# number of distinct standards in each group (IC Type)
StndInGroup <- dat.u.d %>%  
  filter(!is.na(ICcode.TRAC),SumMetric !="EQR") %>%   # remove types not mapped & EQRs
  select(ValueStd,ICcode.TRAC) %>% 
  group_by(ICcode.TRAC,ValueStd) %>% 
  count %>% 
  group_by(ICcode.TRAC) %>% 
  count() %>% 
  rename(ValueStd=n)
#--------------------------------------------------------------------------------------------------------
# create overview table
OverV1 <- select(dat.TW,ICcode.TRAC,ICType) %>% filter(!is.na(ICcode.TRAC)) %>% group_by() %>% distinct()
OverV2 <-left_join(OverV1,CntryInGroup) %>% rename(Cntry=n)
OverV3 <-left_join(OverV2,TypeInGroup)
OverV4 <- left_join(OverV3,StndInGroup) %>% arrange(ICcode.TRAC)# the table needed for report
#--------------------------------------------------------------------------------------------------------
# list countries reporting ranges with reason
CntryReasonRange <- filter(dat.u.d,RangeRep==TRUE) %>% select(Country,ReasonRange) %>% gather(Country,ReasonRange) %>% count(Country,ReasonRange)
NoReason <- filter(CntryReasonRange,is.na(ReasonRange)) %>% select(Country) %>% droplevels()
ReasonHighLow <-filter(CntryReasonRange,ReasonRange=="High low") %>% select(Country) %>% droplevels() 
ReasonSubType <-filter(CntryReasonRange,ReasonRange=="subType") %>% select(Country) %>% droplevels() 
ReasonSalinity <-filter(CntryReasonRange,ReasonRange=="salinity range") %>% select(Country) %>% droplevels()
#--------------------------------------------------------------------------------------------------------
