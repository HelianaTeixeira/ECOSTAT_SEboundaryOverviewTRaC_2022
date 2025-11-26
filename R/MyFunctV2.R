#R script containing functions used for plotting

# Geoff Phillips,  11 September 2019 modified by HT for TRAC

#There are 7 different plot types

#1) BPChk(df)   Box plot of "ValueStd" by "Country", facets are "UnitUsed".
#               Need to pass name of dataframe (df) 
#               Used for initial checking of the data and to present results
#                 where not useful to show range values separately

#2) BPCntry()   Box plots of "ValueStd" & "ValueUpStd" by "Country".
#               No parameters to pass
#               Upper value of range in red, lower value of range in blue, value in black
#               uses function "position_hnudge" to ofset boxes,
#                 offset can be changed, current offsets are -0.2,0.2,0
#               Used where range is potentially used to define upper
#                 and lower limits, e.g. pH, Oxygen

#3) BPType(Pos) Box plot of "ValueStd" & "ValueUpStd" by "ICcode.TRAC" (ICtypes)
#               Upper value of range in red, lower value of range in blue, value in black
#               Can pass parameter "Pos" the position (value y) to print number of 
#                 records for box. Defaults to min of ValueStd.
#               Used where useful to show range values separately e.g. pH Oxygen

#4) BPType1(Pos)  Box plot of "ValueStd & ValueUpStd" by "ICcode.TRAC" (ICtypes)
#                 reshapes the data using tidyr "gather" to combine range values
#                 Used where splitting box plots into value upper and lower ranges
#                  is not helpful, (most cases for lakes)

#5) DPType()    Dot plots of "ValueStd" & "ValueUpStd" by "Country" with "ICcode.TRAC" (ICtypes)
#               as facets. Upper value of range in red, lower value of range in blue, value in black 
#               No parameters to pass

#6) DP()        Dot plots of "ValueStd" & "ValueUpStd" by "Country" (as above but without facets)
#               probably not useful, but used it to check data occasionally

#7) DP2()        Dot plots of "ValueStd" & "ValueUpStd" by "ICcode.TRAC" (as above but without facets)


# There are also versions of the above with Log scale for Y axis.  e.g. BPChk.lg
# note that DPType.lg(bk,ymin,ymax) & DP.lg(bk,ymin,ymax) require parameters break 
# points e.g c(10,100,1000) a min max values for y axis.
# It would also be possible to deal with log axis and different scale requirements by 
# calling the above functions and including additional ggplot2 statements
# e.g. BPChk(dat.u.d) + scale_y_continuous(trans='log10')
#  or to rotate x axis labels +   theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))

# load these functions using source("MyFunctV2.R") in preliminary section of scripts





# horizontal nudge position adjustment, see https://github.com/tidyverse/ggplot2/issues/2733
position_hnudge <- function(x = 0) {
  ggproto(NULL, PositionHNudge, x = x)
}

PositionHNudge <- ggproto("PositionHNudge", Position,
                          x = 0,
                          required_aes = "x",
                          setup_params = function(self, data) {
                            list(x = self$x)
                          },
                          compute_layer = function(data, params, panel) {
                            transform_position(data, function(x) x + params$x)
                          }
)


# Function used to plot the data for initial checks, requires dataframe name (df)
BPChk <- function(df){
  ggplot(df, aes(x=Country, y=ValueStd)) + 
    geom_boxplot() + 
    facet_wrap(~ UnitUsed)+
    theme_bw() +
    ylab(ylb)
}

# Function used to plot the data for initial checks log scale, requires dataframe name (dat)
BPChk.lg <- function(df){
  ggplot(df, aes(x=Country, y=ValueStd)) + 
    scale_y_continuous(trans='log10') + 
    geom_boxplot() + 
    facet_wrap(~ UnitUsed)+
    theme_bw() +
    ylab(ylb)
}


#Function to produce box plot with Grp as Country
BPCntry <- function(){
  #plot the lower values
  ggplot(subset(dat.u.d, !is.na(ValueUpStd)), aes(x=Country, y=ValueStd)) + 
    geom_boxplot(width=0.3,position = position_hnudge(x = 0.2),color="blue")+
    theme_bw() +
    ylab(ylb)+
  #add the upper values (when a range is not specified (when the ValueUp is NA)   
  geom_boxplot(data=subset(dat.u.d, !is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueUpStd),color="red",width=0.3,position = position_hnudge(x = -0.2)) +
  #add the values when no range is specified (when the ValueUp is NA)
  geom_boxplot(data=subset(dat.u.d, is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueStd),position = position_hnudge(x = 0),color="black",width=0.3)
}

BPCntry.lg <- function(){
  #plot the lower values
  ggplot(subset(dat.u.d, !is.na(ValueUpStd)), aes(x=Country, y=ValueStd)) + 
    geom_boxplot(width=0.3,position = position_hnudge(x = 0.2),color="blue")+  
    scale_y_continuous(trans='log10') +
    theme_bw() +
    ylab(ylb)+
    #add the upper values (when a range is not specified (when the ValueUp is NA)   
    geom_boxplot(data=subset(dat.u.d, !is.na(ValueUpStd)), mapping = 
                   aes(x = Country, y = ValueUpStd),color="red",width=0.3,position = position_hnudge(x = -0.2)) +
    #add the values when no range is specified (when the ValueUp is NA)
    geom_boxplot(data=subset(dat.u.d, is.na(ValueUpStd)), mapping = 
                   aes(x = Country, y = ValueStd),position = position_hnudge(x = 0),color="black",width=0.3)
}


#Function to produce box plot with Grp as ICcode.TRAC
BPType <- function(Pos=min(dat.u.d$ValueStd)){
  #plot the lower values
  ggplot(subset(dat.u.d, !is.na(ValueUpStd)), aes(x=ICcode.TRAC, y=ValueStd)) + 
    geom_boxplot(width=0.3,position = position_hnudge(x = 0.2),color="blue")+
    theme_bw() +
    ylab(ylb)+
    #add the upper values (when a ramge is not specified (when the ValueUp is NA)   
    geom_boxplot(data=subset(dat.u.d, !is.na(ValueUpStd)), mapping = 
                   aes(x = ICcode.TRAC, y = ValueUpStd),color="red",width=0.3,position = position_hnudge(x = -0.2)) +
    #add the values when no range is specified (when the ValueUp is NA)
    geom_boxplot(data=subset(dat.u.d, is.na(ValueUpStd)), mapping = 
                   aes(x = ICcode.TRAC, y = ValueStd),position = position_hnudge(x = 0),color="black",width=0.3)+
    geom_text(data=NinGroup,aes(x=ICcode.TRAC,y=Pos, label=n), 
              size = 3, vjust = 0)
}

BPType.lg <- function(Pos=min(dat.u.d$ValueStd)){
  #plot the lower values
  ggplot(subset(dat.u.d, !is.na(ValueUpStd)), aes(x=ICcode.TRAC, y=ValueStd)) + 
    geom_boxplot(width=0.3,position = position_hnudge(x = 0.2),color="blue")+
    scale_y_continuous(trans='log10') +
    theme_bw() +
    ylab(ylb)+
    #add the upper values (when a ramge is not specified (when the ValueUp is NA)   
    geom_boxplot(data=subset(dat.u.d, !is.na(ValueUpStd)), mapping = 
                   aes(x = ICcode.TRAC, y = ValueUpStd),color="red",width=0.3,position = position_hnudge(x = -0.2)) +
    #add the values when no range is specified (when the ValueUp is NA)
    geom_boxplot(data=subset(dat.u.d, is.na(ValueUpStd)), mapping = 
                   aes(x = ICcode.TRAC, y = ValueStd),position = position_hnudge(x = 0),color="black",width=0.3)+
    geom_text(data=NinGroup,aes(x=ICcode.TRAC,y=Pos, label=n), 
              size = 3, vjust = 0)
}


BPType1 <- function(Pos=min(dat.u.d$ValueStd)){
  dat.u.d.l <- gather(dat.u.d,Stand,ValueCombStd,ValueStd,ValueUpStd)
  ggplot(dat.u.d.l, aes(x=ICcode.TRAC, y=ValueCombStd)) + 
    geom_boxplot(color="black") +  
    ylab(ylb) +
    theme_bw() +
  geom_text(data=NinGroup,aes(x=ICcode.TRAC,y=Pos, label=n), 
            size = 3, vjust = 0)
}

BPType1.lg <- function(Pos=min(dat.u.d$ValueStd)){
  dat.u.d.l <- gather(dat.u.d,Stand,ValueCombStd,ValueStd,ValueUpStd)
  ggplot(dat.u.d.l, aes(x=ICcode.TRAC, y=ValueCombStd)) +  
    scale_y_continuous(trans='log10') +
    geom_boxplot(color="black") +
    theme_bw() +
    ylab(ylb)+
    geom_text(data=NinGroup,aes(x=ICcode.TRAC,y=Pos, label=n), 
              size = 3, vjust = 0) 
}


#Function to produce dot plots with facets by IC type
DPType <- function(){
# start by plotting the value upper in red  
ggplot(dat.u.d, aes(x=Country, y=ValueUpStd,shape=SumMetric))+
  geom_point(color="red",position = position_nudge(x= -0.1 ,y = 0))+
  scale_shape_manual(values=symb,name  ="") +
# add lower point of range as blue symbol  
  geom_point(data = subset(dat.u.d,!is.na(ValueUpStd)), mapping = 
               aes(x = Country, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.1 ,y = 0))+
  ylab(ylb)+xlab("")+
  facet_wrap(~ ICcode.TRAC,ncol=2,nrow=7) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))+  
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black")) +
# Add the values (exclude those which were a range), this needs to  be done last to avoid the legend points being colored
  geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
               aes(x = Country, y = ValueStd, shape = SumMetric),color="black")
} 



 

#Function to produce dot plots with facets by IC type log scale
DPType.lg <- function(bk,ymin,ymax){
# start by plotting the value upper in red  
  ggplot(dat.u.d, aes(x=Country, y=ValueUpStd,shape=SumMetric))+
    geom_point(color="red",position = position_nudge(x= -0.1 ,y = 0))+
    scale_shape_manual(values=symb,name  ="") +
    scale_y_continuous(trans='log10',breaks = bk,limits=c(ymin,ymax)) +
# add lower point of range as blue symbol  
    geom_point(data = subset(dat.u.d,!is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.1 ,y = 0))+
    ylab(ylb)+xlab("")+
    facet_wrap(~ ICcode.TRAC,ncol=2,nrow=7) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))+  
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black")) +
# Add the values (exclude those which were a range), this needs to  be done last to avoid the legend points being colored
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueStd, shape = SumMetric),color="black")
}

#Function producing dot plot without facets by Country
DP <- function(){
  # start by plotting the value upper in red  
  ggplot(dat.u.d, aes(x=Country, y=ValueUpStd,shape=SumMetric))+
    geom_point(data = subset(dat.u.d, ReasonRange!="salinity range"), color="red",position = position_nudge(x= -0.1 ,y = 0))+
    scale_shape_manual(values=symb,name  ="") +
    # add lower point of range as blue symbol  
    geom_point(data = subset(dat.u.d,!is.na(ValueUpStd) & ReasonRange!="salinity range"), mapping = 
                 aes(x = Country, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.1 ,y = 0))+
    ylab(ylb)+xlab("")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))+  
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black")) +
    # Add the values (exclude those which were a range), this needs to  be done last to avoid the legend points being colored
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueStd, shape = SumMetric),color="black")+
    geom_point(data = subset(dat.u.d, ReasonRange =="salinity range"),
               mapping = aes(x = Country, y = ValueUpStd, shape = SumMetric),color="black")
}


#Function producing dot plot without facets, log scale
DP.lg <- function(bk,ymin,ymax){
  # start by plotting the value upper in red  
  ggplot(dat.u.d, aes(x=Country, y=ValueUpStd,shape=SumMetric))+
    geom_point(color="red",position = position_nudge(x= -0.1 ,y = 0))+
    scale_shape_manual(values=symb,name  ="") +
    scale_y_continuous(trans='log10',breaks = bk,limits=c(ymin,ymax)) +
    # add lower point of range as blue symbol  
    geom_point(data = subset(dat.u.d,!is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.1 ,y = 0))+
    ylab(ylb)+xlab("")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))+  
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black")) +
    # Add the values (exclude those which were a range), this needs to  be done last to avoid the legend points being colored
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = Country, y = ValueStd, shape = SumMetric),color="black")
}


#Function producing dot plots without facets by IC Type
DP2 <- function(){
  # start by plotting the value upper in red  
  ggplot(dat.u.d, aes(x=ICcode.TRAC, y=ValueUpStd,shape=SumMetric))+
    geom_point(data = subset(dat.u.d, ReasonRange!="salinity range"), color="red",position = position_nudge(x= -0.1 ,y = 0))+
    scale_shape_manual(values=symb,name  ="") +
    # add lower point of range as blue symbol  
    geom_point(data = subset(dat.u.d,!is.na(ValueUpStd)& ReasonRange!="salinity range"), mapping = 
                 aes(x = ICcode.TRAC, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.1 ,y = 0))+
    ylab(ylb)+xlab("")+
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = ICcode.TRAC, y = ValueStd, shape = SumMetric),color="black")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))+  
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black"))+
    # Add the values (exclude those which were a range), this needs to  be done last to avoid the legend points being colored
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = ICcode.TRAC, y = ValueStd, shape = SumMetric),color="black")+
    geom_point(data = subset(dat.u.d, ReasonRange =="salinity range"),
               mapping = aes(x = ICcode.TRAC, y = ValueUpStd, shape = SumMetric),color="black")
}


test <- function(){
  # start by plotting the value upper in red  
  ggplot(dat.u.d, aes(x=ICcode.TRAC, y=ValueUpStd,shape=SumMetric))+
    geom_point(color="red",position = position_nudge(x= -0.1 ,y = 0))
}

#Function producing dot plots without facets by IC Type
DP2.lg <- function(bk,ymin,ymax){
  # start by plotting the value upper in red  
  ggplot(dat.u.d, aes(x=ICcode.TRAC, y=ValueUpStd,shape=SumMetric))+
    geom_point(color="red",position = position_nudge(x= -0.1 ,y = 0))+
    scale_shape_manual(values=symb,name  ="") +
    scale_y_continuous(trans='log10',breaks = bk,limits=c(ymin,ymax)) +
    # add lower point of range as blue symbol  
    geom_point(data = subset(dat.u.d,!is.na(ValueUpStd)), mapping = 
                 aes(x = ICcode.TRAC, y = ValueStd, shape = SumMetric),color="blue",position = position_nudge(x=0.1 ,y = 0))+
    ylab(ylb)+xlab("")+
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = ICcode.TRAC, y = ValueStd, shape = SumMetric),color="black")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))+  
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black"))+
    # Add the values (exclude those which were a range), this needs to  be done last to avoid the legend points being colored
    geom_point(data = subset(dat.u.d,is.na(ValueUpStd)), mapping = 
                 aes(x = ICcode.TRAC, y = ValueStd, shape = SumMetric),color="black")
}

#Functions to produce coplots with facets by IC type, Marine region and GIG
coPIC <- function(){
  ggplot(dat.shrt.l, aes(Salinity, Value))+
    geom_point(aes(color = factor(Country)))+
    facet_wrap(~ ICcode.TRAC, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}

coPMaReg <- function(){
  ggplot(dat.shrt.l, aes(Salinity, Value))+
    geom_point(aes(color = factor(ICcode.TRAC)))+
    facet_wrap(~ MarineRegion, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}

coPGIG.MR <- function(){
  ggplot(dat.shrt.l, aes(Salinity, Value))+
    geom_point(aes(color = factor(MarineRegion)))+
    facet_wrap(~ GIG, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}

coPGIG.IC <- function(){
  ggplot(dat.shrt.l, aes(Salinity, Value))+
    geom_point(aes(color = factor(ICcode.TRAC)))+
    facet_wrap(~ GIG, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}


#for TN,TP,PO4
#Functions to produce coplots with facets by IC type, Marine region and GIG
coPICn <- function(){
  ggplot(dat.shrt.l, aes(Salinity, ValueStd))+
    geom_point(aes(color = factor(Country)))+
    facet_wrap(~ ICcode.TRAC, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}

coPMaRegn <- function(){
  ggplot(dat.shrt.l, aes(Salinity, ValueStd))+
    geom_point(aes(color = factor(ICcode.TRAC)))+
    facet_wrap(~ MarineRegion, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}

coPGIG.MRn <- function(){
  ggplot(dat.shrt.l, aes(Salinity, ValueStd))+
    geom_point(aes(color = factor(MarineRegion)))+
    facet_wrap(~ GIG, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}

coPGIG.ICn <- function(){
  ggplot(dat.shrt.l, aes(Salinity, ValueStd))+
    geom_point(aes(color = factor(ICcode.TRAC)))+
    facet_wrap(~ GIG, ncol = 2)+
    labs(y = ylb,x="Salinity")+
    theme_bw()+
    theme(legend.position = "bottom")+
    theme(legend.title = element_blank())
}
