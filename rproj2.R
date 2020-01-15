library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(plyr)

filename <- "DataStorm.bz2"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, filename, method="curl")
  
}
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              #dest="DataStorm.bz2", 
              #method="curl")
data <- read.csv(bzfile("DataStorm.bz2"), 
                 header=TRUE,
                 sep=",",
                 stringsAsFactors=FALSE)

data$EVTYPE <- toupper(data$EVTYPE)
#newDF <- subset(data, FATALITIES >0 | INJURIES>0 | PROPDMG >0| CROPDMG >0)
newDF <-data %>% select(FATALITIES, EVTYPE, INJURIES, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP) %>% group_by(EVTYPE)



#smallDF <- newDF %>% select(EVTYPE, FATALITIES, INJURIES) %>%  group_by(EVTYPE)  %>% summarise(totalFat=sum(FATALITIES)) & summarise(totalInj= sum(INJURIES)) #arrange(desc(totalFat))
#smallDF <- newDF %>% select(EVTYPE, FATALITIES) %>%  group_by(EVTYPE) %>% filter(FATALITIES >50) %>% summarise(TotFatal = sum(FATALITIES)) 

#newDF <- aggregate(FATALITIES ~EVTYPE, newDF, FUN=sum)
 
#make EVTYPE an ordered factor to stop ggplot ordering them 
#smallDF$EVYTPE <- factor(smallDF$EVTYPE, levels=smallDF$EVTYPE)

#Find top 10 cause of fatalities

healthFat <- aggregate(FATALITIES ~EVTYPE, data= newDF, FUN=sum)
healthFat <- arrange(healthFat, desc(FATALITIES))
TophealthFat <- healthFat[1:10, ]
#plot top 10
g <- ggplot(TophealthFat, aes(x=reorder(EVTYPE, -FATALITIES) , y= FATALITIES, fill=EVTYPE))
Fatplot <- g + geom_col() +theme(axis.text.x=element_text(angle=90), 
                                 legend.position = "none",axis.title.x = element_blank() ) + labs(y="Amount of Fatalities",
                                 title="Total Fatalities Casued by Storms in the US")           


#Find top 10 cause of Injuries

healthInj<- aggregate(INJURIES ~ EVTYPE, data= newDF, FUN=sum)
healthInj <- arrange(healthInj, desc(INJURIES))
TophealthInj <-healthInj[1:10,]

#plot top 10 causes of injury
g2 <- ggplot(TophealthInj, aes(x=reorder(EVTYPE, -INJURIES), y=INJURIES, fill=EVTYPE))
Injplot <- g2+ geom_col() + theme(axis.text.x=element_text(angle=90), legend.position = "none",
                                  axis.title.x = element_blank() ) + labs(y="Amount of Injuries", 
                                  title="Total Injuries Casued by Storms in the US")           


#both together
ggarrange(Fatplot, Injplot)
#plot together?
#healthDF <- aggregate(cbind(INJURIES, FATALITIES) ~EVTYPE, data=newDF, FUN=sum)
#healthDF %>% arrange() %>% tidyr::gather("id", "value", 2:3) %>% ggplot(., aes(EVTYPE, value)) + geom_col()

#what are variables for PROPDMGEXP?
unique(newDF$PROPDMGEXP)
#"K" "M" ""  "B" "m" "+" "0" "5" "6" "4" "h" "2" "7" "3" "H" "-"

#change to upper case
newDF$PROPDMGEXP <- toupper(newDF$PROPDMGEXP)

#Map multiplication values to PROPDMGEXP variables
newDF$PROPDMGEXP <- mapvalues(newDF$PROPDMGEXP, from = c("K", "M", "",  "B", "+", "0", "5", "6", "?" ,"4" ,"2" ,"3","H" ,"7", "-", "1" ,"8"),
                              to=c(10^3,10^6, 10^0,  10^9, 10^0, 10^0, 10^5, 10^6, 10^0, 10^4, 10^2, 10^3, 10^2, 10^7, 10^0, 10^1, 10^8))
                              
newDF$PROPDMGEXP <- as.numeric(as.character(newDF$PROPDMGEXP))  
newDF$PropLossTotal <- (newDF$PROPDMG * newDF$PROPDMGEXP)
                              
sumPropLoss <-aggregate(PropLossTotal ~ EVTYPE, data=newDF, FUN=sum)
sumPropLoss <- arrange(sumPropLoss, desc(PropLossTotal))
TopPropLoss <- sumPropLoss[1:10,]

#Plot
g4 <- ggplot(TopPropLoss, aes(x=reorder(EVTYPE, -PropLossTotal), y=PropLossTotal, fill=EVTYPE))
g4 + geom_col() +theme(axis.text.x=element_text(angle=90), legend.position = "none",axis.title.x = element_blank() ) + labs(y="Property Loss (US$)", title="Total Economic Loss of Propery Damage by Storm in the US")           

                            #change to correct number values
#CROPDMGNUM<- c("\"\"" = 10^0,
              # "?" = 10^0, 
               #"0" = 10^0,
               #"K" = 10^3,
               #"M" = 10^6,
               #"B" = 10^9)
#newDF$CROPDMGEXP <- CROPDMGNUM[as.character(newDF$CROPDMGEXP)]
#newDF$CROPDMGEXP[is.na(newDF$CROPDMGEXP)] <- 10^0


#what are variables for CROPDMGEXP?
unique(newDF$CROPDMGEXP)
#""  "M" "K" "m" "B" "?" "0" "k"
#change to upper case
newDF$CROPDMGEXP <- toupper(newDF$CROPDMGEXP)
unique(newDF$CROPDMGEXP)
#"  "M" "K" "B" "?" "0"

#Map multiplication values to CROPDMGEXP variables

newDF$CROPDMGEXP <- mapvalues(newDF$CROPDMGEXP, from = c("", "?", "0", "K","M", "B"), to=c(10^0,10^0, 10^0,10^3, 10^6,10^9))
newDF$CROPDMGEXP <- as.numeric(as.character(newDF$CROPDMGEXP))
newDF$CropDMGTotal <- (newDF$CROPDMG * newDF$CROPDMGEXP)

sumCropLoss <- aggregate(CropDMGTotal ~ EVTYPE, data=newDF, FUN=sum)
sumCropLoss <- arrange(sumCropLoss, desc(CropDMGTotal))
TopCropLoss <- sumCropLoss[1:10,]

#Plot
g3 <- ggplot(TopCropLoss, aes(x=reorder(EVTYPE, -CropDMGTotal), y= CropDMGTotal, fill= EVTYPE))
g3 + geom_col() + theme(axis.text.x=element_blank())


#Both Property and Crop damage together
newDF$TotalEconDMG <-newDF$CROPDMGEXP + newDF$PROPDMGEXP
CropPropTotal <- aggregate(TotalEconDMG~EVTYPE, data=newDF, FUN=sum)
CropPropTotal <- arrange(CropPropTotal, desc(TotalEconDMG))
TopCropPropTotal <- CropPropTotal[1:10,]

#plot
g5 <- ggplot(TopCropPropTotal, aes(x=reorder(EVTYPE, -TotalEconDMG), y=TotalEconDMG, fill= EVTYPE))
g5 + geom_col() + theme(axis.text.x=element_blank())




#---------------------------------------------------------------------------------------------------------------
#newDF$PROPDMGEXP <- na.omit(newDF$PROPDMGEXP)
#"K" "M" ""  "B" "+" "0" "5" "6" "4" "H" "2" "7" "3" "-"

#change to correct number values
#PROPDMGNUM <- c("\"\"" = 10^0,
#"-" = 10^0, 
#"+" = 10^0,
#"0" = 10^0,
#"1" = 10^1,
#"2" = 10^2,
#"3" = 10^3,
#"4" = 10^4,
#"5" = 10^5,
#"6" = 10^6,
#"7" = 10^7,
#"8" = 10^8,
#"9" = 10^9,
#"H" = 10^2,
#"K" = 10^3,
#"M" = 10^6,
#"B" = 10^9) 

#newDF$PROPDMGEXP <- PROPDMGNUM[as.character(newDF$PROPDMGEXP)]
#newDF$PROPDMGEXP[is.na(newDF$PROPDMGEXP)] <- 10^0


#Multiply Property Damage columne with Property Damage factor create new column

newDF$PropertyLoss <- newDF$PROPDMG * newDF$PROPDMGEXP
PropLoss <- aggregate(PropertyLoss~EVTYPE, data=newDF, FUN=sum)
PropLoss <- arrange(PropLoss, desc(PropertyLoss))
TopPropLoss <- PropLoss[1:10,]

#plot
g3 <- ggplot(TopPropLoss, aes(x= EVTYPE, y= PropertyLoss, fill=EVTYPE))
PropPLot <- g3 + geom_col() + theme(axis.text.x=element_blank())
#Multiply Crop Damage and Crop Expense
newDF$CropLossTotal <- newDF$CROPDMG *newDF$CROPDMGEXP
CropLoss <- aggregate(CropLossTotal ~ EVTYPE, data=newDF, FUN=sum)
CropLoss <-arrange(CropLoss, desc(CropLossTotal))
TopCropLoss <- CropLoss[1:10,]
#plot
g4 <- ggplot(TopCropLoss, aes(x=EVTYPE, y=CropLossTotal, fill=EVTYPE))
CropPlot <-g4 + geom_col() +theme(axis.text.x=element_blank())
``
#plot both top ten causes of  porperty and crop damage side by side\
ggarrange(PropPLot, CropPlot)

#Combine Property and Crop damage
newDF$CropPropDMG <- newDF$CROPDMGEXP + newDF$PROPDMGEXP
newDF$CropPropDMG

#Top ten causes of Property and Crop damage combined
PropCropLoss <- aggregate(CropPropDMG ~EVTYPE, data=newDF, FUN=sum)
PropCropLoss <- arrange(PropCropLoss, desc(CropPropDMG))

TopPropCropLoss <- PropCropLoss[1:10,]
#plot
g5<- ggplot(TopPropCropLoss, aes(x=EVTYPE, y= CropPropDMG, fill=EVTYPE))
g5 + geom_col()