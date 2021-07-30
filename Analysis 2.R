
#Get Important Columns
install.packages("wesanderson")
library(wesanderson)
data1 <- data %>%  select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
#Check the frequencey of different events
table(data1$EVTYPE)
#combine events into catagories
data1$EVENT <- "OTHER"
data1$EVENT[grep("HAIL", data1$EVTYPE, ignore.case = TRUE)] <- "HAIL"
data1$EVENT[grep("HEAT", data1$EVTYPE, ignore.case = TRUE)] <- "HEAT"
data1$EVENT[grep("FLOOD", data1$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
data1$EVENT[grep("WIND", data1$EVTYPE, ignore.case = TRUE)] <- "WIND"
data1$EVENT[grep("STORM", data1$EVTYPE, ignore.case = TRUE)] <- "STORM"
data1$EVENT[grep("SNOW", data1$EVTYPE, ignore.case = TRUE)] <- "SNOW"
data1$EVENT[grep("TORNADO", data1$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
data1$EVENT[grep("WINTER", data1$EVTYPE, ignore.case = TRUE)] <- "WINTER"
data1$EVENT[grep("RAIN", data1$EVTYPE, ignore.case = TRUE)] <- "RAIN"
# listing the transformed event types 
sort(table(data1$EVENT), decreasing = TRUE)
#sort costs based on the XP given
data1$PROPDMGEXP <- as.character(data1$PROPDMGEXP)
data1$PROPDMGEXP[!grepl("K|M|B", data1$PROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
data1$PROPDMGEXP[grep("K", data1$PROPDMGEXP, ignore.case = TRUE)] <- "3"
data1$PROPDMGEXP[grep("M", data1$PROPDMGEXP, ignore.case = TRUE)] <- "6"
data1$PROPDMGEXP[grep("B", data1$PROPDMGEXP, ignore.case = TRUE)] <- "9"
data1$PROPDMGEXP <- as.numeric(as.character(data1$PROPDMGEXP))
data1$property.damage <- data1$PROPDMG * 10^data1$PROPDMGEXP

data1$CROPDMGEXP <- as.character(data1$CROPDMGEXP)
data1$CROPDMGEXP[!grepl("K|M|B", data1$CROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
data1$CROPDMGEXP[grep("K", data1$CROPDMGEXP, ignore.case = TRUE)] <- "3"
data1$CROPDMGEXP[grep("M", data1$CROPDMGEXP, ignore.case = TRUE)] <- "6"
data1$CROPDMGEXP[grep("B", data1$CROPDMGEXP, ignore.case = TRUE)] <- "9"
data1$CROPDMGEXP <- as.numeric(as.character(data1$CROPDMGEXP))
data1$crop.damage <- data1$CROPDMG * 10^data1$CROPDMGEXP

AggHumanCost <- data1 %>%  
  group_by(EVENT)  %>% 
  summarise(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES)) %>% 
  pivot_longer(-EVENT, names_to = "type", values_to = "amount" )

AvgHumanCost <- data1 %>%  
  group_by(EVENT)  %>% 
  summarise(Fatalities = mean(FATALITIES), Injuries = mean(INJURIES)) %>% 
  pivot_longer(-EVENT, names_to = "type", values_to = "amount" )




ggplot(AggHumanCost, aes(x = EVENT, y = amount, fill = type)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event Type") + 
  ylab("Total number of health impact") +
  ggtitle("Weather event types impact on public health") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set1")

Aplot <- ggplot(AvgHumanCost, aes(x = EVENT, y = amount, fill = type)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event Type") + 
  ylab("Average health impact per incident") +
  ggtitle("Weather event types impact on public health") +
  theme(plot.title = element_text(hjust = 0.5))

AggEconCost <- data1 %>%  
  group_by(EVENT)  %>% 
  summarise(LostProperty = sum(property.damage), LostCrop = sum(crop.damage)) %>% 
  pivot_longer(-EVENT, names_to = "type", values_to = "amount" ) +
  

AvgEconCost <- data1 %>%  
  group_by(EVENT)  %>% 
  summarise(LostProperty = mean(property.damage), LostCrop = mean(crop.damage)) %>% 
  pivot_longer(-EVENT, names_to = "type", values_to = "amount" )


ggplot(AggEconCost, aes(x = EVENT, y = amount, fill = type)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event Type") + 
  ylab("Total Cost of Economic Damages") +
  ggtitle("Weather event types impact on Economy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="YlGnBu")

ggplot(AvgEconCost, aes(x = EVENT, y = amount, fill = type)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Event Type") + 
  ylab("Total Cost of Economic Damages") +
  ggtitle("Weather event types impact on Economy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="YlGnBu")
