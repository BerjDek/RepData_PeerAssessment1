data <- read.csv("StormData.csv")
str(data)
summary(data)
library(ggplot2)
library(dplyr)
library(tidyr)

deaths <- aggregate(data$FATALITIES, by=list(type=data$EVTYPE ), sum)


DeathInjury <-  data %>%
  group_by(EVTYPE) %>%
  summarise(deaths = sum(FATALITIES), injuries = sum(INJURIES), totalcost = sum(PROPDMG),  
            deathrate = mean(FATALITIES), injrate = mean(INJURIES), averagecost = mean(PROPDMG))

rm(DeathInjury1,deaths)


ggplot(DeathInjury, aes(x = injuries, y = deaths, color = EVTYPE)) +
  geom_point() +
  labs(x = "Total Number of Injuries", y = "Total Number of Deaths",
       title = "Temperatures in Chicago")


+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
?pivot_longer()

ggplot(DeathInjury, aes(x = injuries, y = deaths)) + geom_point()
       
dat <- DeathInjury %>% pivotlong()