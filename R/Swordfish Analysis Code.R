##Making a histogram of Surface Use Accross Time and Day
library(dplyr)
DSU.110496 <- X110496_Archive %>% 
  filter(Depth < -2)

head(table(X110496_Archive$Depth))

library(hms)
daytime <- as_hms(c("00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00", "05:00:00", "06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00", "21:00:00",  "22:00:00",  "23:00:00"))

ggplot(data = DSU.100976) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime) +
  facet_wrap( ~Day) +
  ggtitle("Daily Surface Use _ 100976")
#OR
ggplot(data = DSU.100976) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime) +
  ggtitle("Daily Surface Use _ 100976")

ggplot(data = Swordfish.Surface) +
  geom_boxplot(mapping = aes(x = Age, y = Surface.Prop, color = Age)) +

##1.17.20 - Using onewaytests
library(onewaytests)
homog.test(Surface.Prop ~ Age, data = Swordfish.Surface, method = "Bartlett")
nor.test(Surface.Prop ~ Age, data = Swordfish.Surface, method = "SW", plot = "qqplot-histogram")
welch.test(Surface.Prop ~ Age, data = Swordfish.Surface, rate = 0.1)

##1.23.20 - Using separate()
library(tidyr)
name <- DSU.110496 %>% separate(Time, c("Time", "Day"), sep = "([// ])")

##Adding Age Column to DSU data.frames
age <- "Sub.Adult", "Juvenile", "Adult"
DSU.100976$Age <- age

##1.24.20 - Merging data.frames
##When adding rows us rbind()
DSU.SubAdult <- rbind(DSU.100976, DSU.100978, DSU.95975)
DSU.Juvenile <- rbind(DSU.98751, DSU.104668, DSU.104670, DSU.104671, DSU.104672, DSU.98721, DSU.98722)
DSU.Adult <- rbind(DSU.110490, DSU.110497, DSU.110498, DSU.106795)
DSU.All <- rbind(DSU.SubAdult, DSU.Juvenile, DSU.Adult)

##Ultimate Daily Surface Use Figure
ggplot(data = DSU.All) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_grid(DeployID ~ Age) +
  coord_flip()
    
ggplot(data = DSU.SubAdult) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_wrap(~ DeployID) + 
  coord_flip() +
  ggtitle("Daily Surface Use in Sub.adults")

ggplot(data = DSU.Adult) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_wrap(~ DeployID) + 
  coord_flip() +
  ggtitle("Daily Surface Use in Adults")
    
ggplot(data = DSU.Juvenile) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_wrap(~ DeployID) + 
  coord_flip() +
  ggtitle("Daily Surface Use in Juveniles")   

#1.28.20 - Depth Distribution by proportion
ggplot(X104672_Series, aes(x = Depth, y = ..count../sum(..count..))) +
  geom_histogram(bins = 40, color = "navy", fill = NA) +
  coord_flip() + scale_x_reverse() +
  ylab("Pdepth") +
  theme_classic() +
  ggtitle("Depth Probability") +
  

sutable <- prop.table(table(X104672_Series$Depth))

#1.30.20 - Surface Use Histogram w/ Time Resolution
ggplot(data = DSU.100976) +
  +   geom_histogram(aes(x = Time, y = (..count..)*7.5, fill = Day), breaks = daytime) +
  +   ggtitle("Daily Surface Use _ 100976") +
  +     ylab("Duration (minutes)") +
  +     xlab("Time of Day")
