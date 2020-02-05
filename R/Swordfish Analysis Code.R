##Making a histogram of Surface Use Accross Time and Day
library(dplyr)
DSU.100976 <- X100976_Series %>% 
  filter(Depth < 1)

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
  geom_boxplot(mapping = aes(x = Age, y = Surface.Prop, color = Age))

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

#1.31.20/1.32.20 - Adding Resolution Column to DSU.[Individual] data.frames
DSU."Individual"$Resolution <- Time.Resolution("Individual")

Time.Resolution <- function(x) {
  as.numeric(x[2,7] - x[1,7])
}


hms.Resolution <- function(x) {
  tvalue <- hms(
    Time.Resolution(x),
    0, 0)
}

#correcting 100978 from data.frames
Swordfish.Surface <- Swordfish.Surface[-c(16),]
new <- data.frame(Age = 'SA', Individual = '100978', Surface.Prop = 0)
Swordfish.Surface <- rbind(Swordfish.Surface, new)
#AND
SA <- SA[-c(2),]
SA <- rbind(SA, new)

#re-do statistics 
xbar_0.5m <- mean(Swordfish.Surface$Surface.Prop)
# - SEE 1.17.20 AND ONEWAYTESTS - #

#2.4.20 - Streamlining Resolution function

DSU <- function(x) {
  as.name(paste0(DSU., x))
} #Non-functional because DSU.100976 is not a name

CallDSU <- function(x) {
  View(
    DSU(x)
  )
} #Disregard

#Juvenile Adjustment: 
DSU.98751$Seconds <- hms.Resolution(X98751_Series)
DSU.104668$Seconds <- hms.Resolution(X104668_Series)
DSU.104670$Seconds <- 300
DSU.104671$Seconds <- hms.Resolution(X104671_Series)
DSU.104672$Seconds <- hms.Resolution(X104672_Series)
DSU.98721$Seconds <- hms.Resolution(X98721_Series)
DSU.98722$Seconds <- hms.Resolution(X98722_Series)

#SA Adjustment
DSU.95975$Seconds <- hms.Resolution(X95975_Series)
DSU.100976$Seconds <- hms.Resolution(X100976_Series)

#Adult Adjustment
DSU.110490$Seconds <- hms.Resolution(X110490_Series)
DSU.110497$Seconds <- hms.Resolution(X110497_Series)
DSU.110498$Seconds <- hms.Resolution(X110498_Series)
DSU.106795$Seconds <- hms.Resolution(X106795_Series)
DSU.110496$Seconds <- 5

#Wrangling all_sword_hmmoce_locs
unique(all_sword_hmmoce_locs[["ptt"]])
all_sword_hmmoce_locs$ptt <- factor(all_sword_hmmoce_locs$ptt, levels = c("95975", "98721", "98722", "100976", "100980", seq(104668, 104672), "106788", "106795", "110490", "110491", "110496", "110497", "110498", "98751"))


