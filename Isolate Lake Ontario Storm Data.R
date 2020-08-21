# Owen Alexander Isley
# Professor Pawlicki
# Lake Ontario Independent Study
# 8 April 2020
# N.O.A.A. Storm Events Database: Exploratory Analysis
# Reference https://rstudio-pubs-static.s3.amazonaws.com/82514_6be266f96736496a9dd65590d4ad8bb4.html

library(dplyr, quietly = TRUE)
library(R.utils, quietly = TRUE)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(stringr)

########## Downloading data ##########
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists("./data/repdata-data-StormData.csv")) {
  if (!file.exists("./data/repdata-data-StormData.csv.bz2")) {
    download.file(fileUrl, destfile = "./data/repdata-data-StormData.csv.bz2", method="curl")
  }  
  bunzip2("./data/repdata-data-StormData.csv.bz2", "./data/repdata-data-StormData.csv")
}

noaa_full <- read.csv("./data/repdata-data-StormData.csv")
dim(noaa_full)
head(noaa_full)

########## Keeping only the fields pertinent to our analysis  ##########
noaa <- noaa_full %>% select(BGN_DATE, BGN_TIME, COUNTY, COUNTYNAME, STATE, EVTYPE, END_DATE, END_TIME, LENGTH, WIDTH, F, MAG, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, LATITUDE, LONGITUDE)
tail(noaa)

# Filter out only Lake Ontario data
# An approximate latitude and longitude "box" for Lake Ontario was estimated using https://www.latlong.net/ (43 <= lat <= 44.1) (76 <= lon <= 80)
# **NOAA's formatting for latitude: e.g. 43.07 => 4307
# **NOAA's formatting for longitude: longitude values are reported as POSITIVE despite truly being negative coordinates! e.g. -76.47 => 7647 Beware!
ontario <- noaa %>% filter(LATITUDE >= 4300, LATITUDE <= 4410, LONGITUDE <= 8000, LONGITUDE <= 7600)
dim(ontario)
head(ontario)
tail(ontario)
########## Exploratory visualizations  ##########

########## Frequency plot of EVTYPE  ##########
event_frequencies <- ontario %>%
  group_by(EVTYPE) %>%
  summarise(counts = n())
event_frequencies

frequency_plot_events <- ggplot(event_frequencies,aes(x = EVTYPE, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) +
  ggtitle("Frequency Plot of Event Types") +
  theme_pubclean() 
frequency_plot_events  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.text = element_text(size = 10)) 

########## Line graph flooding events over time  ##########
flooding_events <- ontario %>% select(BGN_DATE, EVTYPE)
head(flooding_events)
dim(flooding_events)
flooding_events <- flooding_events %>% filter(EVTYPE == "FLASH FLOOD" | EVTYPE == "FLOOD")
dim(flooding_events)

# Re-formatting the date for readability
flooding_events$BGN_DATE = str_sub(flooding_events$BGN_DATE, -12)
head(flooding_events)
flooding_events$BGN_DATE = str_sub(flooding_events$BGN_DATE, 1, 4)
head(flooding_events)
dim(flooding_events)

# Calculating number of floods per year
flooding_frequencies <- as.data.frame(table(flooding_events$BGN_DATE))
names(flooding_frequencies)[1] <- "Year"
names(flooding_frequencies)[2] <- "Floods"
flooding_frequencies

ggplot(data=flooding_frequencies, aes(x=Year, y=Floods, group=1)) +
  labs(title="Number of Floods versus Time",
       y = "Frequency", x="Year") +
  geom_line() +
  geom_point()

########## Line graph of flooding events AND precipitation related storms ##########
precip_events <- ontario %>% select(BGN_DATE, EVTYPE)
precip_events <- precip_events %>% filter(EVTYPE == "HAIL" | EVTYPE == "HEAVY RAIN" | EVTYPE == "MARINE THUNDERSTORM WIND" | EVTYPE == "MARINE TSTM WIND"| EVTYPE == "THUNDERSTROM WIND" | EVTYPE == "TSTM WIND" | EVTYPE == "WATERSPOUT")
dim(precip_events)
head(precip_events)

# Re-formatting the date for readability
precip_events$BGN_DATE = str_sub(precip_events$BGN_DATE, -12)
head(flooding_events)
precip_events$BGN_DATE = str_sub(precip_events$BGN_DATE, 1, 4)
head(precip_events)
dim(precip_events)                                                                                    

# Calculating number of precip related events per year
precip_frequencies <- as.data.frame(table(precip_events$BGN_DATE))
names(precip_frequencies)[1] <- "Year"
names(precip_frequencies)[2] <- "Precip_Events"
precip_frequencies
head(precip_frequencies)

# Merging the flooding and precip tables
merged = merge(flooding_frequencies, precip_frequencies, by="Year")
merged

colors <- c("Floods" = "red", "Precip_Events" = "blue")

ggplot(merged, aes(x = Year)) +
  geom_line(aes(y = Floods, color = "Floods", group = 1), size = 1.5) +
  geom_line(aes(y = Precip_Events, color = "Precip_Events", group = 2), size = 1.5) +
  labs(title="Number of Floods and Precipitation-related Storms versus Time",
       y = "Frequency", x="Year",
       color = "Legend") +
  scale_color_manual(values = colors)
