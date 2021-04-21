##Working on Case Study 1:

## Cyclist Exercise Full Year Analysis ##

# Importing all the required libraries

library(tidyverse)
library(lubridate)
library(ggplot2)

q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


## Checking for colnames in each file so we can club them in one big file

colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)


## Renaming the colnames in Q3_2019 and Q4_2019 to match with Q1_2020

(q4_2019 <- rename(q4_2019,
                   "ride_id" = "trip_id",
                   "rideable_type" = "bikeid",
                   "started_at" = "start_time",
                   "ended_at" = "end_time",
                   "start_station_name" = "from_station_name",
                   "start_station_id" = "from_station_id",
                   "end_station_name" = "to_station_name",
                   "end_station_id" = "to_station_id",
                   "member_casual" = "usertype"))

(q3_2019 <- rename(q3_2019,
                   "ride_id" = "trip_id",
                   "rideable_type" = "bikeid",
                   "started_at" = "start_time",
                   "ended_at" = "end_time",
                   "start_station_name" = "from_station_name",
                   "start_station_id" = "from_station_id",
                   "end_station_name" = "to_station_name",
                   "end_station_id" = "to_station_id",
                   "member_casual" = "usertype"))


## Checking for data type in each files


str(q1_2020)
str(q3_2019)
str(q4_2019)


## Changing ride_id and rideable_type to characters so they can be stacked correctly

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q4_2019 = mutate(q4_2019, ride_id = as.character(ride_id),
                 rideable_type = as.character(rideable_type))


## Stacking individual quarters dataframe into one bug data frame

all_trips <- bind_rows(q3_2019,q4_2019,q1_2020)

## Removing all the other columns which were not mapped 

all_trips <- all_trips %>% 
  select(-c("start_lat","start_lng","end_lat","end_lng","birthyear","gender","tripduration"))



## Checking all the colnames  and other basic details in the new dataframe

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
tail(all_trips)


# We observed that Member casual column has 4 inputs - Casual, Customer, Member, Subscriber, We will change Subscriber to Memeber and Customer to casual
table(all_trips$member_casual)

# Reassiging to the desired values
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

table(all_trips$member_casual) # Rechecking if the changes have taken effect or not


## Adding a column that list the date, month, day and year of each ride - This will help us to aggregrate the data

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

str(all_trips)

# Adding a ride length field in the data frame, which is the calculation of all trips in seconds

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)
all_trips$ride_length_minutes <- all_trips$ride_length / 60
str(all_trips)
all_trips$ride_length_hours <- all_trips$ride_length_minutes / 60
str(all_trips)


## checking whether our new columns are factors if yes then we would need to convert them into num

is.factor(all_trips$ride_length)
is.factor(all_trips$ride_length_minutes)
is.factor(all_trips$ride_length_hours)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
all_trips$ride_length_minutes <- as.numeric(as.character(all_trips$ride_length_minutes))
all_trips$ride_length_hours <- as.numeric(as.character(all_trips$ride_length_hours))

is.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length_minutes)
is.numeric(all_trips$ride_length_hours)


## In our data we have some "bad" data - The dataframe includes few hundred entries when bikes were taken out of docks and checking for quality
## these numbers are in negative

# Now we will create another dataframe as we are removing the data from main data frame

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]



### Conducting descriptive analysis on the data

summary(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length_minutes)
summary(all_trips_v2$ride_length_hours)


###Compaing members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

### Seeing the average ride time by each day of the week for member vs Casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean) # days are not in order

## lets fix day to show in order

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

## Now lets re-run the aggregrate command to see how avg ride time by each day of the week for member vs casual users looks like

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


## Now lets review ridership data by weekday

all_trips_v2 <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% # Creates weekday field using wday()
  group_by(member_casual, weekday) %>% # groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% # Calculates the average duration
  arrange(member_casual, weekday) # sorts




## Lets visualize the data - Number of rides by rider type

ggplot(data = all_trips_v2, aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

ggplot(data = all_trips_v2, aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")


## Lets Extract this v2 file for further visualization in Excel or Tableau

counts <- aggregate(all_trips_v2$number_of_rides ~ all_trips_v2$member_casual + all_trips_v2$weekday, FUN = mean)

write.csv(counts, file = "all_trips.csv")

















