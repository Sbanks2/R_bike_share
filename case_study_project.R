library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)


#Setting the directory I will be working in
getwd()
setwd('C:/Users/insan/OneDrive/Documents/data_analyst_professional_certificate/capstone_project/2021_case_study_files')

#Compiling the 12 csv files into one file
combined_csv <- list.files(pattern = "\\.csv$", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

colnames(combined_csv)

#Removing all of the N/A
all_data <- na.omit(combined_csv)

#Removing columns that I won't need at this time
all_data <- select(all_data, -c(start_lat, start_lng, end_lat, end_lng, ride_id))

#Removing data that was related to maintenance and not rides
all_data_clean <- subset(all_data,
                         !start_station_name == "HQ QR"&
                           !difftime(ended_at, started_at) <0) #!difftime(ended_at, started_at) > 24*60*60)
                           


#Checking the structures of my data
str(all_data_clean)

#Adding additional columns I think I'll need for my analysis
all_data_clean <- all_data_clean %>%
  mutate(start_station_end_station = paste(start_station_name, end_station_name, sep = " to "),
         start_id_end_id = paste(start_station_id, end_station_id, sep = " to "),
         ride_duration = difftime(ended_at, started_at),
         ride_duration_minutes = as.numeric((ride_duration), units = "mins"),
         date = as.Date(started_at),
         month = format(date, format = "%b"),
         day = format(as.Date(date), "%d"),
         year = format(as.Date(date), "%Y"),
         day_of_week = format(as.Date(date), "%A"),
         weekday = wday(started_at, label = TRUE))


#Checking the summary of these columns of data
summary(all_data_clean$ride_duration_minutes)
summary(all_data_clean$rideable_type)

#Reviewing the summary mean and summaries and various relationships, dependent is ride duration in minutes and independent variables are the member types and days of the week
aggregate(all_data_clean$ride_duration_minutes ~ all_data_clean$member_casual, FUN = mean)
aggregate(all_data_clean$ride_duration_minutes ~ all_data_clean$member_casual, FUN = summary)
aggregate(all_data_clean$ride_duration_minutes ~ all_data_clean$member_casual + all_data_clean$day_of_week, FUN = summary)

result <- all_data_clean %>%
  group_by(member_casual) %>%
  summarize(count = sum(ride_duration_minutes > 2*60))
result

total_duration_count <- all_data_clean %>%
  group_by(member_casual) %>%
  summarize(total_count = n())
total_duration_count


outlier_percentage <- left_join(result, total_duration_count, by = "member_casual")
outlier_percentage <- outlier_percentage %>%
  mutate(percentage = (count / total_count) * 100)

outlier_percentage

total_ride_count <- all_data_clean %>%
  group_by(member_casual, rideable_type) %>%
  summarize(ride_count = n())

total_ride_count_by_member <- total_ride_count %>%
  group_by(member_casual) %>%
  summarize(total_ride_count = sum(ride_count))

bike_use_percentage <- total_ride_count %>%
  left_join(total_ride_count_by_member, by = "member_casual") %>%
  mutate(bike_percentage = ((ride_count / total_ride_count) * 100),
         bike_percentage = round(bike_percentage, 2))

#Here I'm ordering the days of the weeks and the months so my visualizations are organized
all_data_clean$day_of_week <- ordered(all_data_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
all_data_clean$month <- ordered(all_data_clean$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Charting the number of rides by member type and day of the week
all_data_clean %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n()/1000,
            median_duration = median(ride_duration_minutes)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Number of Rides by Member Type & Day of the Week", x = "Day of the Week", y = "Number of Rides (thousands)", fill = "Member Type")

#Charting the average ride duration by member type and day of the week
all_data_clean %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(),
            median_duration = median(ride_duration_minutes)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = median_duration, fill = member_casual)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Median Ride Duration by Member Type & Day of the Week", x = "Day of the Week", y = "Median Ride Duration (minutes)", fill = "Member Type")


#Charting the number of rides by member type and month
all_data_clean %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n()/1000,
            median_duration = median(ride_duration_minutes)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Number of Rides by Member Type & Month", x = "Day of the Week", y = "Number of Rides (thousands)", fill = "Member Type")


#Charting the average ride duration by member type and month
all_data_clean %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n(),
            median_duration = median(ride_duration_minutes)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = median_duration, fill = member_casual)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Median Ride Duration by Member Type & Month", x = "Month", y = "Median Ride Duration (minutes)", fill = "Member Type")


#Charting the number of rides by rideable type (type of bike) and by member type
all_data_clean %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, rideable_type) %>%
  summarize(number_of_rides = n()/1000) %>%
  arrange(member_casual, rideable_type) %>%
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Number of Rides by Bike & Member Type", x = "Bike Types", y = "Number of Rides (thousands)", fill = "Member Type")  # Customize chart labels


str(bike_use_percentage$bike_percentage)
#Trying to convert into a percentage pie chart with side-by-side comparison
bike_use_percentage %>%
  ggplot(aes(x = "", y = bike_percentage, fill = factor(rideable_type))) +
  geom_bar(stat = "identity", width = 1) + 
  geom_text(aes(label = paste0(bike_percentage, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Percentage of Rides by Bike & Member Type", fill = "Bike Type") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  facet_wrap(~member_casual)



counts <- aggregate(all_data_clean$ride_duration_minutes ~ all_data_clean$member_casual +
                      all_data_clean$day_of_week, FUN = mean)

write.csv(counts, file = 'C:/Users/insan/OneDrive/Documents/data_analyst_professional_certificate/capstone_project/case_study_summary_files/avg_ride_lengths.csv')
