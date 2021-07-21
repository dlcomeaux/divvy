# Library loading

library(tidyverse)
library(lubridate)
library(slider)
library(cmapplot)

source("../mydailytravel/R/helper_fns.R")
#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

# Download data (only run once per machine, unless data needs to be updated)
divvy_zip <- tempfile()

divvy_urls <- 
  c("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q1.zip",
    "https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q2.zip",
    "https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q3.zip",
    "https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q4.zip",
    "https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2020_Q1.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202004-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202005-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202006-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202007-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202008-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202009-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202010-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202011-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202012-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202101-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202102-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202103-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202104-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202105-divvy-tripdata.zip",
    "https://divvy-tripdata.s3.amazonaws.com/202106-divvy-tripdata.zip"
    )

for (i in 1:length(divvy_urls)) {
  download.file(divvy_urls[i],divvy_zip,quiet = TRUE)
  unzip(divvy_zip,exdir = "data")
}

file.remove(divvy_zip)
rm(divvy_zip)

# Load data (every session unless workspace saved). Note that the variable names
# and values changed in 2020 Q1 to reflect e-bikes, remove gender, and change
# the values of user types

pre_2020_relevant_cols <- c(2,3,10)
pre_2020_names <- c("start_time","end_time","usertype")
post_2020_relevant_cols <- c(2,3,4,13)

divvy_2019 <-
  rbind(read.csv("data/Divvy_Trips_2019_Q1.csv") %>% select(pre_2020_relevant_cols),
        read.csv("data/Divvy_Trips_2019_Q2.csv") %>% select(pre_2020_relevant_cols) %>%
          # Fix column names, which are different only for this one for some reason
          'colnames<-' (pre_2020_names),
        read.csv("data/Divvy_Trips_2019_Q3.csv") %>% select(pre_2020_relevant_cols),
        read.csv("data/Divvy_Trips_2019_Q4.csv") %>% select(pre_2020_relevant_cols)) %>% 
  # Add value for bike type
  mutate(rideable_type = "docked_bike") %>% 
  # Rename column names to match post-2020 values
  rename("started_at" = "start_time",
         "ended_at" = "end_time",
         "member_casual" = "usertype") %>% 
  # Recode values from usertype to new system
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))
  
divvy_2020 <-
  rbind(read.csv("data/Divvy_Trips_2020_Q1.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202004-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202005-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202006-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202007-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202008-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202009-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202010-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202011-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202012-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols))
  
divvy_2021 <-
  rbind(read.csv("data/202101-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202102-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202103-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202104-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202105-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols),
        read.csv("data/202106-divvy-tripdata.csv") %>% select(!!post_2020_relevant_cols))




# Create helper values

# 3am day threshold (since our day starts at 3am)
threshold_divvy <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
# The number of seconds in a day (used to add days)
day_value <- 60*60*24

weekdays_2020 <- 
  sum(!weekdays(seq(as.Date("2020-01-01"),
                    as.Date("2020-12-31"),
                    "days")) %in% c("Saturday","Sunday"))

weekdays_2021 <-
  sum(!weekdays(seq(as.Date("2021-01-01"),
                    as.Date("2021-6-30"),
                    "days")) %in% c("Saturday","Sunday"))


divvy_cleaner <- function(data,years) {
# Clean and filter data for application of the TIM calculator function
  data %>%
  # Convert to datetime object
  mutate(start_time = ymd_hms(started_at, tz = "America/Chicago"),
         end_time = ymd_hms(ended_at, tz = "America/Chicago")) %>%
  mutate(trip_time = end_time - start_time) %>%
  # Exclude trips > 3 hours as outliers
  filter(trip_time <= 60 * 60 * 3) %>% # 3629175 records
  
  # Identify the day of the week of the trip using the `wday` function from the
  # lubridate package. Note that we subtract 3 hours from the day to use this
  # function since our days "begin" at 3am, i.e., a trip that starts at 2:35am
  # on a Saturday will be evaluated at 11:35pm the prior day, making it a
  # Friday.
  mutate(wday = wday(start_time - 3 * 60 * 60)) %>%
  # Create a day field for total trips by day
  mutate(day = floor_date(start_time - 3 * 60 * 60,"day")) %>% 
  # Keep out trips that are either Saturday (7) or Sunday (1)
  filter(!(wday %in% c(1,7))) %>% 
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_start = force_tz(ymd_hms(paste0("2020-01-01 ",
                                              substr(start_time,12,19))),
                               tzone = "America/Chicago")) %>%
  # Since we just forced all trips to start on the same day, but our days do not
  # begin at midnight, make trips that start before 3am into trips on the next
  # day.
  mutate(trip_start = case_when(
    trip_start < threshold_divvy ~ trip_start + day_value,
    TRUE ~ trip_start)
  ) %>%
  # Add trip end based on trip duration
  mutate(trip_end = trip_start + trip_time) %>%
  # Create trip interval using the lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Add weight of 1 divided by the number of weekdays for summing the average TIMs
  mutate(weight = 1/years)
}

divvy_20_wip <-
  divvy_cleaner(divvy_2020,weekdays_2020)

divvy_21_wip <-
  divvy_cleaner(divvy_2021,weekdays_2021)

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
# Plot of Divvy ridership
################################################################################

# Use function defined in helper_fns.R to create trips in motion graph
trip_times_divvy_counts_20 <-
  tim_calculator(
    data = divvy_20_wip,
    # weights = "weight",
    criteria = "member_casual")

trip_times_divvy_counts_21 <-
  tim_calculator(
    data = divvy_21_wip,
    weights = "weight",
    criteria = "member_casual")

# Define breaks
divvy_breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 06:00:00"),
                           to = as.POSIXct("2020-01-02 03:00:00"),
                           by = "6 hours")
# Create chart for 2020
divvy20_p1 <-
  # Get data
  trip_times_divvy_counts_20 %>%
  # Relevel user type
  mutate(member_casual = recode_factor(factor(member_casual, levels = c("member",
                                                              "casual")),
                                  "member" = "Subscriber",
                                  "casual" = "One-time user")) %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count, fill = member_casual)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  
  # Reformat axes
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = divvy_breaks) +
  # scale_y_continuous(limits = c(0,500),expand = expansion(mult = c(.05,.01))) +
  
  # Add CMAP style
  scale_fill_discrete(type = c("#475c66","#ac8c00")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Divvy trips")

# Export plot
finalize_plot(divvy20_p1,
              filename = "divvy20_p1",
              # sidebar_width = 0,
              # caption_align = 1,
              # mode = c("png","pdf"),
              # height = 2.25,
              # width = 8,
              # overrides = list(margin_plot_l = 30),
              overwrite = T)

# Create chart for 2021
divvy21_p1 <-
  # Get data
  trip_times_divvy_counts_21 %>%
  # Relevel user type
  mutate(member_casual = recode_factor(factor(member_casual, levels = c("member",
                                                                        "casual")),
                                       "member" = "Subscriber",
                                       "casual" = "One-time user")) %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count, fill = member_casual)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  
  # Reformat axes
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = divvy_breaks) +
  # scale_y_continuous(limits = c(0,500),expand = expansion(mult = c(.05,.01))) +
  
  # Add CMAP style
  scale_fill_discrete(type = c("#475c66","#ac8c00")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Divvy trips")

# Export plot
finalize_plot(divvy21_p1,
              filename = "divvy21_p1",
              # sidebar_width = 0,
              # caption_align = 1,
              # mode = c("png","pdf"),
              # height = 2.25,
              # width = 8,
              # overrides = list(margin_plot_l = 30),
              overwrite = T)
