install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
flight_delays <- read_excel("C:/Users/Sivaram/Downloads/1657873325_flightdelays.xlsx")
View(flight_delays)
colSums(is.na(flight_delays))

summary(flight_delays)

str(flight_delays)

# Plot the histograms to understand the relationships between scheduled time, 
# carrier, destination, origin, weather, and day of the week
install.packages("plotly")
library(plotly)
library(dplyr)

for (var in c("schedtime", "carrier", "dest", "origin", "weather", "dayweek")) {
  plot_ly(flight_delays, x = ~get(var), type = "histogram", nbinsx = 30) %>%
    layout(title = paste("Histogram of", var)) %>%
    print()
}

# Create the scatter plot for scheduled time in red
p <- plot_ly(data = flight_delays, x = ~schedtime, y = ~deptime, type = 'scatter', mode = 'markers', 
             marker = list(color = 'red'), name = 'Scheduled Time')
# Add the scatter plot for departure time in blue
p <- p %>% add_trace(x = ~schedtime, y = ~deptime, type = 'scatter', mode = 'markers', 
                     marker = list(color = 'blue'), name = 'Departure Time')
# Set the layout for the plot
p <- p %>% layout(title = "Scatter plot for flights based on scheduled and departure time",
                  xaxis = list(title = "Scheduled Time"),
                  yaxis = list(title = "Departure Time"))

# To display the plot
p

# Plot the box plot to understand how many days in a month flights are delayed by what time

plot_ly(flight_delays, x = ~as.factor(daymonth), y = ~deptime, color = ~delay, type = "box") %>%
  layout(title = "Box plot for flight delays by day of the month")
# Define the hours of departure

flight_delays$deptime_char <- as.character(flight_delays$deptime)


# Extract the hour portion
flight_delays$hour_of_departure <- as.numeric(substr(flight_delays$deptime_char, start=1, 
                                                     stop=nchar(flight_delays$deptime_char)-2))


# Create a categorical representation of data using a table
contingency_table <- table(flight_delays$carrier, flight_delays$delay)

contingency_table <- table(flight_delays$origin, flight_delays$delay)

# Redefine the delay variables
flight_delays$delay <- ifelse(flight_delays$delay == "ontime", 0, 1)

# Understand the summary of major variables
major_variables <- c('schedtime', 'deptime', 'distance', 'weather', 'dayweek', 'daymonth', 'delay')
summary(flight_delays[, major_variables])

# Plot histograms of major variables

for (var in c('schedtime', 'deptime', 'distance', 'weather', 'dayweek', 'daymonth', 'delay')) {
  p <- plot_ly(flight_delays, x = ~get(var), type = "histogram", nbinsx = 30) %>%
    layout(title = paste("Histogram of", var))
  print(p)
}
p

# Plot a pie chart to see how many flights were delayed
# Calculate the distribution of delays
delay_counts <- table(flight_delays$delay)

plot_ly(labels = c('On Time', 'Delayed'), values = delay_counts, type = 'pie') %>%
  layout(title = "Distribution of Flight Delays")

