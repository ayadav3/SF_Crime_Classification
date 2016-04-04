#-----------------------------------SF Crime Classification----------------------#
# libraries
library(ggmap)
library(ggplot2)
library(dplyr)
install.packages('RgoogleMaps')
library(RgoogleMaps)
# Read data

train_data <- read.csv("../data/train.csv")
test_data <- read.csv("../data/test.csv")

# plotting crime type on SF map

# map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")
# 
# map_crime <- function(crime_df, crime) {
#   filtered <- filter(crime_df, Category %in% crime)
#   plot <- ggmap(map, extent='device') + 
#     geom_point(data=filtered, aes(x=X, y=Y, color=Category), alpha=0.6)
#   return(plot)
# }
# 
# map_crime(train, c('SUICIDE', 'ARSON'))

summary(train_data)

# functions to make variables from date info.

make_vars_date <- function(crime_df) {
  crime_df$Years = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"),"%Y")
  crime_df$Month = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"),"%m")
  crime_df$DayOfMonth = strftime(strptime(crime_df$Dates,
                                          "%Y-%m-%d %H:%M:%S"),"%d")
  crime_df$Hour = strftime(strptime(crime_df$Dates,
                                    "%Y-%m-%d %H:%M:%S"),"%H")
  crime_df$YearsMo = paste( crime_df$Years, crime_df$Month , 
                            sep = "-" )
  
  
  
  crime_df$DayOfWeek = factor(crime_df$DayOfWeek,
                              levels=c("Monday","Tuesday",
                                       "Wednesday","Thursday",
                                       "Friday","Saturday","Sunday"),
                              ordered=TRUE)
  
  
  crime_df$weekday = "Weekday"
  crime_df$weekday[crime_df$DayOfWeek== "Saturday" | 
                     crime_df$DayOfWeek== "Sunday" | 
                     crime_df$DayOfWeek== "Friday" ] = "Weekend"
  
  
  addr_spl = strsplit(as.character(crime_df$Address),"/")
  crime_df$AddressType = "Non-Intersection"
  ind_l = vector()
  ind_inxn = sapply(1:dim(crime_df)[1], 
                    function(x) length(addr_spl[[x]]) == 2)
  crime_df$AddressType[ ind_inxn ]="Intersection"
  
  
  
  return(crime_df)
}

train_data = make_vars_date(train_data)
test_data = make_vars_date(test_data)
# data_train_ss <- sample_n(train_data,10000)

# Types of crime

data_plot = train_data %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  transform(Category = reorder(Category,-count))

ggplot(data_plot) + 
  geom_bar(aes(x=Category, y=count, 
               color = Category, fill = Category),
           stat="identity")+
  coord_flip()+
  theme(legend.position="None")+
  ggtitle("Number of crimes in individual category")+
  xlab("Number of crimes")+
  ylab("Category of crime")

