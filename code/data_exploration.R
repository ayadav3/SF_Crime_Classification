#-----------------------------------SF Crime Classification----------------------#
# libraries
library(ggmap)
library(ggplot2)
library(dplyr)
install.packages('RgoogleMaps')
library(RgoogleMaps)
install.packages('LiblineaR')
library(LiblineaR)
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

data_plot = data_plot[with(data_plot,order(-count)),]
Top_crimes = data_plot

print("Top 10 crimes")
top_10 = Top_crimes[1:10,1]


data_plot = train_data %>%
  subset(Category %in% top_10) %>%
  group_by(Years,Category,Month) %>%
  summarise(count = n()) 

data_plot$Category = factor(data_plot$Category,levels = top_10)

ggplot(data = data_plot,aes(x=Years, y=count,fill = Category)) + 
  geom_boxplot() + 
  facet_wrap(~Category,ncol = 5)+
  theme(legend.position="None",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Year")+
  ylab("Number of crime incidents")+
  ggtitle("Variations in crime by year")+
  xlab("Number of crimes")+
  ylab("Year")


months_name = c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec")


data_plot = train_data %>%
  group_by(DayOfWeek,Years,Month,YearsMo) %>%
  summarise(count = n()) 
p1 = ggplot(data = data_plot,aes(x=DayOfWeek, y=count,fill = DayOfWeek)) + 
  geom_boxplot() + 
  theme(legend.position="None") +
  xlab("Day of week")+
  ylab("Number of crime incidents")+
  coord_cartesian(ylim = c(300,1200))

# Crime vs month
data_plot = train_data %>%
  group_by(Month,Years,Month,YearsMo) %>%
  summarise(count = n()) 
p2 = ggplot(data = data_plot,aes(x=as.numeric(Month), y=count,fill = Month)) + 
  geom_boxplot() + 
  xlab("Month")+
  ylab("Number of crime incidents")+
  theme(legend.position="None") +
  scale_x_continuous(breaks = 1:12, labels=months_name)


# Crime vs hour of the day
data_plot = train_data %>%
  group_by(Hour,Years,Month,YearsMo) %>%
  summarise(count = n()) 
p3 = ggplot(data = data_plot,aes(x=Hour, y=count,fill = Hour)) + 
  geom_boxplot() + 
  xlab("Hour of day")+
  ylab("Number of crime incidents")+
  theme(legend.position="None")
p3


months_name = c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec")

# Types of crime vs Month

top_10 = Top_crimes[1:10,1]

data_plot = train_data %>%
  group_by(Category,Month) %>%
  summarise(count = n()) %>%
  mutate(norm_count = (count-mean(count))/sd(count)) 
p1 = ggplot(data = subset(data_plot, Category %in% top_10),
            aes(x=as.numeric(Month), y=count,color = Category)) + 
  geom_line()+
  geom_point()+
  scale_x_discrete(breaks = 1:12, labels=c("Jan","Feb","Mar",
                                           "Apr","May","Jun",
                                           "Jul","Aug","Sep",
                                           "Oct","Nov","Dec")) +
  xlab("Months")+
  ylab("Crime count") + 
  theme(legend.position="Bottom")

p2 = ggplot(data = subset(data_plot, Category %in% top_10),
            aes(x=as.numeric(Month), y=norm_count,color = Category)) + 
  geom_line()+
  geom_point()+
  scale_x_discrete(breaks = 1:12, labels=c("Jan","Feb","Mar",
                                           "Apr","May","Jun",
                                           "Jul","Aug","Sep",
                                           "Oct","Nov","Dec")) +
  xlab("Months")+
  ylab("Normalized crime count")+ 
  theme(legend.position="None")

grid.arrange(p1,p2,ncol = 1,top = "Normalizing by month reveals common patterns in data")



top_10 = Top_crimes[1:10,1]

data_plot = train_data %>%
  group_by(Category,Hour) %>%
  summarise(count = n()) %>%
  mutate(norm_count = (count-mean(count))/sd(count)) 
p1 = ggplot(data = subset(data_plot, Category %in% top_10),
            aes(x=as.numeric(Hour), y=count,color = Category)) +
  geom_line()+
  geom_point() + 
  xlab("Hour of Day") +
  ylab("Crime count") + 
  theme(legend.position="None")




p2 = ggplot(data = subset(data_plot, Category %in% top_10),
            aes(x=as.numeric(Hour), y=norm_count,color = Category)) +
  geom_line()+
  geom_point() + 
  xlab("Hour of Day") +
  ylab("Nomalized crime count") + 
  theme(legend.position="None")



####

## MAKING TRAINING FACTORS 

make_training_factors = function(df) {
  df$Years=paste("Yr",df$Years,sep = ".")
  df$Years = factor(df$Years)
  y <- as.data.frame(model.matrix(~df$Years - 1))
  names(y) <- levels(df$Years)
  
  df$Hour=paste("Hr",df$Hour,sep = ".")
  df$Hour = factor(df$Hour)
  h <- as.data.frame(model.matrix(~df$Hour - 1))
  names(h) <- levels(df$Hour)
  
  dow <- as.data.frame(model.matrix(~df$DayOfWeek - 1))
  names(dow) <- levels(df$DayOfWeek)
  
  df$Month=paste("Mon",df$Month,sep = ".")
  df$Month = factor(df$Month)
  m <- as.data.frame(model.matrix(~df$Month-1))
  names(m) <- levels(df$Month)
  head(m)
  
  district <- as.data.frame(model.matrix(~df$PdDistrict - 1))
  names(district) <- levels(df$PdDistrict)
  
  df$pY=paste(df$PdDistrict,df$Years,sep = ".")
  df$pY = factor(df$pY)
  pY <- as.data.frame(model.matrix(~df$pY - 1))
  names(pY) <- levels(df$pY)
  #training set
  train <- data.frame( y,dow, h, district, m,pY)
  return(train)
  
  
}


MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}

```



### TRAINING 

Data is fit with hour, day of week, month, year and police district as independent features.
Below is the code to generate an output file submit for submission. 
This is work in progress, and I will test more complex algorithms later. 
This code got me in early 400s, with a log loss about 2.6. 






```{r echo=TRUE, message=FALSE, warning=FALSE}
## TRAINING WITH DATA FROM 2012 ONLY. 
set.seed(22)
## used this to generate sample and test set to test model
#        smp_size <- floor(0.5 * nrow(data_train)) # splitting data
#        train_ind <- sample(seq_len(nrow(data_train)), size = smp_size)
#        train <- data_train[train_ind, ]
#        test <- data_train[-train_ind, ]


train = train_data 
target <- train$Category
#length(unique(target))
train = make_training_factors(train)
#dim(train)
head(train)

gc()

#Build a linear model
model <- LiblineaR(train, target, type = 7, verbose = FALSE)
rm(train)
gc()

##model

#Prepare test set.
test <- test_data
Id <- test$Id
test = make_training_factors(test)
head(test)

#Cleanup
gc()

#Build submission.
submit <- data.frame(predict(model,test, proba = TRUE)$probabilities[, levels(target)])\
submit$Id <- seq.int(0,nrow(submit)-1)
colnames(submit) <- c("ARSON","ASSAULT","BAD CHECKS","BRIBERY","BURGLARY","DISORDERLY CONDUCT","DRIVING UNDER THE INFLUENCE","DRUG/NARCOTIC","DRUNKENNESS","EMBEZZLEMENT","EXTORTION","FAMILY OFFENSES","FORGERY/COUNTERFEITING","FRAUD","GAMBLING","KIDNAPPING","LARCENY/THEFT","LIQUOR LAWS","LOITERING","MISSING PERSON","NON-CRIMINAL","OTHER OFFENSES","PORNOGRAPHY/OBSCENE MAT","PROSTITUTION","RECOVERED VEHICLE","ROBBERY","RUNAWAY","SECONDARY CODES","SEX OFFENSES FORCIBLE","SEX OFFENSES NON FORCIBLE","STOLEN PROPERTY","SUICIDE","SUSPICIOUS OCC","TREA","TRESPASS","VANDALISM","VEHICLE THEFT","WARRANTS","WEAPON LAWS","Id")
write.csv(submit, "submission.csv")
