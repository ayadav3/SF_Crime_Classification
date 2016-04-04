
# Read data

train_data <- read.csv("../data/train.csv")
test_data <- read.csv("../data/test.csv")

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
  
  Category <- as.data.frame(with(df, model.matrix(~Category+0)))

  #training set
  train <- data.frame(Category,y,dow, h, district, m,pY, as.data.frame(df$Intersection),as.data.frame(df$Night), as.data.frame(df$Week))
  return(train)
  
  
}



train <- train_data 
target <- train$Category
#length(unique(target))
train$Intersection<-grepl("/", train$Address)
train$Intersection<-plyr::mapvalues(train$Intersection,from=c("TRUE","FALSE"),to=c(1,0))

train$Night<-ifelse(train$Hour > 22 | train$Hour < 6,1,0)
train$Week<-ifelse(train$DayOfWeek=="Saturday" | train$DayOfWeek=="Sunday",0,1)
train = make_training_factors(train)
#dim(train)
head(train)



# categoryMatrix<-data.frame(with(train,model.matrix(~Category+0)))
# names(categoryMatrix)<-sort(unique(train$Category))
# train<-cbind(categoryMatrix,train)


train.tr.index<-sample(1:nrow(train),0.7*nrow(train))
train.tr<-train[train.tr.index,]
train.test<-train[-train.tr.index,]

matMod.tr <- as.matrix(train.tr[,40:238])
matMod.test <- as.matrix(train.test[,40:238])
#glmnet
m<-glmnet(matMod.tr,train.tr[,1],family="binomial")
pred<-as.data.frame(predict(m,matMod.test,s=1e-15,type="response"))
#numCat<-length(unique(train.tr$Category))
pb <- txtProgressBar(min = 1, max = 39, style = 3)
for (i in 2:39) {
  m<-glmnet(matMod.tr,train.tr[,i],family="binomial")
  pred<-cbind(pred,predict(m,matMod.test,s=1e-15,type="response"))
  setTxtProgressBar(pb, i)
}

test <- test_data
Id <- test$Id
test$Intersection<-grepl("/",test$Address)
test$Intersection<-plyr::mapvalues(test$Intersection,from=c("TRUE","FALSE"),to=c(1,0))

test$Night<-ifelse(test$Hour > 22 | train$Hour < 6,1,0)
test$Week<-ifelse(test$DayOfWeek=="Saturday" | test$DayOfWeek=="Sunday",0,1)
test = make_training_factors(test)
head(test)

