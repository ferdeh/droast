library(dplyr)
#library(hms)

data <- read.csv('Report Item Details.csv')
head(data)
#data<- as.data.frame(data,stringsAsFactors = FALSE)
data<- select(data,Receipt.Number, Date, Time,Category,Items,Variant,Gross.Sales)
data$Date <- as.Date(data$Date,format = "%d-%m-%y")
data$day <-weekdays(data$Date) %>% factor( levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)
data$daynum <- as.integer(data$day)

data$Time <- strptime(data$Time, "%H:%M:%S") %>% format( "%H:%M:%S")
data$Timenum <- strptime(data$Time, "%H:%M:%S") %>% format( "%H") %>% as.factor()
data<-filter(data,Gross.Sales>0)

str(data)
index<- which(data$Category == 'Uncategorized')

data$Category[index]<-data$Items[index]

data$Category
index<- which(data2$Category == 'Uncategorized')

length(which(data$Variant==''))
nrow(data)
data[which(data$Variant==''),]

nrow(filter(data,Items =='Ricebowl'))
