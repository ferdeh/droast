library(dplyr)
library(plotly) 
#library(hms)
#read data
data <- read.csv('https://raw.githubusercontent.com/ferdeh/droast/master/Report%20Item%20Details.csv')
head(data)

#delete unnecesary colomn 
data<- select(data,Receipt.Number, Date, Time,Category,Items,Variant,Gross.Sales)
#reformat date
data$Date <- as.Date(data$Date,format = "%d-%m-%y")
data$day <-weekdays(data$Date) %>% factor( levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)
data$daynum <- as.integer(data$day)
#reformat Time
data$Time <- strptime(data$Time, "%H:%M:%S") %>% format( "%H:%M:%S")
data$Timenum <- strptime(data$Time, "%H:%M:%S") 
data$Timenum  <-format(data$Timenum, "%H") %>% as.integer()
#delete 0 gross
data<-filter(data,Gross.Sales>0)
data<-data[order(as.Date(data$Date)),]


#Category Uncategorized
index<- which(data$Category == 'Uncategorized')
data$Category<-as.character(data$Category)
data$Category[index]<-as.character(data$Items[index])
data$Category <-as.factor(data$Category)
data$Category

#Variant
index<- which(data$Variant == '')
data$Variant<-as.character(data$Variant)
data$Variant[index]<-as.character(data$Items[index])
data$Variant <-as.factor(data$Variant)

#cummulative gross
data$cumm <- cumsum(data$Gross.Sales)

#data exploration


p <- plot_ly(data, x = ~daynum, y = ~Timenum, z = ~Gross.Sales) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'day'),
                      yaxis = list(title = 'time'),
                      zaxis = list(title = 'gross')))

p

plot(density(data$daynum))

plot(density(data$Timenum))

hist(data$daynum)

hist(data$Timenum)
names(data)
x.scale <- as.data.frame(scale(data[,c(7,9,10)], scale = TRUE))
data$Gross_z<-x.scale$Gross.Sales
data$daynum_z <-x.scale$daynum
data$Timenum_z <-x.scale$Timenum
data

p <- plot_ly(data, x = ~daynum_z, y = ~Timenum_z, z = ~Gross_z) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'day'),
                      yaxis = list(title = 'time'),
                      zaxis = list(title = 'gross')))

p

plot(density(data$daynum_z))

plot(density(data$Timenum_z))

hist(data$daynum)

hist(data$Timenum)



head(data)
data_ayam<- filter(data,Category == 'Ayam')
nrow(data_ayam)
nrow(data)
data_ayam_count<-aggregate(data_ayam$Receipt.Number, by=list(day=data_ayam$daynum,hour=data_ayam$Timenum), FUN=length)
head(data_ayam_count)

p <- plot_ly(data_ayam_count, x = ~day, y = ~hour, z = ~x) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'day'),
                      yaxis = list(title = 'time'),
                      zaxis = list(title = 'count')))

p

plot(density(data_ayam_count$x))


data_ayam_count
library(corrplot)
library(factoextra)

df_cor<-cor(data_ayam_count)
df_cor

corrplot(df_cor, type = "upper", order = "original", 
         tl.col = "black")

#### Elbow method
fviz_nbclust(data_ayam_count[,c("hour","x")], kmeans, method = "wss") +
  geom_vline(xintercept = 8, linetype = 2)+
  labs(subtitle = "Elbow method")


##### Silhouette method

fviz_nbclust(data_ayam_count[,c("hour","x")], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#### Kedua metode menghasilkan jumlah K optimal k=2


#### Membuat model k-mean
km.out <- kmeans(data_ayam_count[,c("hour","x")], center=3, nstart=10)
data_ayam_count$cluster <- km.out$cluster

#### Plot hasil clustering
plot(data_ayam_count[,c("hour","x")], col = data_ayam_count$cluster,
     main = "K-MEANS of RFM")
head(df_encode)





day<- c(1,1,1)
hour<-c(1,1,3)
invoice<-c('a','a','b')

df <- data.frame(day,hour,invoice)
df

aggregate(df$invoice, by=list(day=df$day,hour=df$hour), FUN=length)

data$cumm <- cumsum(data$Gross.Sales)
head(data)
plot(data$cumm,data$Date)
max(data$Gross.Sales)

temp<-aggregate(data$cumm, by=list(day=data$Date), FUN=last)
temp
plot(temp$x,temp$day)
temp[which(temp$x == max(temp$x)),]
data$Date

data2<-data[order(as.Date(data$Date, format="%d/%m/%Y")),]
data2
data2$cumm <- cumsum(data2$Gross.Sales)
plot(data2$cumm,data2$Date)

temp<-aggregate(data2$cumm, by=list(day=data2$Date), FUN=last)
temp
max(diff(temp$x))
a<-c(1,2,3,5,8,9)
diff(a)

library(plotly)

p <- plot_ly(data2, x = ~Date, y = ~cumm) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency')
                      ))

p

