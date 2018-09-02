library("dplyr")
library("ggplot2")
library("RColorBrewer")
#library("keras")
library("forecast")

d_test <- read.csv(file = "./dataset/yds_test2018.csv", header = T)
d_train <- read.csv(file = "./dataset/yds_train2018.csv", header = T)
d_train_aggr <- d_train %>% 
  select(-Merchant_ID) %>%
  group_by(Year, Month, Product_ID, Country) %>%
  summarise(Sales = sum(Sales)) %>%
  arrange(Product_ID, Country, Year, Month)

d_train_aggr$Dt <- as.Date(paste(d_train_aggr$Year, d_train_aggr$Month, 1, sep="-"))

ggplot(d_train_aggr) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) + 
  scale_x_date() +
  scale_color_manual(values = brewer.pal(length(unique(d_train_aggr$Year)), "Dark2")) + 
  labs(colour="Year") + 
  facet_grid(Country~Product_ID)


ggplot(d_train_aggr %>% filter(Country=="Argentina")) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) + 
  scale_x_date() +
  scale_color_manual(values = brewer.pal(4, "Dark2")) + 
  labs(colour="Year", title = "Argentina") + 
  facet_wrap(~Product_ID)

ggplot(d_train_aggr %>% filter(Country=="Belgium")) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) +
  scale_x_date() +
  scale_color_manual(values = brewer.pal(4, "Dark2")) + 
  labs(colour="Year", title = "Belgium") + 
  facet_wrap(~Product_ID)


ggplot(d_train_aggr %>% filter(Country=="Columbia")) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) + 
  scale_x_date() +
  scale_color_manual(values = brewer.pal(4, "Dark2")) + 
  labs(colour="Year", title = "Columbia") + 
  facet_wrap(~Product_ID)

ggplot(d_train_aggr %>% filter(Country=="Denmark")) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) + 
  scale_color_manual(values = brewer.pal(4, "Dark2")) + 
  labs(colour="Year", title = "Denmark") + 
  facet_wrap(~Product_ID)

ggplot(d_train_aggr %>% filter(Country=="England")) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) + 
  scale_x_date() +
  scale_color_manual(values = brewer.pal(4, "Dark2")) + 
  labs(colour="Year", title = "England") + 
  facet_wrap(~Product_ID)

ggplot(d_train_aggr %>% filter(Country=="Finland")) +
  geom_line(aes(x=Dt, y=Sales, colour=as.factor(Year)), size=1) +
  scale_x_date() +
  scale_color_manual(values = brewer.pal(4, "Dark2")) + 
  labs(colour="Year", title = "Finland") + 
  facet_wrap(~Product_ID)

for(country in unique(d_train_aggr$Country)){
  for(productId in unique(d_train_aggr$Product_ID)){
    
    d_train_country_product <- d_train_aggr %>% 
      filter(Country==country, Product_ID==productId)
    
    if(nrow(d_train_country_product)>0){
      
      ts_country_product <- ts(
        d_train_country_product$Sales, 
        start=c(d_train_country_product$Year[1],d_train_country_product$Month[1]), 
        end=c(d_train_country_product$Year[length(d_train_country_product$Year)],d_train_country_product$Month[length(d_train_country_product$Month)]), 
        frequency = 12)
      
      fit_country_product <- HoltWinters(ts_country_product)
      forcast_country_product <- forecast(fit_country_product, nrow(d_test[d_test$Country==country & d_test$Product_ID==productId,]))
      plot(forcast_country_product, main=paste("forecast for", country, ", and Product", productId))
      d_test[d_test$Country==country & d_test$Product_ID==productId,"Sales"] <- forcast_country_product$mean # * ifelse(forcast_country_product$mean>0,1,0)
    }
  }
}

write.csv(d_test,file = "./dataset/yds_prediction2018.csv")

# actual <- c()
# predicted <- c()
# 
# for(country in unique(d_train_aggr$Country)){
#   for(productId in unique(d_train_aggr$Product_ID)){
#     
#     d_train_country_product <- d_train_aggr %>% 
#       filter(Country==country, Product_ID==productId)
#     
#     if(nrow(d_train_country_product)>0){
#       
#       total <- nrow(d_train_country_product)
#       num_train <- ceiling(total*0.9)
#       num_val <- total-num_train
#       
#       ts_country_product <- ts(
#         d_train_country_product$Sales, 
#         start=c(d_train_country_product$Year[1],d_train_country_product$Month[1]), 
#         end=c(d_train_country_product$Year[num_train],d_train_country_product$Month[num_train]), 
#         frequency = 12)
#       
#       fit_country_product <- HoltWinters(ts_country_product)
#       forcast_country_product <- forecast(fit_country_product, num_val)
#       actual <- c(actual, d_train_country_product[num_val:total, "Sales"])
#       predicted <- c(predicted, forcast_country_product$mean)
#     }
#   }
# }
# 
# smape <- function(actual, forcast){
#   mean(2*abs(forcast-actual)/(abs(actual)+abs(forcast)))
# }
# 
# print(paste("sMAPE score", smape(actual, predicted)))