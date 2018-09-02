library("dplyr")
library("ggplot2")
library("RColorBrewer")
#library("keras")
library("forecast")

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

#HoltWinter's Method

##Argentina_Pdt1_31-8 Months

d_train_argentia_p1 <- d_train_aggr %>% filter(Country=="Argentina", Product_ID==1)
ts_argentia_p1 <- ts(d_train_argentia_p1$Sales[1:32], start=c(2013,1), end=c(2015,7), frequency = 12)
ts_argentia_p1
fit_argentian_p1 <- HoltWinters(ts_argentia_p1)
plot(forecast(fit_argentian_p1, 8))
mean(smape_cal(d_train_argentia_p1$Sales[32:39], f_a_p1$mean))