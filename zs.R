#Used to estimate sMAPE
smape_cal <- function(outsample, forecasts){
  outsample <- as.numeric(outsample)
  forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts))/(abs(outsample)+abs(forecasts))
  return(smape)
}


# Univariate baseline solution using naive base 
require(dplyr)

# Load data
data<-read.csv("yds_train2018.csv", as.is = T)
submission <- read.csv("yds_test2018.csv", as.is = T)

# Roll data at monthly resolution
data_monthly <- data %>% group_by(S_No, Year, Month, Product_ID, Country) %>% summarise(monthly_sales=sum(Sales))

# Set-up forecasting
uniqCountry <-  unique(data_monthly$Country)
ydsnaiveSumission <- NULL
trainForecastlastpoint <- NULL
for(country in uniqCountry) {
  
  # Filter data based on country and product ID
  data_month_country <- data_monthly %>%
    select(S_No, Year, Month, Country, Product_ID, monthly_sales) %>% 
    filter(Country==country)
  
  # Unique Product
  uniqProduct <-  unique(data_monthly$Product_ID)                          
  
  for(product in uniqProduct){
    
    # Filter data based on country and product ID
    data_month_filterd <- data_month_country %>%
      select(S_No, Year, Month, Country, Product_ID, monthly_sales) %>% 
      filter(Product_ID==product)
    
    
    # Use Last point as forecast
    lastPointValue <- data_month_filterd$monthly_sales[length(data_month_filterd$monthly_sales)]
    
    # Forecast
    submission_filter<-submission %>%
      select(S_No, Year, Month, Country, Product_ID, Sales) %>% 
      filter(Country==country & Product_ID==product)
    horizon <- dim(submission_filter)[1]
    submission_filter$Sales<-lastPointValue
    ydsnaiveSumission <- rbind(ydsnaiveSumission, submission_filter)
    trainForecastlastpoint <- rbind(trainForecastlastpoint, data.frame(subset(data_month_filterd, select=-c(monthly_sales)), lastPointValue))
  }
}

# Test SMAPE Function
SMAPE_ERR <- mean(smape_cal(outsample=data_monthly$monthly_sales, forecasts=trainForecastlastpoint$lastPointValue))

# Write final submission
write.csv(ydsnaiveSumission, file="ydsnaivesubmission.csv", row.names = F)
