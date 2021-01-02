
```{r}
setwd("C:/Users/acer/Desktop/Data_Science_project")
library(xml2)
library(rvest)
library(xml2)
library(XML)
library(rvest)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyquant)
#install.packages("quantmod")
#install.packages("TTR")
library(quantmod)
library(TTR)
library(tidyquant)
library(timetk)
setwd("C:/Users/acer/Desktop/Data_Science_project")
```


## Import Tickers
```{r}
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
```

```{r}
sp500 <- read_html(sp500_url) %>% 
  html_node("table") %>% 
  html_table()

sp500<-sp500 %>% select('Symbol','Security','SEC filings','GICS Sector', 'Headquarters Location')
names(sp500) <- c("Ticker", "Name", "Sector", "Industry", "HQ_Location")

save(sp500, file = "sp500.RData")
```
# importation of price Data sp&500
```{r}
sp500_price<-read.csv("^GSPC.csv")
sp500_price<-sp500_price %>% 
  mutate(Date=as.Date(Date))
sp500_price <-sp500_price %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
)
names(sp500_price) <- c("Date","Adjusted","Volume","Close","Open","High","Low","Mouvement")
sp500_price
View(sp500_price)
```
#jointure de returns avec sp500_price
```{r}
stock_join<-inner_join(returns,sp500_price,by = c("Date" = "Date"), suffix = c("_stock", "_sp500_price"))
View(stock_join)
```
## Import Price Data
```{r}
returns <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adjusted", "Volume", "Ticker")

for(symbol in sp500$Ticker){
  print(symbol)
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol, "?period1=1280102400&period2=1595721600&interval=1d&events=history")
  print(url)
  ret <- try(read_csv(url))
  
  if(mode(ret) != "character"){
    ret$Ticker <- symbol
    returns <- rbind(returns, ret)
  }
  
}
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adjusted", "Volume", "Ticker")
returns<- returns %>%  select("Date","Ticker", "Open", "High", "Low", "Close", "Adjusted", "Volume")
returns <- returns %>% mutate(
  Open = as.numeric(Open),
  High = as.numeric(High),
  Low = as.numeric(Low),
  Close = as.numeric(Close),
  Adjusted= as.numeric(Adjusted),
  Volume=as.numeric(Volume),
)

returns <- returns %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
  
)
save(returns, file = "returns.RData")


returns_long <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
returns_long <- returns %>%left_join(sp500 %>% select("Ticker", "Name", "Sector", "Industry"), by = c("Ticker" = "Ticker"))
returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker) %>% arrange(desc(Date))
```
## Performance calcs
```{r}
performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  #print(ticker)
  returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker) %>% arrange(desc(Date))
  
  
  thrity_day <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[21])/returns_long_by_ticker$Adjusted[21]
  ninety_day <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[63])/returns_long_by_ticker$Adjusted[63]
  one_year <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[253])/returns_long_by_ticker$Adjusted[253]
  three_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[759])/returns_long_by_ticker$Adjusted[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[1265])/returns_long_by_ticker$Adjusted[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[2518])/returns_long_by_ticker$Adjusted[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}
performance_summary
```


```{r}
load("sp500.RData")

performance_summary <- performance_summary %>% left_join(sp500, by = c("Ticker" = "Ticker"))
save(performance_summary, file = "performance_summary.RData")
save(returns_long,file="returns_long.RData")



```
#visualisation de SP&500
```{r}

sp<-sp500_price %>% ggplot(mapping=aes(x =Date,y=Adjusted) )+
  geom_line(color="red")+
  scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Adjusted_Close") +
  labs(
    title = "SP&500",
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
sp
```
#volume sp500
```{r}
# volume 
ggplot(sp500_price,aes(Date,Volume)) + 
  geom_line(color = "green") +theme(plot.title = element_text(hjust = 0.5)) +scale_y_log10()+
  xlab("Date") + 
  ylab("Volume") +
  labs(
    title =" SP&500 index",
    
    caption = "Source: Yahoo! Finance"
  ) 

```
#moyenne mobile sp500
```{r}
moyenne1<-sp500_price %>% 
  mutate(Rm_200=rollmean(Adjusted,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right")) %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Adjusted))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) 

moyenne1

```
# daily_returns sp500
```{r}
daily_returns_sp <- sp500_price %>%
  tq_transmute(select = Adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "Daily_returns_sp") # renames the column
daily_returns_sp %>%
  ggplot(aes(x =Date,y =Daily_returns_sp)) +
  geom_line(color="purple")+
  theme_classic() +
  xlab("Date") + 
  ylab("daily_returns of SP&500") +
  labs(
    title = "SP_500 returns ",
    
    
    caption = "Source: Yahoo! Finance"
  )
```
#visualization with time series 
```{r}

library(tseries)
ticker_dataSelect<-sp500_price %>% select("Date" , "Open","Close","Adjusted","Volume","High","Low")
ticker_dataSelect
ticker_data_series<-xts(ticker_dataSelect[,-1],order.by =as.Date(ticker_dataSelect$Date))
str(ticker_data_series)
ticker_data_series%>%Ad()%>%chartSeries()
ticker_data_series%>%chartSeries(TA='addBBands(n=20,sd=2);addVo();addMACD();addSMA(n=24,on=1,col="blue");addSMA(n=48,on=1,col="red");addMomentum(n=1)',theme=chartTheme("white"),subset='2012')

```
# choisir l'action désiré
```{r}
ticker <- "MMM"
```
#filtrer le dataframe
```{r}
charting_data <- returns_long %>% filter(Ticker == ticker)
```
#boxplot
```{r}
ggplot(data=charting_data) +
  geom_boxplot(aes(x = (Date), y=Adjusted, fill=Movement ))

```
#chart de prix d'action
```{r}
stock<-charting_data %>% ggplot(mapping=aes(x =Date,y=Adjusted) )+
  geom_line(color="purple")+
  scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Adjusted_Close") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
stock

```
#Volume
```{r}
# volume 
ggplot(charting_data,aes(Date,Volume)) + 
  geom_line(color = "darkblue") +theme(plot.title = element_text(hjust = 0.5)) +scale_y_log10()+
  xlab("Date") + 
  ylab("Volume") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) 

```
#moving average 
```{r}
moyenne<-charting_data %>% mutate(Rm_200=rollmean(Adjusted,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right")) %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Adjusted))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) 

moyenne
```
#visualization with time series 
```{r}
library(tseries)
ticker_dataSelect<-charting_data %>% select("Date" , "Open", "High","Low","Close","Adjusted","Volume")
ticker_dataSelect
ticker_data_series<-xts(ticker_dataSelect[,-1],order.by =as.Date(ticker_dataSelect$Date))
str(ticker_data_series)

ticker_data_series%>%Ad()%>%chartSeries()
ticker_data_series%>%Ad()%>%dailyReturn(type='log')
ticker_data_series%>%chartSeries(TA='addBBands(n=20,sd=2);addVo();addMACD();addSMA(n=24,on=1,col="blue");addSMA(n=48,on=1,col="red");addMomentum(n=1)',theme=chartTheme("white"),subset='2012')

```

```{r}
## Performance calcs

performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker) %>% arrange(desc(Date))
  
  
  thrity_day <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[21])/returns_long_by_ticker$Adjusted[21]
  ninety_day <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[63])/returns_long_by_ticker$Adjusted[63]
  one_year <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[253])/returns_long_by_ticker$Adjusted[253]
  three_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[759])/returns_long_by_ticker$Adjusted[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[1265])/returns_long_by_ticker$Adjusted[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[2518])/returns_long_by_ticker$Adjusted[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}
performance_summary
```

## Performance Charting
```{r}
performance_summary_data <- performance_summary %>% 
  filter(Ticker == ticker) %>% 
  select(Thirty_days, Ninety_days, One_year, Three_years, Five_years, Ten_years)

performance_summary_data <- performance_summary_data %>% gather("Period", "Return")

performance_summary_data <- performance_summary_data %>% mutate(
  Period = case_when(
    Period == "Thirty_days" ~ "1 Month", 
    Period == "Ninety_days" ~ "1 Quarter", 
    Period == "One_year" ~ "1 Year", 
    Period == "Three_years" ~ "3 Years", 
    Period == "Five_years" ~ "5 Years", 
    Period == "Ten_years" ~ "10 Years", 
  )
)

performance_summary_data$Period <- factor(performance_summary_data$Period, levels = c("1 Month", "1 Quarter", "1 Year", "3 Years", "5 Years", "10 Years"))

performance_chart <- ggplot(performance_summary_data) +
  geom_bar(aes(x = Period, y = Return), stat = "identity", fill = "#0066ff") +
  ylab("Annualized Return") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::percent) 


performance_chart
```
#group by industry

```{r}
x<-"Energy"
```
#filtrer
```{r}
data_segmentation2<-returns_long %>% filter(Industry == x)
```

```{r}
data_segmentation <- data_segmentation2 %>% arrange(desc(Date))
```

```{r}
performance_summary2 <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary2) <- c("Industry", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")
i <- 1
for(Industry in unique(returns_long$Industry)){
  returns_long_by_Industry<- returns_long %>% filter(Industry==x) %>% arrange(desc(Date))
  
  thrity_day <- (data_segmentation$Adjusted[1] - data_segmentation$Adjusted[21])/data_segmentation$Adjusted[21]
  ninety_day <- (data_segmentation$Adjusted[1] - data_segmentation$Adjusted[63])/data_segmentation$Adjusted[63]
  one_year <- (data_segmentation$Adjusted[1] - data_segmentation$Adjusted[253])/data_segmentation$Adjusted[253]
  three_year <- (1 + ((data_segmentation$Adjusted[1] - data_segmentation$Adjusted[759])/data_segmentation$Adjusted[759]))^(1/3)-1
  five_year <- (1 + ((data_segmentation$Adjusted[1] - data_segmentation$Adjusted[1265])/data_segmentation$Adjusted[1265]))^(1/5)-1
  ten_year <- (1 + ((data_segmentation$Adjusted[1] - data_segmentation$Adjusted[2518])/data_segmentation$Adjusted[2518]))^(1/10)-1
  
  performance_summary2[i, 1] <- Industry
  performance_summary2[i, 2] <- thrity_day
  performance_summary2[i, 3] <- ninety_day
  performance_summary2[i, 4] <- one_year
  performance_summary2[i, 5] <- three_year
  performance_summary2[i, 6] <- five_year
  performance_summary2[i, 7] <- ten_year
  
  i <- i + 1
}
performance_summary2
```

```{r}


performance_summary_data2 <- performance_summary2 %>% 
  filter(Industry==Industry) %>% 
  select(Thirty_days, Ninety_days, One_year, Three_years, Five_years, Ten_years)

performance_summary_data2 <- performance_summary_data2 %>% gather("Period", "Return")

performance_summary_data2 <- performance_summary_data2 %>% mutate(
  Period = case_when(
    Period == "Thirty_days" ~ "1 Month", 
    Period == "Ninety_days" ~ "1 Quarter", 
    Period == "One_year" ~ "1 Year", 
    Period == "Three_years" ~ "3 Years", 
    Period == "Five_years" ~ "5 Years", 
    Period == "Ten_years" ~ "10 Years", 
  )
)

performance_summary_data2$Period <- factor(performance_summary_data2$Period, levels = c("1 Month", "1 Quarter", "1 Year", "3 Years", "5 Years", "10 Years"))

performance_chart2 <- ggplot(performance_summary_data2) +
  geom_bar(aes(x = Period, y = Return), stat = "identity", fill = "red") +
  
  ylab("Annualized Return") +
  labs(
    title =paste0(data_segmentation2$Industry[1], " (", x, ")"),
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::percent)

performance_chart2
```

```{r}
rendement_journalier <- data_segmentation %>%
  tq_transmute(select = Adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "rendement_journalier") # renames the column
rendement_journalier %>%
  ggplot(aes(x =Date,y =rendement_journalier)) +
  geom_line(color="purple")+
  theme_classic() +
  xlab("Date") + 
  ylab("rendement_journalier") +
  labs(
    title =paste0(data_segmentation$Industry[1], " (",x,")"),
    
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )
```

## Industry Chart

```{r}
print(Industry)
sector <- returns_long %>% filter(Ticker == Ticker) %>% select(Sector) %>% as.character()
industry <- returns_long %>% filter(Ticker == Ticker) %>% select(Industry) %>% as.character()
industry_summary_data <- returns_long %>% 
  filter(Sector==sector) %>% 
  mutate(
    isIndustry = ifelse(Industry==Industry, "Industry", "Non_Industry")
  )

sector
industry

```


```{r}
industry_chart <- ggplot(industry_summary_data) +
  geom_bar(aes(x = Industry, y =Date, fill = isIndustry), stat = "summary", fun = "mean") +
  scale_fill_manual(values = c(Industry = "#ffff00", Non_Industry = "#0066ff")) +
  ylab("One Year Return") +
  labs(
    title = "Industry Returns",
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.background = element_rect(fill = "#17202A"),
    panel.background = element_rect(fill = "#17202A"),
    axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(color = "#ffffff"),
    axis.title.y = element_text(color = "#ffffff"),
    axis.title.x = element_text(color = "#ffffff"),
    plot.title = element_text(color = "#ffffff"),
    plot.subtitle = element_text(color = "#ffffff"),
    plot.caption = element_text(color = "#ffffff", face = "italic", size = 6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#273746"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
  )


industry_chart
```
xlab("Date") + 
  ylab("Adj_Close") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)

```{r}
moyenne<-charting_data %>% mutate(Rm_200=rollmean(Adjusted,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right")) %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Adjusted))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) 

moyenne
```
```{r}
moving_average<-charting_data%>% 
  mutate(Rm_200=rollmean(Adjusted,200,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right"))
moving_average
```
Moving average pour SP&500
```{r}


moyenne_SP<-sp500_price %>% 
  mutate(Rm_200=rollmean(Adjusted,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right")) 
moyenne_SP
ggplot(aes(x=Date))+
  geom_line(aes(y=Adjusted))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) +
  xlab("Date") + 
  ylab("Adjusted") +
  labs(
    title = ("Moyenne Mobile pour SP&500"),
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)

moyenne_SP
```


```{r}


```

```{r}
daily_returns <- charting_data %>%
  tq_transmute(select = Adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "Daily_returns") # renames the column
daily_returns

```



```

Visualisation of daily return of stock  
```{r}
daily_returns %>%
  ggplot(aes(x =Date,y =Daily_returns)) +
  geom_line() +
  theme_classic() +
  xlab("Date") + 
  ylab("daily_returns") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )
```

Visualisation of daily return of SP&500
```{r}

daily_returns_sp <- sp500_price %>%
  tq_transmute(select = Adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "Daily_returns_sp") # renames the column
daily_returns_sp %>%
  ggplot(aes(x =Date,y =Daily_returns_sp)) +
  geom_line() +
  theme_classic() +
  xlab("Date") + 
  ylab("daily_returns of SP&500") +
  labs(
    title = "SP_500 returns ",
    
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )
```










 
