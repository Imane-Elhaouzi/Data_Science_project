

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
library(tidyquant)

setwd("C:/Users/acer/Desktop/Data_Science_project")
```



```{r}
## Import Tickers
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

```{r}
# importation of price Data sp&500
sp500_price<-read.csv("^GSPC.csv")
sp500_price<-sp500_price %>% 
  select(Date,Adj.Close,Volume,Close,Open) %>% 
  mutate(Date=as.Date(Date))
sp500_price <-sp500_price %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
)
sp500_price
View(sp500_price)
```

```{r}
stock_join<-inner_join(returns,sp500_price,by = c("Date" = "Date"), suffix = c("_stock", "_sp500_price"))
View(stock_join)
```


```{r}

## Import Price Data
returns <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume", "Ticker")

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
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume", "Ticker")
returns<- returns %>%  select("Date","Ticker", "Open", "High", "Low", "Close", "Adj_Close", "Volume")
returns <- returns %>% mutate(
  Open = as.numeric(Open),
  High = as.numeric(High),
  Low = as.numeric(Low),
  Close = as.numeric(Close),
  Adj_Close= as.numeric(Adj_Close),
  Volume=as.numeric(Volume),
)

returns <- returns %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
  
)
save(returns, file = "returns.RData")

returns_long <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
returns_long <- returns %>%left_join(sp500 %>% select("Ticker", "Name", "Sector", "Industry"), by = c("Ticker" = "Ticker"))

```

```{r}
## Performance calcs

performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  print(ticker)
  
  
  
  thrity_day <- (returns_long$Adj_Close[1] - returns_long$Adj_Close[21])/returns_long$Adj_Close[21]
  ninety_day <- (returns_long$Adj_Close[1] - returns_long$Adj_Close[63])/returns_long$Adj_Close[63]
  one_year <- (returns_long$Adj_Close[1] - returns_long$Adj_Close[253])/returns_long$Adj_Close[253]
  three_year <- (1 + ((returns_long$Adj_Close[1] - returns_long$Adj_Close[759])/returns_long$Adj_Close[759]))^(1/3)-1
  five_year <- (1 + ((returns_long$Adj_Close[1] - returns_long$Adj_Close[1265])/returns_long$Adj_Close[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long$Adj_Close[1] - returns_long$Adj_Close[2518])/returns_long$Adj_Close[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}
```


```{r}
load("sp500.RData")

performance_summary <- performance_summary %>% left_join(sp500, by = c("Ticker" = "Ticker"))
save(performance_summary, file = "performance_summary.RData")



```

```{r}
library(ggplot2)
library(dplyr)
library(tidyr)
```

```{r}

ticker <- "NFLX"
```

```{r}
returns

charting_data <- returns_long %>% filter(Ticker == ticker)
ggplot(data=charting_data) +
  geom_boxplot(aes(x = (Date), y=Adj_Close, fill=Movement ))

```


```{r}
charting_data <- returns_long %>% filter(Ticker == ticker)
charting_data 
 

```


```{r}
Candelstick<-ggplot(data=charting_data) +
  geom_boxplot(aes(x =Date,y=Adj_Close,fill=Movement))+
  scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Stock Price") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
Candelstick

```

```{r}
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2009-07-26")
ggplot(charting_data,aes(Date,Adj_Close,fill =Movement)) +
  geom_boxplot()
```


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

```{r}
#distribution de volume à vrifier 

ggplot(Volume, mapping =aes(Date)) + geom_histogram(bins =30,color = "darkblue") +
  facet_wrap(~Date)+
  theme(plot.title = element_text(hjust = 0.5)) +
   xlab("Date") + 
  ylab("Volume") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) 
```

```{r}
 moyenne<-charting_data %>% mutate(Rm_200=rollmean(Adj_Close,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adj_Close,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adj_Close,50,na.pad=TRUE, align="right")) %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Adj_Close))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) +
  xlab("Date") + 
  ylab("Adj_Close") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
   moyenne
```
```{r}
 moving_average<-charting_data%>% 
  mutate(Rm_200=rollmean(Adj_Close,200,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adj_Close,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adj_Close,50,na.pad=TRUE, align="right"))
moving_average
```


```{r}
#à verifier 

#distribution de volume 

ggplot(charting_data,aes(Date)) + geom_histogram(color = "darkblue") +ggtitle("Apple Volume distrubition")+theme(plot.title = element_text(hjust = 0.5)) 
```



```{r}


#install.packages("quantmod")
#install.packages("TTR")
library(quantmod)
library(TTR)
library(tidyquant)
library(timetk)
```
```{r}
```
Daily Return 

```{r}
daily_returns <- charting_data %>%
  tq_transmute(select = Adj_Close,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "Daily_returns") # renames the column

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
  tq_transmute(select = Adj.Close,           # this specifies which column to select   
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
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
    )
```
```{r}
daily_returns %>%
  ggplot(aes(x = Daily_returns)) +
  geom_histogram(color="white") +
  theme_classic() +
   xlab("daily_returns ") + 
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )+
scale_x_continuous(labels = scales::percent)+
  annotate(geom = 'text', x = -0.30, y= 200, label = "Extremely\nnegative\nreturns") +
  annotate(geom = 'segment', x = -0.305, xend = -0.35,  y = 120, yend = 20, color = 'red', arrow = arrow()) +
  annotate(geom = 'segment', x = 0.405, xend = 0.42,  y = 120, 
           yend = 20, color = 'blue', arrow = arrow(type = "open")) +
  annotate(geom = 'text', x = 0.430, y = 200, label = "Extremely\npositive\nreturns")
```
```{r}
daily_returns_sp %>%
  ggplot(aes(x = Daily_returns_sp)) +
  geom_histogram(color="white") +
  theme_classic() +
   xlab("daily_returns for sp&500 ") + 
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )+
scale_x_continuous(labels = scales::percent)+
  annotate(geom = 'text', x = -0.30, y= 200, label = "Extremely\nnegative\nreturns") +
  annotate(geom = 'segment', x = -0.305, xend = -0.35,  y = 120, yend = 20, color = 'red', arrow = arrow()) +
  annotate(geom = 'segment', x = 0.405, xend = 0.42,  y = 120, 
           yend = 20, color = 'blue', arrow = arrow(type = "open")) +
  annotate(geom = 'text', x = 0.430, y = 200, label = "Extremely\npositive\nreturns")
```
 
  visualisation of daily return ( geom_bar)
  
```{r}
daily_returns %>%
  ggplot(aes(x =Date, y =Daily_returns)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  xlab("26-July-2010 to 24-July-2020 ") + 
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-0.6,0.8,0.1),
                     labels = scales::percent) +
  scale_x_date(date_breaks = "years", date_labels = "%Y")
```
```{r}
daily_returns_sp %>%
  ggplot(aes(x =Date, y =Daily_returns_sp)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  xlab("26-July-2010 to 24-July-2020 ") + 
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-0.6,0.8,0.1),
                     labels = scales::percent) +
  scale_x_date(date_breaks = "years", date_labels = "%Y")
```
```{r}
daily_mean_ret <- daily_returns %>%
  select(Daily_returns) %>%
  .[[1]] %>%
  mean(na.rm = TRUE)

# Calculating the standard deviation

daily_sd_ret <- daily_returns %>%
  select(Daily_returns) %>%
  .[[1]] %>%
  sd()


nflx_stat <- tibble(period = c("Daily"),
                    mean = c(nflx_daily_mean_ret),
                    sd = c(nflx_daily_sd_ret))

nflx_stat 
```


```{r}
daily_returns %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(Mean_Returns = mean(Daily_returns),
            Standard_Deviation = sd(Daily_returns)) %>%
  gather(Mean_Returns, Standard_Deviation, key = statistic, value = value) %>%
  ggplot(aes(x = year, y = value, fill = statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(-0.1,0.4,0.02),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(2009,2018,1)) +
  labs(x = "Year",
       title = paste0(charting_data$Name[1], " (", ticker, ")",(",  Monthly Mean and Standard Deviation since 2010     ,")) ,
    subtitle = charting_data$Sector[1],
    y = "")+
  theme_bw() +
  
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set1",
                    name = "",
                    labe = c("Mean", "Standard Deviation")) 
 
```



```{r}
symbol_stock <- c("FB", "AMZN", "AAPL", "NFLX", "GOOG") 
charting_data_multiple <- returns_long %>% filter(Ticker == symbol_stock,Date >= "2009-07-26")
charting_data_multiple %>%
  ggplot(aes(x =Date, y =Adj_Close)) +
  geom_line() +
  facet_wrap(~Ticker, scales = "free_y") +  # facet_wrap is used to make diff frames
  theme_classic() +       # using a new theme
  labs(x = "Date", y = "Adj_Close") +
  ggtitle("Price chart for multiple stocks")
```

```{r}
charting_data_multiple %>%
  ggplot(aes(x =Date, y =Adj_Close, color =Ticker)) +
  geom_line() +
  ggtitle("Price chart for multiple stocks")
```
```{r}
multpl_stock_daily_returns <- charting_data_multiple %>%
  group_by(Ticker) %>%                            # We are grouping the stocks by the stock symbol
  tq_transmute(select =Adj_Close,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')

#Calculating the monthly returns for multiple stocks

multpl_stock_monthly_returns <- charting_data_multiple %>%
  group_by(Ticker) %>%                             # We are grouping the stocks by symbol
  tq_transmute(select = Adj_Close,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'returns')
```

```{r}
multpl_stock_daily_returns %>%
  ggplot(aes(x =Date, y =returns)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~Ticker, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Daily returns for Multipl stock") +
  labs(x = "Date", y = "Returns") +
  scale_color_brewer(palette = "Set2",
                     name = "",
                     guide = FALSE) +
  theme_classic()
```
```{r}
#Calculating the yearly mean and standard deviation of returns.

multpl_stock_daily_returns %>%
  mutate(year = year(Date)) %>%
  group_by(Ticker, year) %>%
  summarise(mean = mean(returns),
            sd = sd(returns))
```
 
```{r}
# Calculating the Covariance
multpl_stock_monthly_returns %>%
  spread(Ticker, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cov()
```
 
 
