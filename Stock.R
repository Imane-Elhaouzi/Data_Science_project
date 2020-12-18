

```{r}
install.packages("tidyquant")
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
View(sp500_price
)
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
returns_long <-returns %>% left_join(sp500 %>% select("Ticker", "Name", "Sector", "Industry"), by = c("Ticker" = "Ticker"))

```

```{r}
## Performance calcs

performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  print(ticker)
  
  returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker ) %>% arrange(desc(Date))
  
  thrity_day <- (returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[21])/returns_long_by_ticker$Adj_Close[21]
  ninety_day <- (returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[63])/returns_long_by_ticker$Adj_Close[63]
  one_year <- (returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[253])/returns_long_by_ticker$Adj_Close[253]
  three_year <- (1 + ((returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[759])/returns_long_by_ticker$Adj_Close[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[1265])/returns_long_by_ticker$Adj_Close[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[2518])/returns_long_by_ticker$Adj_Close[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}

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

ticker <- "AMZN"
```

```{r}
returns

charting_data <- returns_long %>% filter(Ticker == ticker,Date >= "2020-01-01")
ggplot(data=charting_data) +
  geom_line(aes(x = (Date), y=Adj_Close ))

```


```{r}
returns
install.packages("plotly")
library(plotly)
```

```{r}
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2020-01-01")
charting_data %>% plot_ly(
  type="Candelstick",
  x=Date,
  y=Adj_Close
  
) 
```


```{r}
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2020-01-01")
Candelstick<-ggplot(data=charting_data) +
  geom_boxplot(aes(x = (Date), y=Adj_Close  ,fill=Movement))+
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
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2013-06-24")
ggplot(charting_data,aes(Date,Adj_Close,fill =Movement)) +
  geom_boxplot()
```

