setwd('D:/Bruh/R')
getwd()
#Import cac thu vien
library(ggplot2)
library(lubridate)
library(quantmod)
library(highcharter)

#Doc du lieu
df = read.csv('TSLA.csv')
names(df)

#Dinh dang cot Date tu str thanh kieu du lieu Date
df$Date <- as.Date(df$Date, format = '%Y-%m-%d')
df$Date
View(df)
str(df)
attach(df)

#Tao ham de tinh toan cac gia tri thong ke mo ta
tkmt <- function(x){
  av <- mean(x)
  sd <- sd(x)
  se <- sd/sqrt(length(x))
  c(MEAN=av, SD=sd, SE=se, summary(x))
}

#Open
tkmt(Open)

ggplot(df, aes(x = Date, y = Open)) +
  geom_line(color = "indianred3", 
            size=1 )
#Close
tkmt(Close)

ggplot(df, aes(x = Date, y = Close)) +
  geom_line(color = "indianred3", 
            size=1 )
#High
tkmt(High)

ggplot(df, aes(x = Date, y = High)) +
  geom_line(color = "indianred3", 
            size=1 )

#Low
tkmt(Low)

ggplot(df, aes(x = Date, y = Low)) +
  geom_line(color = "indianred3", 
            size=1 )

#Volume
tkmt(Volume)

ggplot(df, aes(x = Date, y = Volume)) +
  geom_line(color = "indianred3", 
            size=0.5 )
#Lay du lieu truc tiep tu thu vien quantmod
tsla <- getSymbols("TSLA", auto.assign=FALSE, from="2020-12-16", to="2021-12-16")

#Ve bieu do nen
chartSeries(tsla, name="TSLA Chart", theme="white")

#Bieu do SMA
highchart(type="stock") %>% 
  hc_add_series(tsla) %>% 
  hc_add_series(SMA(na.omit(Cl(tsla)),n=20),name="SMA(20)") %>% 
  hc_title(text="<b>TSLA</b>")
