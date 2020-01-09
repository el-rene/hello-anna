### Skript zur Datenbereinigung

# benötigte Libraries laden

library(readxl)
library(tidyverse)
library(lubridate) # falls noch nicht vorhanden über install packages erst installieren

# Import der Daten

raw_data <- read_excel("Online_Retail_Data.xlsx")

# Manipulation der Rohdaten

weekend <- c("Sa", "So") # Sa. und So. als Value, um Weekend-Spalte im nächsten Schritt zu erzeugen

bereinigte_daten <- raw_data %>% # Verwende die hochgeladenen Rohdaten und dann...
  distinct(InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country) %>% # lösche doppelte Werte 
  filter(Country == "United Kingdom", # filtere diese auf UK, 
         CustomerID != is.na(CustomerID), # schließe Customer ohne ID aus und
         Quantity > 0, # berücksichtige nur Artikel mit einer Quantity größer Null (nochmal überdenken, aber für den Moment zum arbeiten okay)
         UnitPrice >= 0) %>% # und filtere negative Preise raus. 
  mutate(InvoiceNo = factor(InvoiceNo), # Ändere InvoiceNo, StockCode, CustomerID und Country in Faktoren
         StockCode = factor(StockCode),
         CustomerID = factor(CustomerID),
         Country = factor(Country),
         date = floor_date(InvoiceDate, "day"), # erzeuge eine Date Spalte aus dem InvoiceDate
         hour = hour(InvoiceDate), # erzeuge eine zusätzliche hour Spalte,
         weekday = wday(InvoiceDate, label = TRUE), # eine weekdaySpalte
         is_weekend = weekday %in% weekend) # sowie eine Spalte ob eine Order am Wochenende erfolgte. 
