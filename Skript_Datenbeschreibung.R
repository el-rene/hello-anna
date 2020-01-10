### Skript zur Datenbeschreibung

# Libraries laden
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

## Unterteilung nach Kunde, Produkt und Bons

# Kunden
# Tabelle nach Kunden sortiert

customer_summary <- 
  bereinigte_daten %>% 
  group_by(CustomerID) %>% 
  summarise(orders = n(), # Anzahl an Orders n()
            quantity = sum(Quantity),
            revenue = sum(Quantity * UnitPrice),
            UnitPrice = sum(UnitPrice),
            first_order = min(date),
            last_order = max(date)) %>% 
  mutate(avg_order_val = revenue / orders,
         avg_item_val = revenue / quantity)

#Wie viele Kunden haben wir?
dim(customer_summary) # 3921 Kunden
anzahl_kunden=3921

# Wie viele Bestellungen wurden in dem Zeitraum ausgelöst?

sum(customer_summary$orders)
gesamtbestellungen_kunden=349227

summarise(bereinigte_daten$InvoiceNo)
# Wie viele Bestellungen pro Kunde?

summary(customer_summary$orders)
bestellungen_pro_kunde=mean(customer_summary$orders)
bestellungen_pro_kunde_median=median(customer_summary$orders)

# Wie viel Umsatz pro Kunde?

umsatz_pro_kunde_mean=mean(customer_summary$revenue)
umsatz_pro_kunde_medaian=median(customer_summary$revenue)

# Gesamter Umsatz über alle Kunden?

sum(customer_summary$revenue)
gesamtumsatz_kunden=7285025

# Verteilung Bestellungen und Umsatz (Pareto Prinzip)?

#Daten vorbereiten
customer_revenue <- 
  customer_summary %>% 
  group_by(CustomerID) %>% 
  summarise(orders = n(), # Anzahl an Orders n()
            quantity = sum(quantity),
            revenue = sum(quantity * UnitPrice),
            prozent_rev = sum(revenue/gesamtumsatz_kunden*100))

ggplot(data = customer_summary) +
  geom_smooth(mapping = aes(x=avg_order_val, y=revenue), color="blue")


      
## Produkte







