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

# Wie viele Bestellungen pro Kunde?

bestellungen_pro_kunde=mean(customer_summary$orders)
durchschnittliche_bestellungen=gesamtbestellungen_kunden/anzahl_kunden #das gleiche wie drüber

summary(customer_summary$orders)

bestellungen_pro_kunde_median=median(customer_summary$orders)

# Wir sehen das mindestens ein Kunde 7676 Bestellungen getätigt hat, deshalb Durschnittswert erhöht
# Betrachtet man stattdessen den Median, liegt der Durchschnitt bei 40, kommmt der Realität näher, da der Ausreißer nicht so stark ins Gewicht fällt.
bestellungen_pro_kunde_median=median(customer_summary$orders)

# Wie viel Umsatz pro Kunde?

umsatz_pro_kunde_mean=mean(customer_summary$revenue)
umsatz_pro_kunde_medaian=median(customer_summary$revenue)

# Gesamter Umsatz über alle Kunden?

sum(customer_summary$revenue)
gesamtumsatz_kunden=7285025

# Verteilung Bestellungen und Umsatz (Pareto Prinzip)?

#Daten vorbereiten - NOCH NICHT READY
customer_revenue <- 
  bereinigte_daten %>% 
  group_by(CustomerID) %>% 
  summarise(orders = n(), # Anzahl an Orders n()
            quantity = sum(Quantity),
            revenue = sum(Quantity * UnitPrice),
            prozent_rev = sum(revenue / gesamtumsatz_kunden)) %>% 
  mutate(avg_order_val = revenue / orders) 







  sum(customer_revenue$revenue)
  sum(customer_revenue$prozent_rev)
  
  
ggplot(data = customer_revenue) +
  geom_point(mapping = aes(x=CustomerID, y=prozent_rev), color="blue") 

hist(customer_revenue$revenue)

ggplot(data = customer_revenue) +
  geom_histogram(mapping = aes(x =revenue)) +
  scale_x_log10() +
  stat_bin(bins = 30)
  
ggplot(aes(x = log10(revenue)), data = customer_revenue) + 
  geom_histogram()
    

ggplot(aes(x=revenue), data = customer_revenue) + 
  geom_histogram() + 
  scale_x_log10() + 
  stat_bin(binwidth = 30)  

      
## Produkte und Bestellungen


product_summary <- bereinigte_daten %>%
  group_by(StockCode, Description) %>%
  summarise(anzahl = n(),
            unit_price = mean(UnitPrice),
            revenue = anzahl * unit_price)

# Wochentag, wie viele Bestellungen gab es je Wochentag
weekday_summary <- bereinigte_daten %>%
  group_by(InvoiceNo, weekday) %>%
  summarise(anzahl_produkte = n()) %>%
  group_by(weekday) %>%
  summarise(anzahl_bestellungen = n())

# Uhrzeit, wie viele Bestellungen nach Tageszeit

hour_summary <- bereinigte_daten %>% 
  group_by(InvoiceNo, hour) %>% 
  summarise(anzahl_produkte = n()) %>% 
  group_by(hour) %>% 
  summarise(anzahl_bestellungen_tageszeit = n())

# Wochentag nach Uhrzeit der Bestellung
weekday_hour_summary <- bereinigte_daten %>%
  group_by(InvoiceNo, weekday, hour)%>%
  summarise(anzahl_produkte = n())%>%
  group_by(hour, weekday) %>%
  summarise(anzahl_bestellungen = n())


# Zeigt den Umsatz pro Bestellung an und wie viele Produkte pro Bestellung
order_summary <- bereinigte_daten %>% 
  mutate(revenue = UnitPrice*Quantity) %>% 
  group_by(InvoiceNo, weekday, hour) %>% 
  summarise(anzahl_verschiedene_produkte = n(),
            revenue_order = sum(revenue),
            anzahl_produkte = sum(Quantity))



