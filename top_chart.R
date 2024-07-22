library(tidyverse)
library(lubridate)
library(readr)
library(zoo)


DRAW_TOP_N = 100

# Lue data tiedostoista
d_title <- read.csv("titles.csv")
d_title_power <- read.csv("title_power.csv")

d_title <- tibble(d_title)
d_title_power <- tibble(d_title_power)

d_title <- d_title |> select(Title, Holder, Party, begin_date, end_date)

# Muunnetaan päivämäärät datetime-muotoon ja korjataan NA-arvot
d_title <- d_title %>%
  mutate(
    begin_date = as.Date(begin_date, origin = "1970-01-01"),
    end_date = as.Date(end_date, origin = "1970-01-01")
  )

# Korjataan NA-tapaukset kiinteällä päivämäärällä (15. kesäkuuta)
fixed_date <- as.Date("2023-06-15")
d_title$end_date[is.na(d_title$end_date)] <- fixed_date
d_title$begin_date[is.na(d_title$begin_date)] <- fixed_date

# Varmistetaan, että kaikki päivämäärät ovat kunnossa
d_title <- d_title %>%
  filter(!is.na(begin_date) & !is.na(end_date))

# Vaihdetaan päivämäärät tarvittaessa
d_title <- d_title %>%
  mutate(
    begin_date = pmin(begin_date, end_date),
    end_date = pmax(begin_date, end_date)
  )

# Laajennetaan data päivittäin
d_expanded <- d_title %>%
  rowwise() %>%
  mutate(Date = list(seq(from = begin_date, to = end_date, by = "day"))) %>%
  unnest(Date) %>%
  ungroup()

# Aggregoidaan data kuukausittain
d_expanded <- d_expanded %>%
  mutate(month = floor_date(Date, "month"))

# Lasketaan keskiarvo Score-arvoista, jos duplikaatteja on
d_title_power_avg <- d_title_power %>%
  group_by(Title) %>%
  summarise(Score = mean(Score, na.rm = TRUE), .groups = 'drop')

# Lisää Score muuttuja
d_expanded <- d_expanded %>%
  left_join(d_title_power_avg, by = "Title")

# Luo täydellinen data jokaiselle kuukaudelle ja henkilölle
all_months <- seq(min(d_expanded$month), max(d_expanded$month), by = "month")
all_holders <- unique(d_expanded$Holder)
all_combinations <- expand_grid(month = all_months, Holder = all_holders)

# Yhdistä kaikki kuukaudet ja henkilöt laajennettuun dataan
monthly_power_individual <- d_expanded %>%
  group_by(month, Holder) %>%
  summarise(monthly_power = sum(Score, na.rm = TRUE), .groups = 'drop') %>%
  right_join(all_combinations, by = c("month", "Holder")) %>%
  mutate(monthly_power = ifelse(is.na(monthly_power), 0, monthly_power))

# Lasketaan kuukausittainen kokonaisvalta henkilöille
total_monthly_power_individual <- monthly_power_individual %>%
  group_by(month) %>%
  summarise(total_power = sum(monthly_power))

# Yhdistetään kuukausittainen kokonaisvalta henkilöille
monthly_power_individual <- monthly_power_individual %>%
  left_join(total_monthly_power_individual, by = "month") %>%
  mutate(percentage_power = (monthly_power / total_power))

# Lasketaan 10 vuoden liukuva keskiarvo kuukausittain
monthly_power_individual_avg <- monthly_power_individual %>%
  group_by(Holder) %>%
  arrange(month) %>%
  mutate(avg_power_last_5_years = rollmean(percentage_power, k = 10*12, fill = NA, align = "right")) %>%
  ungroup()

# Lasketaan keskiarvo koko ajan jaksolta 5 vuoden liukuvan keskiarvon perusteella
average_power <- monthly_power_individual_avg %>%
  group_by(Holder) %>%
  summarise(avg_percentage_power = mean(avg_power_last_5_years, na.rm = TRUE)) %>%
  arrange(desc(avg_percentage_power))

# Top 100 vallankäyttäjää
top_power_holders <- average_power %>%
  top_n(DRAW_TOP_N, avg_percentage_power)

# Suodatetaan data top 100 vallankäyttäjille
monthly_power_top_individuals <- monthly_power_individual_avg %>%
  filter(Holder %in% top_power_holders$Holder)

# Lasketaan maksimi valta ja kuukausi kullekin henkilölle 5 vuoden liukuvan keskiarvon perusteella
max_power <- monthly_power_top_individuals %>%
  group_by(Holder) %>%
  filter(avg_power_last_5_years == max(avg_power_last_5_years, na.rm = TRUE)) %>%
  slice(1) %>%  # Jos useita maksimikohtia, ota vain yksi
  ungroup()


