# Ladataan tarvittavat kirjastot
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)

# Määritellään puolueparametri
target_party <- "RKP"

# Muunnetaan päivämäärät datetime-muotoon
d_title <- d_title %>%
  mutate(
    begin_date = as.Date(begin_date),
    end_date = as.Date(end_date, origin = "1970-01-01")
  )

# Korjataan NA-tapaukset nykyiseksi päivämääräksi
d_title$end_date[is.na(d_title$end_date)] <- Sys.Date()

# Laajennetaan data päivittäin
d_expanded <- d_title %>%
  rowwise() %>%
  mutate(Date = list(seq(from = begin_date, to = end_date, by = "day"))) %>%
  unnest(Date) %>%
  ungroup()

# Aggregoidaan data kuukausittain
d_expanded <- d_expanded %>%
  mutate(month = floor_date(Date, "month"))

# Lisää Score muuttuja
d_expanded <- d_expanded %>%
  left_join(d_title_power, by = "Title")

# Luo täydellinen data jokaiselle kuukaudelle ja puolueelle
all_months <- seq(min(d_expanded$month), max(d_expanded$month), by = "month")
all_parties <- unique(d_expanded$Party)
all_combinations <- expand_grid(month = all_months, Party = all_parties)

# Yhdistä kaikki kuukaudet ja puolueet laajennettuun dataan
monthly_power <- d_expanded %>%
  group_by(month, Party) %>%
  summarise(monthly_power = sum(Score, na.rm = TRUE), .groups = 'drop') %>%
  right_join(all_combinations, by = c("month", "Party")) %>%
  mutate(monthly_power = ifelse(is.na(monthly_power), 0, monthly_power))

# Lasketaan kuukausittainen kokonaisvalta
total_monthly_power <- monthly_power %>%
  group_by(month) %>%
  summarise(total_power = sum(monthly_power))

# Yhdistetään kuukausittainen kokonaisvalta
monthly_power <- monthly_power %>%
  left_join(total_monthly_power, by = "month") %>%
  mutate(percentage_power = (monthly_power / total_power) * 100)

# Suodatetaan valittu puolue
selected_power <- monthly_power %>%
  filter(Party == target_party)

# Visualisoidaan ggplot2:lla
ggplot(selected_power, aes(x = month, y = percentage_power, color = Party, group = Party)) +
  geom_line(color = ifelse(target_party == "VIHR", "#61BF1A", "black")) +
  labs(title = paste(target_party, "n valta Suomessa ajan funktiona"),
       x = "Vuosi",
       y = "Vallan prosenttiosuus") +
  ylim(0, 100) +
  theme_minimal()