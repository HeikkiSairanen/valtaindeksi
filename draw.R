# Ladataan tarvittavat kirjastot
library(dplyr)
library(lubridate)
library(ggplot2)

# Muunnetaan päivämäärät datetime-muotoon
d_title <- d_title %>%
  mutate(
    begin_date = as.Date(begin_date),
    end_date = as.Date(end_date, origin = "1970-01-01")
  )

# Korjataan NA-tapaukset nykyiseksi päivämääräksi
d_title$end_date[is.na(d_title$end_date)] <- Sys.Date()

# Laajennetaan data päivittäin
dates <- seq(min(d_title$begin_date), max(d_title$end_date), by = "day")
d_expanded <- d_title %>%
  rowwise() %>%
  do(data.frame(
    Date = seq(.$begin_date, .$end_date, by = "day"),
    Title = .$Title,
    Holder = .$Holder..if.available.,
    Party = .$Party,
    Score = .$Score
  ))

# Aggregoidaan data kuukausittain
d_expanded <- d_expanded %>%
  mutate(month = floor_date(Date, "month"))

# Rajataan data vain 2000-luvulle
d_expanded <- d_expanded %>%
  filter(month >= as.Date("2000-01-01"))

# Luo täydellinen data jokaiselle kuukaudelle ja puolueelle
all_months <- seq(min(d_expanded$month), max(d_expanded$month), by = "month")
all_parties <- unique(d_expanded$Party)
all_combinations <- expand.grid(month = all_months, Party = all_parties)

# Yhdistä kaikki kuukaudet ja puolueet laajennettuun dataan
monthly_power <- d_expanded %>%
  group_by(month, Party) %>%
  summarise(monthly_power = sum(Score), .groups = 'drop') %>%
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

# Määritellään värit puolueille
party_colors <- c(
  "VIHR" = "#61BF1A",
  "SDP" = "#CD1232",
  "VAS" = "#C55A66",
  "KOK" = "#6DCFF6",
  "KESK" = "#95C11A",
  "RKP" = "#0079C9",
  "KD" = "#00A0E1",
  "PS" = "#001D99",
  "ST" = "#004B8F",
  "LIIK" = "#BB3492"
)

# Muut puolueet saavat satunnaiset värit, jos niitä on
remaining_parties <- setdiff(unique(monthly_power$Party), names(party_colors))
if (length(remaining_parties) > 0) {
  set.seed(123)  # For reproducibility
  remaining_colors <- hue_pal()(length(remaining_parties))
  names(remaining_colors) <- remaining_parties
  # Yhdistetään värit
  all_colors <- c(party_colors, remaining_colors)
} else {
  all_colors <- party_colors
}

# Visualisoidaan ggplot2:lla
ggplot(monthly_power, aes(x = month, y = percentage_power, color = Party, group = Party)) +
  geom_line() +
  scale_color_manual(values = all_colors) +
  labs(title = "Puolueiden valta Suomessa 2000-luvulla ajan funktiona",
       x = "Vuosi",
       y = "Vallan prosenttiosuus") +
  theme_minimal()
