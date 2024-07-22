
source("load.r")
source("load_draw.r")
monthly_power |> filter(Party %in% c("SDP","KOK","KESK")) |> ggplot(aes(x = month, y = percentage_power, fill = Party)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = all_colors) +
  labs(title = "Percentage of Power Over Time",
  x = "Month",
  y = "Percentage of Power") +
  theme_minimal()