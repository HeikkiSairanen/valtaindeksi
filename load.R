library(tidyverse)
library(lubridate)
library(readr)


# Lue data tiedostoista

d_title <- read.csv("titles.csv")
d_title_power <- read.csv("title_power.csv")

d_title <- tibble(d_title)
d_title_power <- tibble(d_title_power)

#d_title$Holder = d_title$Holder..if.available.
d_title <- d_title |> select(Title,Holder,Party,begin_date,end_date)

# Lue eduskuntavaalien data
d_ek <- read_csv("ek_vaalit.csv") %>%
  rename(
    pvm = `...1`,
    SDP = `STP\nSDP`,
    KOK = `SP\nKok.`,
    ED = `NSP\nEd.\nKP\nLKP`,
    RKP = `RKP`,
    KESK = `SML\nML\nKePu\nKesk.`,
    SKrTL = `SKrTL`,
    VAS = `SSTP\nSTPV\nSKDL\nVas.`,
    SPP = `SPP\nPMP`,
    SV = `SV`,
    IKL = `IKL`,
    KP = `KP`,
    TPSL = `TPSL`,
    VL = `VL`,
    PS = `SPP\nSMP\nPS`,
    KD = `SKL\nKD`,
    SKYP = `SKYP`,
    PKP = `SPK\nPKP\nPOP`,
    VIHR = `Vihreät`,
    Nuors = `Nuors.`,
    EPV = `Vihreät\nEPV\nKiPu`,
    MUUT = `muut`
  ) %>%
  mutate(pvm = dmy(pvm))

# Muutetaan kaikki sarakkeet ensin merkki-tyyppisiksi ja sitten numeerisiksi ja puuttuvat arvot nolliksi
d_ek <- d_ek %>%
  mutate(across(-pvm, as.character)) %>%
  mutate(across(-pvm, ~as.numeric(.))) %>%
  mutate(across(-pvm, ~replace_na(., 0)))

# Lataa purrr kirjasto
library(purrr)

# Apufunktio seuraavien vaalien päivämäärän laskemiseen
seuraavat_vaalit <- function(date) {
  seuraava_vaali <- d_ek %>%
    filter(pvm > date) %>%
    arrange(pvm) %>%
    slice(1) %>%
    pull(pvm)
  
  if (length(seuraava_vaali) == 0) {
    return(NA)
  } else {
    return(as.Date(seuraava_vaali - days(1)))
  }
}

d_uncounted <- d_ek %>%
  pivot_longer(cols = -pvm, names_to = "Party", values_to = "Count") %>%
  rowwise() %>%
  mutate(
    begin_date = pvm,
    end_date = seuraavat_vaalit(pvm),
    Count = as.integer(Count)
  ) %>%
  ungroup() %>%
  uncount(Count, .remove = FALSE)

d_title_compatible <- d_uncounted %>%
  select(Party, begin_date, end_date)

d_title_compatible$Title = rep("Kansanedustaja",nrow(d_title_compatible))
d_title_compatible$Holder = rep(NA,nrow(d_title_compatible))

d_title_compatible$begin_date = as.character(d_title_compatible$begin_date)
d_title_compatible$end_date = as.character(d_title_compatible$end_date)


d_title <- bind_rows(d_title, d_title_compatible)


# työnnetään eteenpäin aloituspäivää sellaisissa, joissa se olisi sama kuin lopettavan lopetuspäivä

for(i in 2:(nrow(d_title))) {
  if(!is.na(d_title$end_date[i-1]) && d_title$begin_date[i] == d_title$end_date[i-1]) {
    d_title$begin_date[i] = as.character(as.Date(d_title$begin_date[i])+days(1))
  } 
  
}

# otetaan pois puolueettomat henkilöt

d_title <- d_title |> dplyr::filter(!is.na(Party))
