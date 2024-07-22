# Generoi kuukausittaiset kansanedustajien määrät
# olettaa d_expanded:n olemassaolon


get_kedit_valta <- function(puolue_txt) {
  kansanedustajat <- d_expanded %>%
    filter(Title == "Kansanedustaja")
  
  # Lasketaan kansanedustajien lukumäärä per kuukausi ja puolue
  kansanedustajat_per_paiva_puolue <- kansanedustajat %>%
    group_by(Date, Party) %>%
    summarize(count = n())
  
  
  kok_kedit_lkm <- kansanedustajat_per_paiva_puolue |>  dplyr::filter(Party==puolue_txt)
  
  #monthly_power |> filter(Party==puolue_txt) |> rename(month=Date)
  
  puolue_kedit_valta <- left_join(monthly_power |> dplyr::filter(Party==puolue_txt) |>
                                    rename(Date=month), kok_kedit_lkm |>
                                    select(Date,count),by="Date")
  return(puolue_kedit_valta)
}
puolue_kedit_valta = get_kedit_valta("VIHR")