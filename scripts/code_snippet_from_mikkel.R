##preliminary code snippet for R estimation by Mikkel Freltoft Krogsholm. Mikkel maintains covid19data.dk

library(tidyverse)
library(EpiEstim)

url <- "https://api.covid19data.dk/ssi_newly_hospitalized"

hosp_raw <- jsonlite::fromJSON(url)

hosp_raw <- hosp_raw %>%
  as_tibble() %>%
  mutate(date = date %>% lubridate::ymd_hms() %>% as.Date())


hosp_raw %>% slice(4:n()) %>%
  ggplot() + 
  geom_bar(aes(date, newly_hospitalized), stat = "identity")

ads <- hosp_raw  %>% slice(5:n())

confirmed_cases <- ads %>% rename(I = newly_hospitalized)

res_parametric_si <- EpiEstim::estimate_R(confirmed_cases, 
                                          method = "parametric_si", 
                                          config = EpiEstim::make_config(list(mean_si = 4.7, std_si = 1)))



plot(res_parametric_si)