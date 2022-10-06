## STETUP ----------
library(tidyverse)

international_matches <- read.csv(file = "~/Desktop/work/data/r/kaggle/fifa_wc_2022/international_matches.csv")


## DATA PREP ----------
wc_data <- international_matches %>%
  filter(date >= '2006-01-01', tournament %in% c("AFC Asian Cup", "African Cup of Nations", "CONCACAF Nations League", "Gold Cup",
                                                 "Copa América", "FIFA World Cup", "FIFA World Cup qualification", "UEFA Euro",
                                                 "UEFA Euro qualification", "UEFA Nations League"))

