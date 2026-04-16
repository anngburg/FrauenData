library(tidyverse)
library(xlsx)

Frauen_data <- read_csv("Frauen_data.csv")

qual_Frauen_data <- Frauen_data %>% select(Case, Hürden_offen, Unterstützung_offen)

write.xlsx(qual_Frauen_data, "QualitativeAuswertung_Frauen.xlsx")
