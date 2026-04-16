library(tidyverse)

Frauen_data <- read_csv("Frauen_data.csv")


Frauen_data_long <- Frauen_data %>% select(Case, Hü_offen_WartezeitTherapeut:Hü_offen_kA) %>% pivot_longer(cols = c(Hü_offen_WartezeitTherapeut:Hü_offen_kA),
                                                 names_to = "Hürde_long", values_to = "Value") %>% na.omit(.)

Zuordnung <- c("Hü_offen_WartezeitTherapeut" = "Wartezeit/ Therapeut finden",	
               "Hü_offen_Kontaktaufnahme" = "Kontaktaufnahme",	
               "Hü_offen_SchamAngst" = "Scham/ Angst",
               "Hü_offen_ErnstGenommenWerden" = "Nicht ernst genommen werden (durch Fachpersonal)",
               "Hü_offen_EingestehenErkennenHilfe" = "Eingestehen/ Erkennen, dass man krank ist bzw. Hilfe braucht",	
               "Hü_offen_DepressiveSymptome" = "Depressive Symptome",
               "Hü_offen_schlechteTherapieerf" = "Negative Behandlungserfahrungen",
               "Hü_offen_negativeAuswirkungen" = "Negative Auswirkungen",
               "Hü_offen_Stigma" = "Stigmatisierung",
               "Hü_offen_SorgePlatzwegnehmen" = "Sorge, jemandem den Platz wegzunehmen",
               "Hü_offen_FehlendesWissenInfo" = "Fehlende(s) Wissen/ Informationen",
               "Hü_offen_keineZeit" = "Keine Zeit",
               "Hü_offen_KeinVertraueninHilfe" = "Kein Vetrauen in Hilfe",
               "Hü_offen_DiagnoseUrsache" = "Diagnose bekommen",
               "Hü_offen_Sonstiges" = "Sonstiges",
               "Hü_offen_KeineHürde" = "Keine Hürden",
               "Hü_offen_kA" = "Keine Angabe")


Frauen_data_long <- Frauen_data_long %>%
  mutate(Hürde_long = factor(Hürde_long, levels = rev(names(Zuordnung))))

BarPlot_Hürden_offen <- ggplot(Frauen_data_long, aes(y = Hürde_long)) +
  geom_bar(color = "skyblue3", fill = "skyblue3") +
  scale_y_discrete(labels = Zuordnung) +
  theme_minimal() +
  labs()

print(BarPlot_Hürden_offen)

