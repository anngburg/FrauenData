library(tidyverse)

Frauen_data <- read_csv("Frauen_data.csv")

UntLZ_data_long <- Frauen_data %>% pivot_longer(cols = c(Unt_offen_AnderePersonPriv:Unt_offen_kA), names_to = "Unterstützung", values_to = "Value"
                                                ) %>% select(Unterstützung, Value, Latenz_Gesamt) %>% na.omit(.) %>% select(-Value)

Unt_Zuordnung <- c("Unt_offen_AnderePersonPriv" = "Andere Person (privat)",	
                   "Unt_offen_InstitutionelleProfHilfe" = "Institutionelle/ prof. Hilfsangebote",	
                   "Unt_offen_StarkeSymptome" = "Starke Symptome",
                   "Unt_offen_negativeAuswiArbeit" = "Negative Auswirkungen auf Arbeit/Studium/etc.",
                   "Unt_offen_einschneidendeEreignis" = "Einschneidende Lebens- ereignisse",	
                   "Unt_offen_Zusammenbruch" = "Zusammenbruch",
                   "Unt_offen_Suizidalität" = "Suizidalität",
                   "Unt_offen_Sonstiges" = "Sonstiges",
                   "Unt_offen_Keine" = "Keine Unterstützung",
                   "Unt_offen_kA" = "Keine Angabe")

UntLZ_data_long <- UntLZ_data_long %>%
  mutate(Unterstützung = factor(Unterstützung, levels = names(Unt_Zuordnung)))

#kombinierten Plot erstellen
UnterstützungXLatenz <- ggplot() +
  geom_hline(yintercept = 166 / 17, color = "skyblue3", linewidth = 1, alpha = 0.5, linetype = 3) +
  geom_hline(yintercept = 44 / 15, color = "skyblue3", linewidth = 1, alpha = 0.5)+
  # Bar graph for counts on primary y-axis
  geom_bar(data = UntLZ_data_long, aes(x = Unterstützung), stat = "count", fill = "palegreen3", alpha = 0.5) +
  # Boxplots on secondary y-axis
  geom_boxplot(data = UntLZ_data_long, aes(x = Unterstützung, y = Latenz_Gesamt / (800/80)), fill = "lightblue", color = "skyblue4", width=0.4, linewidth = 0.8) + #definiert die Boxplots
  stat_boxplot(data = UntLZ_data_long, aes(x = Unterstützung, y = Latenz_Gesamt / (800/80)), geom = "errorbar", color = "skyblue4", width=0.2, linewidth = 0.8) + #fügt errorbars hinzu
  stat_summary(data = UntLZ_data_long, aes(x = Unterstützung, y = Latenz_Gesamt / (800/80)), fun.y=mean, geom="point", shape=1, size=3, color="skyblue4") + #fügt einen Kreis beim Mittelwert hinzu
  # Adjust scales
  scale_x_discrete(labels = function(x) str_wrap(Unt_Zuordnung[x], width = 15)) +
  scale_y_continuous(
    name = "Häufigkeit der Nennungen", breaks=seq(0, 80, by = 10),
    limits = c(0, 80),
    sec.axis = sec_axis(~ . * (800/80), name="Gesamtlatenz in Wochen", breaks=seq(0, 800, by = 200)) #definiert die zwei y-Achsen
  ) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size=20, angle = 0, hjust = 0.5,margin=margin(t=-15), color="grey30"),
        axis.text.y.left = element_text(size=20, color="darkgreen"),
        axis.text.y.right = element_text(size=20, color="skyblue4"),
        axis.title.y.left = element_text(size = 24, color="darkgreen"),
        axis.title.y.right = element_text(size = 24, color="skyblue4"),
        axis.title.x=element_text(size=20, color="grey30"),
        axis.ticks.x = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.background = element_rect(fill = "white"),  # Background color of entire plot
        panel.background = element_rect(fill = "white"), # Background color of plotting area
        panel.grid.major.y=element_line(color="gray90"), # Major horizontal grid lines
        panel.grid.minor.y=element_line(color="gray90"), # Minor horizontal grid lines
        panel.grid.major.x=element_blank(),              # Remove major vertical grid lines
        panel.grid.minor.x=element_blank()               # Remove minor vertical grid lines
  ) +
  labs(title = "Gesamtlatenz x Unterstützung", x = "")

UnterstützungXLatenz
ggsave("UnterstützungXLatenz.svg", UnterstützungXLatenz, width = 28, height = 15)
