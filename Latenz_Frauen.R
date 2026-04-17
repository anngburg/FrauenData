library(tidyverse)
library(xlsx)
library(summarytools)

Frauen_data <- read_csv("Frauen_data.csv")

Frauen_data_mK <- Frauen_data %>% filter(Kontakt == 1)
Frauen_data_oK <- Frauen_data %>% filter(Kontakt == 2)

descr(Frauen_data$Latenz_Wahrnehmung)
  descr(Frauen_data$Latenz_Problembewusst)
  descr(Frauen_data$Latenz_Öffnung)
  descr(Frauen_data$Latenz_Erkenntnis)
  descr(Frauen_data$Latenz_Gesamt)

descr(Frauen_data_mK$Latenz_Wahrnehmung)
  descr(Frauen_data_mK$Latenz_Problembewusst)
  descr(Frauen_data_mK$Latenz_Öffnung)
  descr(Frauen_data_mK$Latenz_Erkenntnis)
  descr(Frauen_data_mK$Latenz_Gesamt)

descr(Frauen_data_oK$Latenz_Wahrnehmung)
  descr(Frauen_data_oK$Latenz_Problembewusst)
  descr(Frauen_data_oK$Latenz_Öffnung)
  descr(Frauen_data_oK$Latenz_Erkenntnis)
  descr(Frauen_data_oK$Latenz_Gesamt)


  MCxLatenz <- Frauen_data %>%
  gather(key = "Hürde", value = "Ausgewählt", c(Hürden_alleineSchaffen:Hürden_Antriebslos, Hürden_DruckBeruf:Hürden_negErwartungen, Hürden_Scham:Hürden_Wartezeiten)) %>% 
  filter(Ausgewählt == 2) %>% 
  group_by(Hürde) %>% 
  summarise(WL = str_c(round(mean(Latenz_Wahrnehmung, na.rm = TRUE), 2), " (", median(Latenz_Wahrnehmung, na.rm = TRUE), "; ", sum(!is.na(Latenz_Wahrnehmung)), ")"), 
            PL = str_c(round(mean(Latenz_Problembewusst, na.rm = TRUE), 2)     , " (", median(Latenz_Problembewusst, na.rm = TRUE)     , "; ", sum(!is.na(Latenz_Problembewusst)), ")"), 
            ÖL = str_c(round(mean(Latenz_Öffnung, na.rm = TRUE), 2)    , " (", median(Latenz_Öffnung, na.rm = TRUE)    , "; ", sum(!is.na(Latenz_Öffnung)), ")"), 
            EL = str_c(round(mean(Latenz_Erkenntnis, na.rm = TRUE), 2)  , " (", median(Latenz_Erkenntnis, na.rm = TRUE)  , "; ", sum(!is.na(Latenz_Erkenntnis)), ")"), 
            GL = str_c(round(mean(Latenz_Gesamt, na.rm = TRUE), 2)      , " (", median(Latenz_Gesamt, na.rm = TRUE)      , "; ", sum(!is.na(Latenz_Gesamt)), ")")
            ) 

write.xlsx(MCxLatenz, "MC x Latenz_Tabelle.xlsx")

####### Hürden Offen X Latenz ######

HüOffxLatenz <- Frauen_data %>%
  gather(key = "Hürde", value = "Ausgewählt", c(Hü_offen_WartezeitTherapeut:Hü_offen_kA)) %>% 
  filter(Ausgewählt == 1) %>% 
  group_by(Hürde) %>% 
  summarise(WL = str_c(round(mean(Latenz_Wahrnehmung, na.rm = TRUE), 2), " (", median(Latenz_Wahrnehmung, na.rm = TRUE), "; ", sum(!is.na(Latenz_Wahrnehmung)), ")"), 
            PL = str_c(round(mean(Latenz_Problembewusst, na.rm = TRUE), 2)     , " (", median(Latenz_Problembewusst, na.rm = TRUE)     , "; ", sum(!is.na(Latenz_Problembewusst)), ")"), 
            ÖL = str_c(round(mean(Latenz_Öffnung, na.rm = TRUE), 2)    , " (", median(Latenz_Öffnung, na.rm = TRUE)    , "; ", sum(!is.na(Latenz_Öffnung)), ")"), 
            EL = str_c(round(mean(Latenz_Erkenntnis, na.rm = TRUE), 2)  , " (", median(Latenz_Erkenntnis, na.rm = TRUE)  , "; ", sum(!is.na(Latenz_Erkenntnis)), ")"), 
            GL = str_c(round(mean(Latenz_Gesamt, na.rm = TRUE), 2)      , " (", median(Latenz_Gesamt, na.rm = TRUE)      , "; ", sum(!is.na(Latenz_Gesamt)), ")")
  ) 

write.xlsx(HüOffxLatenz, "HüOffxLatenz_Tabelle.xlsx")

####### Unterstützung Offen X Latenz ######

UntOffxLatenz <- Frauen_data %>%
  gather(key = "Unterstützung", value = "Ausgewählt", c(Unt_offen_AnderePersonPriv:Unt_offen_kA)) %>% 
  filter(Ausgewählt == 1) %>% 
  group_by(Unterstützung) %>% 
  summarise(WL = str_c(round(mean(Latenz_Wahrnehmung, na.rm = TRUE), 2), " (", median(Latenz_Wahrnehmung, na.rm = TRUE), "; ", sum(!is.na(Latenz_Wahrnehmung)), ")"), 
            PL = str_c(round(mean(Latenz_Problembewusst, na.rm = TRUE), 2)     , " (", median(Latenz_Problembewusst, na.rm = TRUE)     , "; ", sum(!is.na(Latenz_Problembewusst)), ")"), 
            ÖL = str_c(round(mean(Latenz_Öffnung, na.rm = TRUE), 2)    , " (", median(Latenz_Öffnung, na.rm = TRUE)    , "; ", sum(!is.na(Latenz_Öffnung)), ")"), 
            EL = str_c(round(mean(Latenz_Erkenntnis, na.rm = TRUE), 2)  , " (", median(Latenz_Erkenntnis, na.rm = TRUE)  , "; ", sum(!is.na(Latenz_Erkenntnis)), ")"), 
            GL = str_c(round(mean(Latenz_Gesamt, na.rm = TRUE), 2)      , " (", median(Latenz_Gesamt, na.rm = TRUE)      , "; ", sum(!is.na(Latenz_Gesamt)), ")")
  ) 

write.xlsx(UntOffxLatenz, "UntOffxLatenz_Tabelle.xlsx")
####### Grafiken #######

library(ggbeeswarm)
library(ggtext)
library(svglite)
library(systemfonts)

Frauen_data_long <- Frauen_data %>% pivot_longer(cols = c(Latenz_Wahrnehmung, Latenz_Problembewusst,
                                                                      Latenz_Öffnung, Latenz_Erkenntnis, Latenz_Gesamt),
                                                             names_to = "Latenz", values_to = "Value")


Latenz_Boxplots <- ggplot(Frauen_data_long, aes(x = as_factor(Latenz), y = Value, color = Latenz))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3)+
  scale_y_continuous(limits = c(0,1000))+
  theme_minimal()
print(Latenz_Boxplots)

Latenz_densityPlot <- ggplot(Frauen_data_long, aes(x = Value, fill = Latenz, color = Latenz))+
  geom_density(alpha = 0.2)+
  #geom_quasirandom(alpha = 0.2)+
  #scale_x_log10()+
  facet_grid(as.factor(Latenz)~.)+
  scale_x_continuous(limits = c(0, 104), 
                     breaks = c(0,4.33,8.66,12.99,17.32,21.65,25.98,30.31,34.64,38.97,43.30,47.63, 52, 65, 78, 91, 104),
                     labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "15", "18", "21", "24"))+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_line(), 
    panel.grid.minor.x = element_blank())+
  labs(x = "Dauer in Monaten")
print(Latenz_densityPlot)

Latenz_ViolinPlot <- ggplot(Frauen_data_long, aes(x = as_factor(Latenz), y = Value, fill = Latenz, color = Latenz))+
  geom_violin(alpha = 0.3)+
  geom_quasirandom(alpha = 0.3)+
  #scale_x_log10()+
  #facet_grid(Latenz~.)+
  scale_y_continuous(limits = c(1, 800))+
  theme_minimal()
print(Latenz_ViolinPlot)

ggsave(file="BoxPlot.svg", plot=Latenz_Boxplots, width=16, height=9)
ggsave(file="ViolinPlot.svg", plot=Latenz_ViolinPlot, width=16, height=9)
ggsave(file="DensityPlot.svg", plot=Latenz_densityPlot, width=16, height=9)
