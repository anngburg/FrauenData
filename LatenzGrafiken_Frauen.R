#install.packages("svglite")
#install.packages("systemfonts")
#install.packages("ggbeeswarm")
#install.packages("ggtext")
library(tidyverse)
library(ggbeeswarm)
library(ggtext)
library(svglite)
library(systemfonts)


Frauen_data <- read_csv("Frauen_data.csv")

Frauen_data_long <- Frauen_data %>% pivot_longer(cols = c(Latenz_Wahrnehmung, Latenz_Problembewusst,
                                                          Latenz_Öffnung, Latenz_Erkenntnis, Latenz_Gesamt),
                                                          names_to = "Latenz", values_to = "Value")

Latenz_Beeswarm <- ggplot(Frauen_data_long, aes(x = as_factor(Latenz), y = Value, color = Latenz))+
  geom_quasirandom(alpha = 0.6)+
  geom_jitter(alpha = 0.2)+
  theme_minimal()+
  scale_y_continuous(limits = c(1,1000))
#scale_y_log10()
print(Latenz_Beeswarm)

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

ggsave("ViolinPlot.svg", Latenz_ViolinPlot, width = 16, height = 9)
ggsave("DensityPlot.svg", Latenz_densityPlot, width = 16, height = 9)

