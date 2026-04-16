install.packages("svglite")
library(tidyverse)
library(svglite)

######## Boxplots nach profHilfe ########
Frauen_data <- read_csv("Frauen_data.csv")

data_long_all1 <- Frauen_data %>% pivot_longer(cols = c("PHQ2_Sum", "IDQ_DenialNeg_Sum", "IDQ_ResistCh_Sum", "IDQ_ConscAv_Sum", "MHSAS_MW", "DepSter_Lit_MW", 
                                                        "DepSter_Mis_MW", "UBogen_KA_MW", "SIAS_MW", "KÜ_ext_MW", "KÜ_int_MW", "SELSA_Soz_MW", "SELSA_Fam_MW",
                                                        "SELSA_Rom_MW", "Alexithymie_MW", "PAREMO_MW", "Veränderung_P_MW", "Veränderung_S_MW", 
                                                        "NEOffi_Neur_MW", "NEOffi_Ext_MW", "Optimismus", "Pessimismus", 
                                                        "BSSS_need_MW", "BSSS_seek_MW", "BSSS_receive_MW", "SozU_MW",
                                                        "ProzEinschränkung", "VergleichAndere", "SO_spez_kont", "SO_allgemein"
                                                        ), names_to = "Variable", values_to = "Value") %>% select(Variable, Value, profHilfe)

my_fill <- c("1" = "#F8766D", "2" = "#F8766D", "3" = "#F8766D", "4" = "#F8766D", "5" = "#00BFC4")
my_color <- c("1" = "#f74c40", "2" = "#b5423a", "3" = "#87251e", "4" = "#57211d", "5" = "#029599")

facetPlot1 <- ggplot(data_long_all1, aes(x=as.factor(profHilfe), y=Value, color = as.factor(profHilfe), fill = as.factor(profHilfe)))+
  geom_jitter(alpha = 0.2, size = 2)+
  geom_boxplot(alpha = 0.5, size = 0.8)+
  stat_summary(fun.y = mean, geom = "point", shape = 21, size = 3)+
  stat_boxplot(geom ="errorbar", width = 0.4, linewidth = 1)+
  facet_wrap(~ Variable, ncol=4, scales="free_y") + # Create a grid with three columns
  labs(title="\nVergleich der erhobenen Konstrukte nach Status des Hilfesuchens\n",
       x="",
       y="Skalenwert",
       color="Hilfegesuch",
       fill="Hilfegesuch")+
  scale_color_manual(values = my_color,
                     label=c("1" = "Ja, ich habe in der Vergangenheit eine solche\nBehandlung in Anspruch ngenommen (n = 56)",
                             "2" = "Ja, ich bin aktuell in einer solchen\nBehandlung (n = 72)",
                             "3" = "Ja, ich hatte ein Erst- bzw. Beratungs-\ngespräch (n = 20)",
                             "4" = "Nein, aber ich habe mich aufgrund meiner Gemüts-\nlage an meine Hausarztpraxis gewandt (n = 4)",
                             "5" = "Nein, ich habe keine solche Hilfe in Anspruch\ngenommen (n = 27)"
                     )) + 
  scale_fill_manual(values = my_fill,
                    label=c("1" = "Ja, ich habe in der Vergangenheit eine solche\nBehandlung in Anspruch ngenommen (n = 56)",
                            "2" = "Ja, ich bin aktuell in einer solchen\nBehandlung (n = 72)",
                            "3" = "Ja, ich hatte ein Erst- bzw. Beratungs-\ngespräch (n = 20)",
                            "4" = "Nein, aber ich habe mich aufgrund meiner Gemüts-\nlage an meine Hausarztpraxis gewandt (n = 4)",
                            "5" = "Nein, ich habe keine solche Hilfe in Anspruch\ngenommen (n = 27)"
                    )) +
  theme_light()+
  theme(axis.text.x=element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.title = element_text(size = 40, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.y = element_text(size = 30, color = "grey40"),
        axis.title.y = element_text(size = 34, color = "grey40"),
        legend.title =element_text(size = 30, color = "grey40", face = "bold"),
        legend.text = element_text(size = 28, color = "grey40"),
        legend.position = "inside",
        legend.position.inside = c(.69,.06), #POSITION MUSS NOCH ANGEPASST WERDEN
        legend.key.spacing.y = unit(10, "pt"),
        strip.text = element_text(size = 30))

#print(facetPlot1)

ggsave("FacetPlot1.svg", facetPlot1, width = 30, height = 45)

######## Boxplots nach mit/ohne Kontakt ########

data_long_all2 <- Frauen_data %>% pivot_longer(cols = c("PHQ2_Sum", "IDQ_DenialNeg_Sum", "IDQ_ResistCh_Sum", "IDQ_ConscAv_Sum", "MHSAS_MW", "DepSter_Lit_MW", 
                                                        "DepSter_Mis_MW", "UBogen_KA_MW", "SIAS_MW", "KÜ_ext_MW", "KÜ_int_MW", "SELSA_Soz_MW", "SELSA_Fam_MW",
                                                        "SELSA_Rom_MW", "Alexithymie_MW", "PAREMO_MW", "Veränderung_P_MW", "Veränderung_S_MW", 
                                                        "NEOffi_Neur_MW", "NEOffi_Ext_MW", "Optimismus", "Pessimismus", 
                                                        "BSSS_need_MW", "BSSS_seek_MW", "BSSS_receive_MW", "SozU_MW",
                                                        "ProzEinschränkung", "VergleichAndere", "SO_spez_kont", "SO_allgemein"
                                                        ), names_to = "Variable", values_to = "Value") %>% select(Variable, Value, Kontakt)

facetPlot2 <- ggplot(data_long_all2, aes(x=as.factor(Kontakt), y=Value, color = as.factor(Kontakt), fill = as.factor(Kontakt)))+
  geom_jitter(alpha = 0.2, size = 2)+
  geom_boxplot(alpha = 0.5, size = 1)+
  stat_summary(fun.y = mean, geom = "point", shape = 21, size = 3)+
  stat_boxplot(geom ="errorbar", width = 0.4, linewidth = 1)+
  facet_wrap(~ Variable, ncol=4, scales="free_y") + 
  labs(title="\nVergleich der erhobenen Konstrukte nach Status des Hilfesuchens\n",
       x="",
       y="Skalenwert",
       color="Hilfegesuch (dichotom)",
       fill="Hilfegesuch (dichotom)")+
  scale_color_discrete(label=c("1" = "Kontakt aufgenommen\nzu Hilfsangeboten (n = 152)",
                               "2" = "keine Unterstützung\naufgesucht (n = 27)"
  )) + 
  scale_fill_discrete(label=c("1" = "Kontakt aufgenommen\nzu Hilfsangeboten (n = 152)",
                              "2" = "keine Unterstützung\naufgesucht (n = 27)"
  )) +
  theme_light()+
  theme(axis.text.x=element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.title = element_text(size = 40, hjust = 0.5, color = "grey20", face = "bold"),
        axis.text.y = element_text(size = 30, color = "grey40"),
        axis.title.y = element_text(size = 34, color = "grey40"),
        legend.title =element_text(size = 34, color = "grey40", face = "bold"),
        legend.text = element_text(size = 34, color = "grey40"),
        legend.position = "inside",
        legend.position.inside = c(.64,.08), #POSITION MUSS NOCH ANGEPASST WERDEN
        legend.key.spacing.y = unit(10, "pt"),
        strip.text = element_text(size = 30))

#print(facetPlot2)

ggsave("FacetPlot2.svg", facetPlot2, width = 27, height = 45)
