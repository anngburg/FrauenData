library(psych)
library(car)

Frauen_data <- read_csv("Frauen_data.csv")

Deskriptionen_Tabelle_all <- readRDS("Deskriptionen_Tabelle_all.rds")

#tTest Ergebnisse an große Deskriptionen Tabelle anhängen
t_Test_Ergebnisse_berechnen <- function(data) {
  
  t_Test_Ergebnisse <- matrix(NA, nrow = 31, ncol = 4)
  colnames(t_Test_Ergebnisse) <- c("t Statistik", "df", "p-Wert", "Cohen's d")
  
  konstrukte <- c("SD_Alter", "PHQ2_Sum", "IDQ_DenialNeg_Sum", "IDQ_ResistCh_Sum", "IDQ_ConscAv_Sum", "MHSAS_MW", "DepSter_Lit_MW", 
                  "DepSter_Mis_MW", "UBogen_KA_MW", "SIAS_MW", "KÜ_ext_MW", "KÜ_int_MW", "SELSA_Soz_MW", "SELSA_Fam_MW",
                  "SELSA_Rom_MW", "Alexithymie_MW", "PAREMO_MW", "Veränderung_P_MW", "Veränderung_S_MW", 
                  "NEOffi_Neur_MW", "NEOffi_Ext_MW", "Optimismus", "Pessimismus", "BSSS_need_MW", "BSSS_seek_MW", "BSSS_receive_MW", "SozU_MW",
                  "ProzEinschränkung", "VergleichAndere", "SO_spez_kont", "SO_allgemein")

  
  for (i in seq_along(konstrukte)) { #for-Loop, die für jedes Konstrukt den t-Test berechnet und in die Matrix überträgt
    konstrukt_name <- konstrukte[i]
    
    levene_result <- car::leveneTest(data[[konstrukt_name]], data$Kontakt) #für jedes Konstrukt soll ein Leven-test berechnet und das Ergebnis in "levene_result" gespeichert werden
    levene_p_value <- levene_result$"Pr(>F)"[1] #dann wird der p-Wert extrahiert
    
    var_equal <- ifelse(levene_p_value < 0.05, FALSE, TRUE) #die Variable var_equal wird aus dem extrahierten p-Wert generiert und bei Signifikanz auf FALSE gesetzt
    
    formula <- as.formula(paste(konstrukt_name, "~ Kontakt"))
    
    # Perform t-test with determined var.equal value
    t_test_result <- t.test(formula, data = data, var.equal = var_equal)
    cohens_d_result <- cohen.d(data[[konstrukt_name]], data$Kontakt)
    
    #t_test_result <- t.test(reformulate("Kontakt"), data = data[konstrukt_name], var.equal = var_equal) #hier wird der T-test berechnet, der sich die var.equal Bedingung zieht
    
    t_value <- round(t_test_result$statistic, 3) #t-Statistik extrahieren
    df <- round(t_test_result$parameter, 2) #Freiheitsgrade extrahieren
    p_value <- round(t_test_result$p.value, 3) #p-Wert extrahieren 
    p_value <- sprintf("%.3f", p_value) # Format without leading zero
    p_value <- sub("^0\\.", ".", p_value) # Remove leading '0.'
    cohens_d <- abs(round(cohens_d_result$cohen.d[,2], 2)) #d-Wert extrahieren
    
    t_Test_Ergebnisse[i, ] <- c(t_value, df, p_value, cohens_d) #die exrahierten Statistiken in der Matrix speichern
  }
  
  return(t_Test_Ergebnisse)
}


t_Test_Ergebnisse <- t_Test_Ergebnisse_berechnen(Frauen_data)

Deskriptionen_Reliabilitäten_tTest <- cbind(Deskriptionen_Tabelle_all, t_Test_Ergebnisse)
colnames(Deskriptionen_Reliabilitäten_tTest) <- c("N (mK)", "Rel. (mK)", "MW (mK)", "SD (mK)", "Emp. Spanne (mK)",
                                                  "N (oK)", "Rel. (oK)", "MW (oK)", "SD (oK)", "Emp. Spanne (oK)", 
                                                  "t Statistik", "df", "p-Wert","Cohen's d")

write.xlsx(Deskriptionen_Reliabilitäten_tTest, "Deskriptionen, Reliabilitäten & tTests_Frauen.xlsx", sheetName = "alle", col.names = TRUE, row.names = TRUE, showNA = FALSE)
