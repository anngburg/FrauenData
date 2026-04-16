library(tidyverse)
library(glmnet)
library(anngburg)
library(logistf)
library(pROC)

Frauen_data <- read_csv("Frauen_data.csv")

Frauen_data_subset <- Frauen_data %>% dplyr::select(Kontakt, IDQ_ConscAv_Sum, MHSAS_MW, Veränderung_P_MW, Veränderung_S_MW, 
                                                    ProzEinschränkung, SO_spez_kont #nur signifikant unterschiedliche Konstrukte zw. den Gruppen
                                                   ) %>% na.omit(.) %>% mutate(Kontakt = 2 - Kontakt)

Frauen_data_subset2 <- Frauen_data %>% dplyr::select(SO_spez_dich, PHQ2_Sum, IDQ_DenialNeg_Sum, IDQ_ResistCh_Sum, IDQ_ConscAv_Sum, MHSAS_MW, DepSter_Lit_MW, 
                                                     DepSter_Mis_MW, UBogen_KA_MW, SIAS_MW, KÜ_ext_MW, KÜ_int_MW, SELSA_Soz_MW, SELSA_Fam_MW,
                                                     SELSA_Rom_MW, Alexithymie_MW, PAREMO_MW, Veränderung_P_MW, Veränderung_S_MW, 
                                                     NEOffi_Neur_MW, NEOffi_Ext_MW, Optimismus, Pessimismus, BSSS_need_MW, BSSS_seek_MW, BSSS_receive_MW, SozU_MW,
                                                     ProzEinschränkung, VergleichAndere, SO_spez_kont, SO_allgemein
                                                    ) %>% na.omit(.) %>% mutate(SO_spez_dich = 2 - SO_spez_dich) %>% mutate(SO_spez_dich = as.integer(SO_spez_dich))

####### AV: Prof. Hilfe #######
#Basismodell
base_model_01 <- glm(Kontakt ~ IDQ_ConscAv_Sum + MHSAS_MW + Veränderung_P_MW + Veränderung_S_MW + ProzEinschränkung, 
                     data = Frauen_data_subset, 
                     family = binomial(link = "logit"))

anngburg::function_logReg_Tabelle(base_model_01, Frauen_data_subset, "Kontakt")

#LASSO
IVs_01 <- Frauen_data_subset %>% dplyr::select(-Kontakt)
DV_01 <- Frauen_data_subset$Kontakt
IVs_01 <- as.matrix(IVs_01)
lasso_fit_01 <- cv.glmnet(IVs_01, DV_01, family = "binomial", alpha = 1)
coef(lasso_fit_01, s = "lambda.min")

#keine Variablen ausgeschlossen verglichen mit dem base_model, daher direkt zu BIC-Reduktion

#lasso.min_model_01 <- glm(Kontakt ~ ,
                          #data = Frauen_data_subset, 
                          #family = binomial(link = "logit"))

#anngburg::function_logReg_Tabelle(lasso.min_model_01, Frauen_data_subset, "Kontakt")

#BIC-Kriterium
BIC_model_01 <- step(base_model_01, direction = "both", k = log(13))

anngburg::function_logReg_Tabelle(BIC_model_01, Frauen_data_subset, "Kontakt")

####### AV: Priv. Offenbarung #######
#Basismodell
base_model_02 <- glm(SO_spez_dich ~ PHQ2_Sum + IDQ_DenialNeg_Sum + IDQ_ResistCh_Sum + IDQ_ConscAv_Sum + MHSAS_MW + DepSter_Lit_MW + 
                     DepSter_Mis_MW + UBogen_KA_MW + SIAS_MW + KÜ_ext_MW + KÜ_int_MW + SELSA_Soz_MW + SELSA_Fam_MW +
                     SELSA_Rom_MW + Alexithymie_MW + PAREMO_MW + Veränderung_P_MW + Veränderung_S_MW + 
                     NEOffi_Neur_MW + NEOffi_Ext_MW + Optimismus + Pessimismus + BSSS_need_MW + BSSS_seek_MW + BSSS_receive_MW + SozU_MW +
                     ProzEinschränkung + VergleichAndere + SO_allgemein,
                     data = Frauen_data_subset2, 
                     family = binomial(link = "logit"))

anngburg::function_logReg_Tabelle(base_model_02, Frauen_data_subset2, "SO_spez_dich")

#LASSO
IVs_02 <- Frauen_data_subset2 %>% dplyr::select(-SO_spez_dich, -SO_spez_kont)
DV_02 <- Frauen_data_subset2$SO_spez_dich
IVs_02 <- as.matrix(IVs_02)
lasso_fit_02 <- cv.glmnet(IVs_02, DV_02, family = "binomial", alpha = 1)
coef(lasso_fit_02, s = "lambda.min")

lasso.min_model_02 <- glm(SO_spez_dich ~ IDQ_DenialNeg_Sum + MHSAS_MW + DepSter_Lit_MW + 
                          DepSter_Mis_MW + Veränderung_P_MW + Veränderung_S_MW + SO_allgemein,
                          data = Frauen_data_subset2, 
                          family = binomial(link = "logit"))

anngburg::function_logReg_Tabelle(lasso.min_model_02, Frauen_data_subset2, "SO_spez_dich")

#BIC-Kriterium
lasso.minBIC_model_02 <- step(lasso.min_model_02, direction = "both", k = log(12))

anngburg::function_logReg_Tabelle(lasso.minBIC_model_02, Frauen_data_subset2, "SO_spez_dich")


###### Firth's penalized logistic regression ####### braucht es nicht, weil Ergebnisse identisch zu normaler logReg sind
 