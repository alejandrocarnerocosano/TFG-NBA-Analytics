library(fixest)

# (1) Model 1: MQO pur sense variables geogràfiques
model1_mqo_base <- feols(W ~ Analysts + Log_Salary + Average_Age + 
                           Coach_Experience + New_Coach + Roster_Continuity + 
                           `Player - Games Injured` + Road_B2B, 
                         data = dades)

# (2) Model 2: EF Equip amb variables geogràfiques
model2_ef_geo <- feols(W ~ Analysts + Log_Salary + Average_Age + 
                         Coach_Experience + New_Coach + Roster_Continuity + 
                         `Player - Games Injured` + Road_B2B + 
                         Market_Size + Tech_ratio + State_tax_rate | Team, 
                       data = dades)

# (3) Model 3: TWFE Definitiu amb variables geogràfiques
model3_twfe_geo <- feols(W ~ Analysts + Log_Salary + Average_Age + 
                           Coach_Experience + New_Coach + Roster_Continuity + 
                           `Player - Games Injured` + Road_B2B + 
                           Market_Size + Tech_ratio + State_tax_rate | Team + Season, 
                         data = dades)

# Generar la taula per pantalla
etable(model1_mqo_base, model2_ef_geo, model3_twfe_geo, 
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       keepFactors = TRUE)
