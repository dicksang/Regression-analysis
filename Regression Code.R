library(dplyr)
library(MASS)

rm(list = ls())

setwd("C:\\Users\\Dick Sang\\Desktop\\5. Data Analytics\\3. PolyU RA\\1. Projects\\3. Cust Value Chain Analysis\\4. Regression\\1. 2020-01\\3. analysis")
data <- read.csv("Data Input for Analysis.csv")

str(data)
######################################################
# 1. Test models by absolute word count
######################################################
#--- 1a. By original sales (Symbolic words drive sales)
model <- glm(Sales_K_Unit ~ Functional + 
                            Experimental +
                            Symbolic +
                            Cost +
                            Functional : Optimistic +
                            Experimental : Optimistic +
                            # Symbolic : Optimistic +
                            Cost : Optimistic +
                            Functional : Affective +
                            Experimental : Affective +
                            # Symbolic : Affective +
                            Cost : Affective
          , data = data)
null_model <- glm(Sales_K_Unit ~ 1, data = data)

final_model <- stepAIC(null_model, direction="forward",scope=list(upper=model,lower=null_model))
# final_model <- stepAIC(model, direction="backward")

summary(final_model)

plot(data$Symbolic, data$Sales_K_Unit)
# plot(data$Symbolic * data$Affective, data$Sales_K_Unit)
# plot(data$Symbolic * data$Optimistic, data$Sales_K_Unit)

#--- 1b. By Adjusted sales (No variables are significant - Not studied further)
# model <- glm(Adj_Sales ~ Functional + Experimental + Symbolic+ Cost, data = data)
# null_model <- glm(Adj_Sales ~ 1, data = data)
# 
# fwd_model <- stepAIC(null_model, direction="forward",scope=list(upper=model,lower=null_model))
# 
# summary(fwd_model)
######################################################
# 2. Test models by absolute word count %
######################################################
#--- 2a. By original sales (Normalized Cost words drive sales)
model <- glm(Sales_K_Unit ~ WC_Normalize_Functional + 
                            WC_Normalize_Experimental + 
                            WC_Normalize_Symbolic + 
                            WC_Normalize_Cost +
                            WC_Normalize_Functional: WC_Normalize_Optimistic+ 
                            WC_Normalize_Experimental: WC_Normalize_Optimistic+ 
                            WC_Normalize_Symbolic: WC_Normalize_Optimistic+ 
                            WC_Normalize_Cost: WC_Normalize_Optimistic+
                            WC_Normalize_Functional: WC_Normalize_Affective+ 
                            WC_Normalize_Experimental: WC_Normalize_Affective+ 
                            WC_Normalize_Symbolic: WC_Normalize_Affective+ 
                            WC_Normalize_Cost: WC_Normalize_Affective
             , data = data)

null_model <- glm(Sales_K_Unit ~ 1, data = data)

final_model <- stepAIC(null_model, direction="forward",scope=list(upper=model,lower=null_model))

summary(final_model)

plot(data$WC_Normalize_Cost, data$Sales_K_Unit)

#--- 2b. By Adjusted sales (No variables are significant - Not studied further)
# model <- glm(Adj_Sales ~ WC_Normalize_Functional + 
#                          WC_Normalize_Experimental + 
#                          WC_Normalize_Symbolic+ 
#                          WC_Normalize_Cost, data = data)
# 
# null_model <- glm(Adj_Sales ~ 1, data = data)
# 
# fwd_model <- stepAIC(null_model, direction="forward",scope=list(upper=model,lower=null_model))
# 
# summary(fwd_model)
######################################################
# 3. Composite model from 1, 2 (Not significant)
######################################################
model2 <- glm(Sales_K_Unit ~ Symbolic +
                             WC_Normalize_Cost
              , data = data)

summary(model2)

#-- cannot combine
model3 <- glm(Sales_K_Unit ~ Symbolic +
                             Cost
              , data = data)

summary(model3)

#-- cannot combine
model4 <- glm(Sales_K_Unit ~ WC_Normalize_Symbolic +
                             WC_Normalize_Cost
              , data = data)

summary(model4)