library(dplyr)
library(MASS)
library(olsrr)

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

# final_model <- stepAIC(null_model, direction="forward",scope=list(upper=model,lower=null_model))
final_model <- stepAIC(model, direction="both")
##########################################################
# fine-tune final model
##########################################################
final_model <- lm(Sales_K_Unit ~ Cost + Cost:Optimistic
                   , data = data)

summary(final_model)

# model assumption - 
# 1. residual is normal: pass
ols_test_normality(final_model$residuals)$kolmogorv

plot(final_model$fitted.values, final_model$residuals)
plot(final_model) # plot 1 is fitted vs residual, plot 2 is QQ plot: fairly normal residuals

# 2. Homoskedasticity: pass - variance is constant
ols_test_breusch_pagan(final_model)

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
plot(data$WC_Normalize_Cost, data$Sales_K_Unit)

#--- 2a. By original sales (Normalized Cost words drive sales)
model2 <- glm(Sales_K_Unit ~ 
                            # WC_Normalize_Functional + 
                            WC_Normalize_Experimental + 
                            # WC_Normalize_Symbolic + 
                            WC_Normalize_Cost +
                            # WC_Normalize_Functional: WC_Normalize_Optimistic+ 
                            # WC_Normalize_Experimental: WC_Normalize_Optimistic+ 
                            # WC_Normalize_Symbolic: WC_Normalize_Optimistic+ 
                            WC_Normalize_Cost: WC_Normalize_Optimistic
                            # WC_Normalize_Functional: WC_Normalize_Affective+ 
                            # WC_Normalize_Experimental: WC_Normalize_Affective
                            # WC_Normalize_Symbolic: WC_Normalize_Affective+ 
                            # WC_Normalize_Cost: WC_Normalize_Affective
             , data = data)

null_model2 <- glm(Sales_K_Unit ~ 1, data = data)

# final_model <- stepAIC(null_model, direction="forward",scope=list(upper=model,lower=null_model))
final_model2 <- stepAIC(model2, direction="backward")

summary(final_model2)
##########################################################
# fine-tune final model
##########################################################
final_model2 <- lm(Sales_K_Unit ~ WC_Normalize_Cost
                  , data = data)

summary(final_model2)

# model assumption - 
# 1. residual is normal: pass
ols_test_normality(final_model2$residuals)$kolmogorv

plot(final_model2$fitted.values, final_model2$residuals)
plot(final_model2) # plot 1 is fitted vs residual, plot 2 is QQ plot: depart from normal residuals

# 2. Homoskedasticity: pass - variance is constant
ols_test_breusch_pagan(final_model2)

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