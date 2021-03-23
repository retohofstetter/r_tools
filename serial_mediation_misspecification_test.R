#
# Serial mediation misspecification test
# ########################################
# jtest performs the Davidson-MacKinnon J test for comparing non-nested models.
#
# The idea of the J test is the following: if the first model contains the correct set of regressors, 
# then including the fitted values of the second model into the set of regressors should provide no significant 
# improvement. But if it does, it can be concluded that model 1 does not contain the correct set of regressors.
#
# Source: http://math.furman.edu/~dcs/courses/math47/R/library/lmtest/html/jtest.html
# Cite for test: R. Davidson & J. MacKinnon (1981). Several Tests for Model Specification in the Presence of Alternative Hypotheses. Econometrica, 49, 781-793.
#
# (c) Reto Hofstetter, University of Lucerne, March 23, 2021
# Cite for application of test: Hofstetter, Reto, Gabriela Kunath, and Leslie K. John (2020),  From Sweetheart to Scapegoat: Brand Selfie-Taking Shapes Consumer Behavior, Harvard Business School Working Paper, No. 20-085, February 2020. Link: https://hbswk.hbs.edu/item/from-sweetheart-to-scapegoat-brand-selfie-taking-shapes-consumer-behavior


library(tidyverse)
library(haven)
library(lmtest)

data1 <- read_dta("C:/Dropbox/paper brandselfie/1_Studies/3_full causal chain - serial mediation (self-perception)/study 4_data_clean_v2_forR.dta")

#first single mediation model (fm2) vs. serial mediation model (fm1)
fm1 <- lm(purchase_online  ~ brandatt + selfie, data = data1)
fm2 <- lm(purchase_online  ~ brandatt + selfper + selfie, data = data1)

jtest(fm1, fm2)

#second single mediation model (fm2) vs. serial mediation model (fm1)
fm1 <- lm(purchase_online  ~ selfper + selfie, data = data1)
fm2 <- lm(purchase_online  ~ brandatt + selfper + selfie, data = data1)

jtest(fm1, fm2)

