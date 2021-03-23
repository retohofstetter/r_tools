#
# Serial mediation order test (based on comparison of indirect effects)
# ####################################################################
# (c) Reto Hofstetter, University of Lucerne, March 23, 2021
# Cite: Hofstetter, Reto, Gabriela Kunath, and Leslie K. John (2020),  From Sweetheart to Scapegoat: Brand Selfie-Taking Shapes Consumer Behavior, Harvard Business School Working Paper, No. 20-085, February 2020. Link: https://hbswk.hbs.edu/item/from-sweetheart-to-scapegoat-brand-selfie-taking-shapes-consumer-behavior
#

library(boot)

data1 <- read_dta("C:/Dropbox/paper brandselfie/1_Studies/3_full causal chain - serial mediation (self-perception)/study 4_data_clean_v2_forR.dta")
n_resamples = 10000

##############################
# First serial mediation order
##############################

#a-path model
model1_1 <- function(d, index){
  coef(lm(selfper  ~ selfie, data = d, subset = index))
}
#d-path model
model1_2 <- function(d, index){
  coef(lm(brandatt  ~ selfper + selfie, data = d, subset = index))
}
#b-path model
model1_3 <- function(d, index){
  coef(lm(purchase_online  ~ brandatt + selfper + selfie, data = d, subset = index))
}

b1_1 <- boot(data1, model1_1, n_resamples)
b1_2 <- boot(data1, model1_2, n_resamples)
b1_3 <- boot(data1, model1_3, n_resamples)

#indirect effect: a_path * d_path * b_path
ind1 <- b1_1$t[,2] * b1_2$t[,2] * b1_3$t[,2]
print(paste0("Indirect effect 1: ", mean(ind1)))

##############################
# Second, alternative, serial mediation order
##############################


#a-path model
model2_1 <- function(d, index){
  coef(lm(brandatt  ~ selfie, data = d, subset = index))
}
#d-path model
model2_2 <- function(d, index){
  coef(lm(selfper  ~ brandatt + selfie, data = d, subset = index))
}
#b-path model
model2_3 <- function(d, index){
  coef(lm(purchase_online  ~ selfper + brandatt + selfie, data = d, subset = index))
}

b2_1 <- boot(data1, model2_1, n_resamples)
b2_2 <- boot(data1, model2_2, n_resamples)
b2_3 <- boot(data1, model2_3, n_resamples)

#indirect effect: a_path * d_path * b_path
ind2 <- b2_1$t[,2] * b2_2$t[,2] * b2_3$t[,2]
print(paste0("Indirect effect 2: ", mean(ind2)))

##############################
# Difference between indirect effects
# If 0 not contained in confidence interval, then null of equal indirect effects can be rejected at p < .05
##############################

diff <- ind1 - ind2
q <- quantile(diff, c(.025, .975)) 

print(paste0("Mean difference: ", mean(diff)))
print(paste0("Bootstrapped CIs: 2.5%: ", q[1], ", 97.5%: ", q[2]))







