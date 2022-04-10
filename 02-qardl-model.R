### BUILDING MODELS ON LEVELS

library(quantreg)

EC_H= matrix(0,503,504)
EC_L= matrix(0,503,504)

EC_H_prev= matrix(0,504,504)
EC_L_prev= matrix(0,504,504)


for (i in 1:504)
{
  EC_H[,i]= rq(dati_agg_log_H[,i]~ dati_agg_log_L[,i], tau=0.75 )$residuals[1:503]
  EC_L[,i]= rq(dati_agg_log_H[,i]~ dati_agg_log_L[,i], tau=0.25 )$residuals[1:503]
  
  EC_H_prev[,i]= rq(dati_agg_log_H[,i]~ dati_agg_log_L[,i], tau=0.75 )$residuals
  EC_L_prev[,i]= rq(dati_agg_log_H[,i]~ dati_agg_log_L[,i], tau=0.25 )$residuals
  
}  

# Residuals from [1:503] I use them when I go to put them in the dynamic model.
# The residuals from [1:504] I have to use when I go to make the forecast.
#When I make the forecast I can't use EC_H and EC_L at the end of the period because it contains information that has already been used to estimate the parameters.


############## DYNAMIC MODEL 

coeff_1h=matrix(0,503,1)
coeff_2h=matrix(0,503,1)
coeff_3h=matrix(0,503,1)
coeff_4h=matrix(0,503,1)


coeff_1l=matrix(0,503,1)
coeff_2l=matrix(0,503,1)
coeff_3l=matrix(0,503,1)
coeff_4l=matrix(0,503,1)


for (i in 1:503)
{
  coeff_1h[i,] = rq(dati_diff_logH[,i+1]~ +dati_diff_logH[,i] +dati_diff_logL[,i] +EC_H[,i],  tau=0.75)$coefficients[1]
  coeff_2h[i,] = rq(dati_diff_logH[,i+1]~ +dati_diff_logH[,i] +dati_diff_logL[,i] +EC_H[,i],  tau=0.75)$coefficients[2]
  coeff_3h[i,] = rq(dati_diff_logH[,i+1]~ +dati_diff_logH[,i] +dati_diff_logL[,i] +EC_H[,i],  tau=0.75)$coefficients[3]
  coeff_4h[i,] = rq(dati_diff_logH[,i+1]~ +dati_diff_logH[,i] +dati_diff_logL[,i] +EC_H[,i],  tau=0.75)$coefficients[4]
  
  coeff_1l[i,] = rq(dati_diff_logL[,i+1]~ +dati_diff_logL[,i] +dati_diff_logH[,i] +EC_L[,i],  tau=0.25)$coefficients[1]
  coeff_2l[i,] = rq(dati_diff_logL[,i+1]~ +dati_diff_logL[,i] +dati_diff_logH[,i] +EC_L[,i],  tau=0.25)$coefficients[2]
  coeff_3l[i,] = rq(dati_diff_logL[,i+1]~ +dati_diff_logL[,i] +dati_diff_logH[,i] +EC_L[,i],  tau=0.25)$coefficients[3]
  coeff_4l[i,] = rq(dati_diff_logL[,i+1]~ +dati_diff_logL[,i] +dati_diff_logH[,i] +EC_L[,i],  tau=0.25)$coefficients[4]
}


dim(dati_diff_logL) # 503x504
