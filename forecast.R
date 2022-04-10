r_hat_h= matrix(0,503,1)
r_hat_l=matrix(0,503,1)

for (i in 1:503)
{
  r_hat_h[i,]= coeff_1h[i,] + dati_diff_logH[i,504]*coeff_2h[i,] +dati_diff_logL[i,504]*coeff_3h[i,] + EC_H_prev[(i+1),504]*coeff_4h[i,]
  r_hat_l[i,]= coeff_1l[i,] + dati_diff_logL[i,504]*coeff_2l[i,] +dati_diff_logH[i,504]*coeff_3l[i,] + EC_L_prev[(i+1),504]*coeff_4l[i,]
}


dim(coeff_1h)       #503x1
dim(dati_diff_logH) #503x504
dim(EC_H_prev)      #504x504


############ THRESHOLDS

dim(dati_agg_H) #504x504
dim(r_hat_h)    #503x1

soglia_sup=matrix(0,503,1)
soglia_inf=matrix(0,503,1)

for (i in (1:503))
{
  soglia_sup[i,] = dati_agg_H[i+1,504]*exp(r_hat_h[i,])
  soglia_inf[i,] = dati_agg_L[i+1,504]*exp(r_hat_l[i,])
}


dim(soglia_sup) #503x
1

####### TABLE WITH POSSIBLE TRADES
###  Calculate when the price exceeds the estimated threshold

TRADE_SUP = dati1927$High[22789:23291] - soglia_sup[,1]
# 23291 last data avaiable

TRADE_SUP = round(TRADE_SUP, 3)
Data= dati1927$Date[22789:23291] 
TRADE_SUP_m = cbind(Data,TRADE_SUP ) 
TRADE_SUP_mm <-  subset(TRADE_SUP_m, TRADE_SUP_m[,2] > 0) # When the difference is positive, then my price has exceeded the threshold
TRADE_SUP_mm

TRADE_SUP = dati1927$High[22789:23291] - soglia_sup[,1]
# 23291 ultimo dato disponibile
TRADE_SUP = round(TRADE_SUP, 3)
Data= dati1927$Date[22789:23291] 
TRADE_SUP_m = cbind(Data,TRADE_SUP,round(dati1927$High[22789:23291],3), round(soglia_sup[,1],3) ) 
TRADE_SUP_mm <-  subset(TRADE_SUP_m, TRADE_SUP_m[,2] > 0) # When the difference is positive, then my price has exceeded the threshold
TRADE_SUP_mm 



### For the lower threshold
TRADE_INF = soglia_inf[,1] - dati1927$Low[22789:23291] 
TRADE_INF = round(TRADE_INF, 3)
Data= dati1927$Date[22789:23291] 
TRADE_INF_m = cbind(Data,TRADE_INF,round(soglia_inf[,1],3) ) 
TRADE_INF_mm <-  subset(TRADE_INF_m, TRADE_INF_m[,2] > 0) #  When the difference is positive, then my price has exceeded the thresholda
TRADE_INF_mm

### Let's see graphically the signals obtained 
plot(dati1927$Low[22789:23291], type="l", col="dark green",ylab=""  )
lines(soglia_inf[,1], type="l", col="purple")
legend(list(x = 0,y = 3600), col =c("dark green", "purple"), legend = c("P Low","S. Inf"), lty = 1, merge = TRUE) 

###Just te last 100 obs 100 (no signals)
plot(dati1927$Low[23191:23291], type="l", col="dark green", ylab="" )
lines(soglia_inf[403:503,1], type="l", col="purple")
legend(list(x = 60,y = 3000), col =c("dark green", "purple"), legend = c("P Low","S. Inf"), lty = 1, merge = TRUE) 

######## The last 252 days 252 
plot(dati1927$Low[23039:23291], type="l", col="dark green",ylab=""  )
lines(soglia_inf[251:503,1], type="l", col="purple")
legend(list(x = 165,y = 2700), col =c("dark green", "purple"), legend = c("P Low","S. Inf"), lty = 1, merge = TRUE) 



plot(dati1927$High[23039:23291], type="l", col="dark green",ylab=""  )
lines(soglia_sup[251:503,1], type="l", col="purple")
legend(list(x = 160,y = 2700), col =c("dark green", "purple"), legend = c("P. High","S. Sup"), lty = 1, merge = TRUE) 




dati1927$High[22789:23291] - soglia_sup[,1]
