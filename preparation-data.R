rm(list=ls())


#setwd("C:/Users/(...)/File Excel S&P") 

dati1927 =read.csv("serie1927.csv") # Historical yahoo data  

## create a matrix with the lagged series (in this case is just for one year (252 days) 

dati_agg_H = matrix(0,504,504) # Raw data
dati_agg_L = matrix(0,504,504) 

dati_agg_log_H = matrix(0,504,504) #Log raw data
dati_agg_log_L = matrix(0,504,504)

dati_diff_logH = matrix(0,503,504) # difference between log prices: log(Pt)- log(Pt-1)
dati_diff_logL = matrix(0,503,504) #  This one has one less record

### Create matrix

for (i in 1:504)
{
  ## From 19 sept. 2016 to 19 sept 2018; 
  # if you add 504 obs to it on Sept. 19, 2018, you get to Sept. 20, 2020
  dati_agg_H[,i] = dati1927[(22284+i):(22787+i),3] 
  dati_agg_L[,i] = dati1927[(22284+i):(22787+i),4]
  
  dati_agg_log_H[,i]= log(dati_agg_H[,i])
  dati_agg_log_L[,i]= log(dati_agg_L[,i]) 
  
  dati_diff_logH[,i] = diff(dati_agg_log_H[,i]) # we're gonna have a record less for the differences
  dati_diff_logL[,i] = diff(dati_agg_log_L[,i])
}

