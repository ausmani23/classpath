#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Class as mediating variable
Calibrated simulation
DZ
"""
#Packages
import numpy as np
import pandas as pd # data manipulation
import matplotlib.pyplot as plt
import statistics
import seaborn as sns
import dc_stat_think as dcst # ecdf estimation


np.random.seed(3) # fix random seed

# Race -> Class
#----------------------
p_black       = 0.15 # proportion of black population
p_class_black = np.array([18.95, 66.30, 12.63, 2.11])/100
p_class_white = np.array([12.13, 59.46, 23.75, 4.65])/100

p_class_black = p_class_black / np.sum(p_class_black )
p_class_white = p_class_white / np.sum(p_class_white )


# Race, Class -> Outcome
#----------------------
y_class_black_median = np.array([3500, 33000, 60000, 35950])
y_class_white_median = np.array([8300, 49000, 80000, 66000])

y_class_black_mean = np.array([13144, 41843, 76287, 62900])
y_class_white_mean = np.array([25293, 63433, 112858, 119976])


# Fixed parameters
#----------------------
n                   = int(1e5) # number of draws
n_black = int( round(p_black * n) )
n_white= n - n_black


mu_class_black = np.log( y_class_black_median )
mu_class_white = np.log( y_class_white_median )

s_class_black = np.sqrt(  2 * np.divide(y_class_black_mean,y_class_black_median) ) #CHECK
s_class_white = np.sqrt(  2 * np.divide(y_class_white_mean,y_class_white_median) )



# Simulate
#----------------------
# Sample black population
y_black = np.empty((n_black,1))
c_black = np.random.choice(4, n_black, replace=True, p=p_class_black)
for k in range(0,n_black-1):
    y_black[k] = np.random.lognormal(mu_class_black[c_black[k]], s_class_black[c_black[k]])  

# Sample white population
y_white = np.empty((n_white,1))
c_white = np.random.choice(4, n_white, replace=True, p=p_class_white)
for k in range(0,n_white-1):
    y_white[k] = np.random.lognormal(mu_class_white[c_white[k]], s_class_white[c_white[k]])  
        

#Statistics
#-----------------
quantile_black_white_50_50 = np.quantile(y_black, 0.50)/np.quantile(y_white, 0.50)
quantile_black_white_20_20 = np.quantile(y_black, 0.20)/np.quantile(y_white, 0.20)
quantile_black_white_80_80 = np.quantile(y_black, 0.80)/np.quantile(y_white, 0.80)

quantile_black_black_80_20 = np.quantile(y_black, 0.80)/np.quantile(y_black, 0.20)
quantile_white_white_80_20 = np.quantile(y_white, 0.80)/np.quantile(y_white, 0.20)


#Print
#-----------------
print(' ')


print('Inter-racial inequality:')
print('------------------------')
print('Black Q20/ White Q20: {:}'.format(quantile_black_white_20_20))
print('Black Q50/ White Q50: {:}'.format(quantile_black_white_50_50))
print('Black Q80/ White Q80: {:}'.format(quantile_black_white_80_80))

print(' ')

print('Black inequality:')
print('------------------------')
print('Black Q80/ Black Q20: {:}'.format(quantile_black_black_80_20))

print(' ')


print('White inequality:')
print('------------------------')
print('White Q80/ White Q20: {:}'.format(quantile_white_white_80_20))

print(' ')


# Plot ECDF
#-----------------

x_cdf_white, y_cdf_white = dcst.ecdf(pd.Series(y_white.flatten()))
x_cdf_black, y_cdf_black = dcst.ecdf(pd.Series(y_black.flatten()))

_ = plt.plot(x_cdf_white, y_cdf_white*100, linestyle='--', lw = 2,label="white")
_ = plt.plot(x_cdf_black, y_cdf_black*100, linestyle='-', lw = 2,label="black")
_ = plt.xlabel('Income less than [$]', size = 14)
_ = plt.ylabel('Proportion [%]', size = 14)

plt.xlim([y_class_white_median[1]/10, 5*y_class_white_median[1]])
#plt.xscale('log')


plt.legend(loc="lower right")
plt.grid(True)


#plt.title('Type: USA, exclusionary-elitist')
#plt.title('Type: USA, homogeneous')
plt.show()
