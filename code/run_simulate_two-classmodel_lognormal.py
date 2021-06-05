#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Class as mediating variable
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

# Variables for counterfactuals
#----------------------
p_black                           = 0.15 # proportion of black population
ratio_median_black2white_worker   = 0.70 # race->outcome: racial differential within non-propertied class
ratio_odds_capitalist_white2black = 4 # race->class: odds ratio of white vs. black in propertied class
ratio_median_capitalist2worker    = 2 # class->outcome: capitalist to white-worker median

# Fixed parameters
#----------------------
n                   = int(1e5) # number of draws
p_capitalist        = 0.30 # proportion of propertied class locations
median_white_worker = 1000 # median income for workers
mean2med_worker     = 1.1 # dispersion of nonproperty incomes (mean to median>0)
mean2med_capitalist = 1.4 # dispersion property incomes (mean to median>0)


# Derived parameters
#----------------------
# Log-normal (non-propertied)
mu_white_worker = np.log( median_white_worker )
mu_black_worker = np.log( median_white_worker*ratio_median_black2white_worker ) # scale parameter
s_worker = np.sqrt(  2 * np.log(mean2med_worker) );

# Log-normal (propertied)
mu_capitalist = np.log( median_white_worker * ratio_median_capitalist2worker ) # > 0
s_capitalist = np.sqrt(  2 * np.log(mean2med_capitalist) );

# Proportions of capitalists
p_property_black = p_capitalist / ( p_black + ratio_odds_capitalist_white2black*(1-p_black) )
p_property_white = p_property_black * ratio_odds_capitalist_white2black

# Population sizes
n_black = int( round(p_black * n) )
n_white= n - n_black


# Simulate
#----------------------

#  Sample white population
y_white = np.empty((n_white,1))
c_white = np.random.binomial(1, p_property_white, n_white)
for k in range(0,n_white-1):
    if c_white[k] == 0:
        y_white[k] = np.random.lognormal(mu_white_worker, s_worker) 
    else:
        y_white[k] = np.random.lognormal(mu_capitalist, s_capitalist) 
        
# Sample black population
y_black = np.empty((n_black,1))
c_black = np.random.binomial(1, p_property_black, n_black)
for k in range(0,n_black-1):
    if c_black[k] == 0:
        y_black[k] = np.random.lognormal(mu_black_worker, s_worker)  
    else:
        y_black[k] = np.random.lognormal(mu_capitalist, s_capitalist) 

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

plt.xlim([median_white_worker/10, 5*median_white_worker])
#plt.xscale('log')


plt.legend(loc="lower right")
plt.grid(True)


#plt.title('Type: USA, exclusionary-elitist')
#plt.title('Type: USA, homogeneous')
plt.show()
