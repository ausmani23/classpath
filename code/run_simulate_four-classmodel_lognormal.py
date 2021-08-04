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


# Data:
#----------------------
N_classes     = 4
p_black       = 0.15 # proportion of black population
p_white       = 1-p_black
p_class_black = np.array([18.95, 66.30, 12.63, 2.11]).T/100 #race-conditioned class probabilities
p_class_white = np.array([12.13, 59.46, 23.75, 4.65]).T/100

p_class_black = p_class_black / np.sum(p_class_black )
p_class_white = p_class_white / np.sum(p_class_white )

p_class = np.empty((N_classes,1)) #marginalized class probabilities
for k in range(N_classes):
    p_class[k]      = p_class_black[k]*p_black + p_class_white[k]*p_white


y_class_black_mean = np.array([13144, 41843, 76287, 62900])
y_class_white_mean = np.array([25293, 63433, 112858, 119976])

y_class_black_median = np.array([3500, 33000, 60000, 35950])
y_class_white_median = np.array([8300, 49000, 80000, 66000])

ratio_median2mean_class_black = np.divide(y_class_black_median,y_class_black_mean)
ratio_median2mean_class_white = np.divide(y_class_white_median,y_class_white_mean)


# Modify capitalist means [COMMENT OUT IF NEEDED]
#----------------------
# ratio_wageshare = 0.55

# y_noncapitalist_mean = 0 #compute per capita income for non-capitalists
# for k in range(0,N_classes-1,1):
#     y_noncapitalist_mean = y_noncapitalist_mean + (y_class_black_mean[k]*p_class_black[k]*p_black  +  y_class_white_mean[k]*p_class_white[k]*p_white)

# ratio_mean2median_capitalist = (1-ratio_wageshare)/(p_class[3]*ratio_wageshare) * y_noncapitalist_mean / (y_class_black_median[3]*p_black + y_class_white_median[3]*p_white)

# y_class_black_mean[3] = ratio_mean2median_capitalist * y_class_black_median[3] #modified means
# y_class_white_mean[3] = ratio_mean2median_capitalist * y_class_white_median[3] 


#"Marginalized" class parameters
#----------------------
ratio_median2mean_class = np.empty((N_classes,1)) #pseudo-marginalized ratios
y_class_mean            = np.empty((N_classes,1))
y_mean                  = 0
for k in range(N_classes):
    y_class_mean[k]            = y_class_black_mean[k]*p_black + y_class_white_mean[k]*p_white
    y_mean                     = y_mean + y_class_mean[k]*p_class[k]
    ratio_median2mean_class[k] = ratio_median2mean_class_black[k]*p_black + ratio_median2mean_class_white[k]*p_white


#print('Derived wage share: {:}'.format(1-  y_class_mean[3]*p_class[3]/y_mean)) #sanity check


# Modelling counterfactual interventions on arrows
#----------------------

# Eliminiate race->class
#p_class_black = p_class
#p_class_white = p_class

# Reduce race->outcome
#y_class_black_mean   = y_class_mean
#y_class_white_mean   = y_class_mean

#y_class_black_median = numpy.multiply(y_class_mean, ratio_median2mean_class)
#y_class_white_median = numpy.multiply(y_class_mean, ratio_median2mean_class)


# Reduce class->outcome (parameterized)
#alpha                = 0 #class convergence: fraction between 0 and 1
#y_class_black_mean   = alpha*y_black + (1-alpha)*y_class_black
#y_class_white_mean   = alpha*y_white + (1-alpha)*y_class_white

#y_class_black_median = numpy.multiply(y_class_black_mean, alpha*max(ratio_median2mean_class) + (1-alpha)*ratio_median2mean_class_black)
#y_class_black_median = numpy.multiply(y_class_white_mean, alpha*max(ratio_median2mean_class) + (1-alpha)*ratio_median2mean_class_white)


# Fixed parameters
#----------------------
n       = int(1e5) # number of draws
n_black = int( round(p_black * n) )
n_white = n - n_black


mu_class_black = np.log( y_class_black_median )
mu_class_white = np.log( y_class_white_median )

s_class_black = np.sqrt(  2 * np.divide(y_class_black_mean,y_class_black_median) ) #CHECK
s_class_white = np.sqrt(  2 * np.divide(y_class_white_mean,y_class_white_median) )



# Simulate
#----------------------
# Sample black population
y_black = np.empty((n_black,1))
c_black = np.random.choice(4, n_black, replace=True, p=p_class_black)
for k in range(n_black):
    y_black[k] = np.random.lognormal(mu_class_black[c_black[k]], s_class_black[c_black[k]])  

# Sample white population
y_white = np.empty((n_white,1))
c_white = np.random.choice(4, n_white, replace=True, p=p_class_white)
for k in range(n_white):
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

plt.xlim([y_class_white_median[1]/10, 10*y_class_white_median[1]])
#plt.xscale('log')


plt.legend(loc="lower right")
plt.grid(True)


#plt.title('Type: USA, exclusionary-elitist')
#plt.title('Type: USA, homogeneous')
plt.show()
