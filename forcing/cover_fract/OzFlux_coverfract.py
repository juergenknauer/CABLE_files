# -*- coding: utf-8 -*-
"""
Created on Thu Feb 14 15:22:02 2019

@author: kna016
Script to create 'iveg' and 'patchfrac' information for OzFLUX site-level runs in CABLE
Feb. 2019: extended to read C4 fraction as well and append them to sitelist
"""

import pandas as pd    # dataframes
import numpy as np  # multi-dimensional arrays


## Options
use_site_years = False  # use site years (True) or long-term climatologies (False)
frac_max       = True   # derive forest fraction from max. forest fraction (rest is grass)?


basepath='C:/Users/kna016/Documents/CABLE/'


## 1) load dataframe containing vegfract timeseries and site list
fcover     = pd.read_csv(basepath + 'cover_fract/ozflux28.gimms3g_fcovfperfrec.csv',index_col='PointName')
C4fraction = pd.read_csv(basepath + 'cover_fract/ozflux28.c4_grass_frac_cov.csv',index_col='PointName')
sitelist   = pd.read_csv(basepath + 'OzFLUX_sitelist_v2.txt',sep='\t',index_col=0) 


## 2) extract fcover climatology information
#clims_index = [k for k, i in enumerate(fcover.index) if '9999' in i]
#fcover_climatology = fcover.iloc[clims_index,:]
fcov_index = [k for k, i in enumerate(fcover.index) if '9999' in i and 'fcov' in i]
fper_index = [k for k, i in enumerate(fcover.index) if '9999' in i and 'fper' in i]
frec_index = [k for k, i in enumerate(fcover.index) if '9999' in i and 'frec' in i]


## 3) extract vegetation cover for each site
frac_forest = []
frac_grass  = []
frac_C4     = []


for s,site in enumerate(sitelist.index):
  
  print('starting site ' + site)
  
  if use_site_years:
    
    print('not yet implemented!')

#    startyear = sitelist.loc[site,'startyear']
#    endyear = sitelist.loc[site,'endyear']
#  
#    #fcover[0,site]
#    dates=fcover.index.tolist()
#    index=[]
#    for year in range(startyear,endyear):
#      index.append([k for k, i in enumerate(dates) if 'fcov' in i and str(year) in i])
     
  else:  
    
#    fcov.append(round(fcover.iloc[fcov_index,s].mean(),3))
#    fper.append(round(fcover.iloc[fper_index,s].mean(),3))
#    frec.append(round(fcover.iloc[frec_index,s].mean(),3))

    
    if frac_max:
      
      frac_forest.append(fcover.iloc[fper_index,s].max())
      frac_grass.append(1.0 - frac_forest[-1])
      
      
    else:
      
      fcov_site = fcover.iloc[fcov_index,s]
      fper_site = fcover.iloc[fper_index,s]
      frec_site = fcover.iloc[frec_index,s]
      
      
      frac_forest.append((np.array(fper_site) / np.array(fcov_site)).mean())
      frac_grass.append((np.array(frec_site) / np.array(fcov_site)).mean())
    

    frac_C4.append(C4fraction.iloc[3,s])
    
    
    
    
  
## ensure that forest and grass fractions sum up to 1
if frac_max==False:
  diff = (np.array(frac_forest) + np.array(frac_grass)) - 1.0
  
  frac_forest = frac_forest - diff*frac_forest
  frac_grass  = frac_grass - diff*frac_grass
  
  diff = (np.array(frac_forest) + np.array(frac_grass)) - 1.0
  
  frac_forest = frac_forest - diff*frac_forest
  frac_grass  = frac_grass - diff*frac_grass


frac_forest = np.array(frac_forest).round(3)
frac_grass  = np.array(frac_grass).round(3)
frac_C4     = np.array(frac_C4).round(3)



### append to sitelist
sitelist['forest_fraction'] = frac_forest
sitelist['grass_fraction']  = frac_grass
sitelist['C4_fraction']     = frac_C4

# sitelist.to_csv('C:/Users/kna016/Documents/CABLE/OzFLUX_sitelist_v3.txt',sep="\t") # frac_max=False
#sitelist.to_csv('C:/Users/kna016/Documents/CABLE/OzFLUX_sitelist_v4.txt',sep="\t")  # frac_max=True
sitelist.to_csv('C:/Users/kna016/Documents/CABLE/OzFLUX_sitelist_v5.txt',sep="\t")  # frac_C4 included



### some tests:
#  x = filter(lambda funs: funs[0:3] == startyear, dates) 
#  x = filter(lambda k: str(startyear) in k, dates)
  
#  lst = ['a', 'ab', 'abc', 'bac']
#  filter(lambda k: 'ab' in k,lst)
#  
#  x = [k for k in lst if 'ab' in k]   # 'list comprehensions'
# list(map(str,range(startyear,endyear)))