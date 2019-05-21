import netCDF4 as nc
import numpy as np

import datetime
import pandas as pd

plotdefaults='''
linewidth = 1.0
markersize = 2.0
'''
def read_cable_single_site_patch(fname, verb=False):
    f = nc.Dataset(fname)
    time = nc.num2date(f.variables['time'][:],
                            f.variables['time'].units)
    nc_dims = [dim for dim in f.dimensions]  # list of nc dimensions

    var_list = f.variables
    df = pd.DataFrame()
    if 'patchfrac' in var_list:
        vars_short = ['iveg','patchfrac']
        for var in vars_short:
            if 'time' not in f.variables[var].dimensions:
                for i in range(np.array(f.variables[var].shape)[0]):
                    df[var + '_' + "%1i" %(i+1)] = \
                    np.repeat(f.variables[var][i,0],f.variables['time'].size)
            elif (f.variables[var].dimensions[0] == 'time' and 
                    f.variables[var].dimensions[1] == 'patch' and 
                    f.variables[var].dimensions[2] == 'land') :
                for i in range(np.array(f.variables[var].shape)[1]):
                    df[var + '_' + "%1i" %(i+1)] = f.variables[var][:,i,0]

    for var in var_list:
        if var in nc_dims:
            if verb:
                print ('\tName:', var)
                print ("\t\tdimensions:", f.variables[var].dimensions)    
                print ("\t\tsize:", f.variables[var].size)
        if var not in nc_dims and var != 'patchfrac':
            if verb:
                print ('\tName:', var)
                print ("\t\tdimensions:", f.variables[var].dimensions)
                print ("\t\tsize:", f.variables[var].size)
            if (f.variables[var].dimensions[0] == 'time' and 
                f.variables[var].dimensions[1] == 'patch' and 
                f.variables[var].dimensions[2] == 'land') :
                    df[var] = np.repeat(0.0,f.variables['time'].size)
                    for i in range(np.array(f.variables[var].shape)[1]):
                        df[var + '_' + "%1i" %(i+1)] = f.variables[var][:,i,0]
                        if var=='TVeg' or var =='ESoil' or var =='Evap':
                            df[var + '_' + "%1i" %(i+1)] = df[var + '_' + "%1i" %(i+1)].values*3600*24 # mmd-1
                        if var=='Tair' or var=='VegT':
                            df[var + '_' + "%1i" %(i+1)] = df[var + '_' + "%1i" %(i+1)].values - 273.15
                        if var=='NEE':
                             df['NEP' + '_' + "%1i" %(i+1)] = -1.0 * df[var + '_' + "%1i" %(i+1)].values
                        df[var] = df[var] + df['patchfrac' + '_' + "%1i" %(i+1)] * \
                                  df[var + '_' + "%1i" %(i+1)] 
            if (f.variables[var].dimensions[0] == 'time' and 
                f.variables[var].dimensions[1] == 'soil' and 
                f.variables[var].dimensions[2] == 'patch' and 
                f.variables[var].dimensions[3] == 'land') :
                df['SoilTempWeighted'] =  np.repeat(0.0,f.variables['time'].size)
                df['SoilTemp1'] =  np.repeat(0.0,f.variables['time'].size)
                for j in range(np.array(f.variables[var].shape)[2]): # patches
                    for i in range(np.array(f.variables[var].shape)[1]): # soil layers        
                        if var =='SoilTemp':
                             df['SoilTemp_%1i' %(i+1) + '_%1i_' %(j+1)] = f.variables[var][:,i,j,0]-273.15
                        if var =='SoilMoist':
                             df['SoilMoisr_%1i' %(i+1) + '_%1i_' %(j+1)] = f.variables[var][:,i,j,0]
                    if var =='SoilTemp':
                            df['SoilTempWeighted_%1i' %(j+1)]  =  np.sum(f.variables['froot'][:,j,0] * \
                                                                  f.variables[var][:,:,j,0], axis = 1)  -273.15
                    if var =='SoilMoist':
                                df['SoilMoistWeighted_%1i' %(j+1)]  =  np.sum(f.variables['froot'][:,j,0] * \
                                                          f.variables[var][:,:,j,0], axis = 1)
                    if var =='SoilTemp':
                        df['SoilTempWeighted'] = df['SoilTempWeighted'] +  df['patchfrac' + '_' + "%1i" %(j+1)] * \
                                                 df['SoilTempWeighted_%1i' %(j+1)]
                        df['SoilTemp1'] = df['SoilTemp1'] +  df['patchfrac' + '_' + "%1i" %(j+1)] * \
                                                 df['SoilTemp_1' + '_%1i_' %(j+1)]
                                
                                
                
              

    df['dates'] = time
    df['NEP'] = -1*df['NEE'].values
    df['vpd'] = (esat(df['Tair']) - df['Qair']*   0.018016/ 0.02897 * df['PSurf'] )*0.1  # esat in hPa; PSurf in hPa; vpd in kPa

    df = df.set_index('dates')
    df['season']=(df.index.month%12 + 3)//3

    df['day'] = np.floor(df.index.to_julian_date().values-0.5)
    grouped = df.groupby('day')
    df = grouped.apply(compute_avg_val)

    return df

#####################################################################################

def read_cable_single_site(fname, verb=False):

    f = nc.Dataset(fname)
    time = nc.num2date(f.variables['time'][:],
                        f.variables['time'].units)
    
    var_list = [var for var in f.variables] # get a list of all netcdf variables
    df = pd.DataFrame()
    
    for var in var_list:
        if var not in nc_dims:
            if verb:
                print ('\tName:', var)
                print ("\t\tdimensions:", f.variables[var].dimensions)
                print ("\t\tsize:", f.variables[var].size)
        try:
            df[var] = f.variables[var][:,0,0]
            if verb:
                print(var)
        except:
            continue
            if verb:
                print(var, '   not included - not a time series variable')
            
    df['TVeg'] = df['TVeg'].values*3600*24 # mmd-1
    df['ESoil'] = df['ESoil'].values*3600*24 # mmd-1
    df['Evap'] = df['Evap'].values*3600*24 # mmd-1
    df['dates'] = time 
    df['NEP'] = -1*df['NEE'].values
    df['Tair'] = df['Tair'].values - 273.15
    df['VegT'] = df['VegT'].values - 273.15
    df['VegT - Tair'] = df['VegT'].values -  df['Tair'].values
    df['vpd'] = (esat(df['Tair']) - df['Qair']*   0.018016/ 0.02897 * df['PSurf'] )*0.1  # esat in hPa; PSurf in hPa; vpd in kPa
    
    
    
    for i in range(6):
        df['SoilTemp%1i' %(i+1)] = f.variables['SoilTemp'][:,i,0]-273.15
        df['SoilMoist%1i' %(i+1)] = f.variables['SoilMoist'][:,i,0]

    df['SoilTempWeighted'] = np.sum(f.variables['froot'][:,0,0]*f.variables['SoilTemp'][:,:,0,0]  ,axis = 1) -273.15
    df['SoilMoistWeighted'] = np.sum(f.variables['froot'][:,0,0]*f.variables['SoilMoist'][:,:,0,0]  ,axis = 1)
        
    df = df.set_index('dates')
    df['season']=(df.index.month%12 + 3)//3

    df['day'] = np.floor(df.index.to_julian_date().values-0.5)
    grouped = df.groupby('day')
    df = grouped.apply(compute_avg_val)

    return df
########################################################################################################################
def read_cable_input(fname, verb=False):
    f=nc.Dataset(fname)
    var_list = f.variables
    nc_dims = [dim for dim in f.dimensions]  # list of nc dimensions
    time = nc.num2date(f.variables['time'][:],
                        f.variables['time'].units)
    df_met = pd.DataFrame()
    df_met['time'] = time
    
    for var in var_list:
        if var not in nc_dims:
            if verb:
                print ('\tName:', var)
                print ("\t\tdimensions:", f.variables[var].dimensions)
                print ("\t\tsize:", f.variables[var].size)
            if 'time' in f.variables[var].dimensions:
                if len(f.variables[var].dimensions)==3:
                    df_met[var] = f.variables[var][:,0,0]
                elif len(f.variables[var].dimensions)==4:
                    df_met[var] = f.variables[var][:,0,0,0]
    df_met = df_met.set_index('time')
    df_met['Tair'] = df_met['Tair'].values - 273.15
    return df_met

########################################################################################################################
def read_ozflux(site, tstep):
    url='http://dap.ozflux.org.au/thredds/dodsC/ozflux/sites/' + site + '/L6/default/' + site + '_L6.nc'
    f = nc.Dataset(url,'r')
    f.set_auto_mask(False)
    time = nc.num2date(f.variables['time'][:],
                        f.variables['time'].units)
    df = pd.DataFrame(f.variables['GPP_SOLO'][:,0,0], columns=['GPP'])
    df['dates'] = time
    df['NEP'] = f.variables['NEP_SOLO'][:,0,0]
    df['Evap'] = f.variables['ET'][:,0,0]*(24*3600/tstep) # mm -> mm/d
    df['Rnet'] = f.variables['Fn'][:,0,0]
    df = df.set_index('dates')
    df['season']=(df.index.month%12 + 3)//3
    return df



########################################################################################################################
def compute_avg_val(df):
    df['TairDailyMean'] = df['Tair'].mean()
    df['SoilTempWeightedDailyMean'] = df['SoilTempWeighted'].mean()
    df['SoilTemp1DailyMean'] = df['SoilTemp1'].mean()
    #df['SoilTemp2DailyMean'] = df['SoilTemp2'].mean()
    #df['SoilTemp3DailyMean'] = df['SoilTemp3'].mean()
    #df['SoilTemp4DailyMean'] = df['SoilTemp4'].mean()
    #df['SoilTemp5DailyMean'] = df['SoilTemp5'].mean()
    #df['SoilTemp6DailyMean'] = df['SoilTemp6'].mean()
    return df

def compute_mean_Trf(df):
    df['Daily_Mean_Trf_Tsoil'] = df['Trf_Tsoil_h'].mean()
    df['Daily_Mean_Trf_Ta'] = df['Trf_Ta_h'].mean()
    return df

def Q10(T, Tref=10, q10=2):
    trf  = q10**((T-Tref)/10.)
    return trf

def LT(T, Tref=10):
    trf = np.exp(308.56*(1.0/56.02-1.0/(np.maximum(T,-20.0)+46.02)))
    trf_ref = np.exp(308.56*(1.0/56.02-1.0/(np.maximum(Tref,-20.0)+46.02)))
    trf = trf/trf_ref
    return trf

def Arrhenius(T, Tref=10, Ea=60):
    trf = np.exp(-Ea/(8.314e-3 * (T+273.15)))
    trf_ref = np.exp(-Ea/(8.314e-3 * (Tref+273.15)))
    trf = trf/trf_ref
    return trf

def varQ10(T,Tref):
    trf = (3.09 - 0.043*((T)+25.)/2.0)**((T -25.0)/10.0)
    trf_ref = (3.09 - 0.043*((Tref)+25.)/2.0)**((Tref -25.0)/10.0)
    trf = trf/trf_ref
    return trf

def vcmx(T,Tgrowth,Tref):

    #  leuning 2002 (p c & e) equation for temperature response
    #  used for vcmax for c3 plants
    # adaptation for T acclimation by Knorr and Kattge PCE 2007
    # T, Tgrowth   ! instantaneous T and mean temperature for the last month (K)

    EHaVc  = 71513.0  # J/mol (K&K Table 3)
    EHdVc  = 200000.0 # J/mol (K&K p.1177)
    aKK = 668.39 # kJ/mol (K&K Table3)
    bKK = -1.07 # kJ/mol/K (K&K Table3)
    TREFK = Tref+273.15
    rgas = 8.314
    entropvc = (aKK + bKK * Tgrowth) 
    xVccoef  = 1.0 + np.exp((entropvc * TREFK - EHdVc)/  ( rgas*TREFK ) )
    xvcnum = xVccoef*np.exp( ( EHaVc / ( rgas*TREFK ) )* ( 1.-TREFK/T ) )
    xvcden=1.0+np.exp( ( entropvc*T-EHdVc ) / ( rgas*T ) )
    trf = np.maximum( 0.0,xvcnum / xvcden )

    return trf

def Genn1(Tair):
    Vb = -0.20
    Vip = 15
    trf = 1./(1.+np.exp(Vb*(Tair-Vip)))
    return trf

def esat(Tair):
    # saturated vapur pressure in hPa
    esata     = 6.106 # constants for saturated vapour pressure calculation
    esatb     = 17.27
    esatc     = 237.3          
    esat =  esata * np.exp(esatb*Tair/(Tair+esatc))
    return esat

def liq_diffusivity(Tair, Tref):
    diff_liq_Trf = np.exp(-577.0/((Tair + 273.15)-145.))
    diff_liq_Trf_ref = np.exp(-577.0/((Tref + 273.15)-145.))
    diff_liq_Trf = diff_liq_Trf/diff_liq_Trf_ref
    return diff_liq_Trf

def vap_diffusivity(Tair, Tref):
    diff_vap = ((Tair + 273.15)/273.15) ** 1.88
    diff_vap_ref = ((Tref + 273.15)/273.15) ** 1.88
    diff_vap = diff_vap / diff_vap_ref
    return diff_vap

