# Processing occurrence probability
# AquaMaps: https://www.aquamaps.org/

import csv
import os
from pandas import DataFrame
from glob import glob


wd=''
to_dir=''
# list all phyla
for name in os.listdir("%s/AquaMaps" % wd): 
    os.chdir("%s/%s" % (wd, name))
    file = sorted(glob('*.csv'))
    for i in file:
        depth = []  
        newRows = []  
        maxd = 0
        mind = 0
        
        # read csv
        csvFileObj = open("%s/AquaMaps/%s/%s" % (wd, name, i), encoding='utf-8-sig', errors='ignore')
        readerObj = csv.reader(csvFileObj)  
        
        # judge distribution depth
        for row in readerObj:
            # skip metadata
            if readerObj.line_num in range(1, 15):  
                continue
            
            depth.append(row)  
            test = DataFrame(depth)
            mind = test.iat[0, 2]  # minimum depth   
            maxd = test.iat[0, 5]  # maximum depth     
            
            if readerObj.line_num == 15:
                break
        
        # extract dataframe
        for row in readerObj:
            if readerObj.line_num in range(1, 33):  # skip 
                continue
            if not row:  # pause at Null line
                break
            newRows.append(row)

        # judge by probability
        df = DataFrame(newRows,
                       columns=['Genus', 'Species', 'Latitude', 'Longitude', 'C-Square', 'probability'])
        df['occurrence'] = '0'  
        df['probability'] = df['probability'].astype('float')  
        df_raw = df.drop(['Genus', 'Species', 'C-Square'], axis=1)
        df_raw.loc[(df.probability > 0), 'occurrence'] = 1
        
        # save by depths
        ## surface
        if int(mind) < 200:
            path_raw = "%s/surface/%s/%s" % (to_dir, name, i)
            df_raw.to_csv(path_raw, sep=',', index=False, header=True)
        ## meso
        if int(maxd) >= 200:
            path_raw = "%s/mesopelagic/%s/%s" % (to_dir, name, i)
            df_raw.to_csv(path_raw, sep=',', index=False, header=True)
        ## bathy
        if int(maxd) >= 1000:
            path_raw = "%s/bathypelagic/%s/%s" % (to_dir, name, i)
            df_raw.to_csv(path_raw, sep=',', index=False, header=True)
        ## abysso
        if int(maxd) >= 4000:
            path_raw = "%s/abyssopelagic/%s/%s" % (to_dir, name, i)
            df_raw.to_csv(path_raw, sep=',', index=False, header=True)
        
  
