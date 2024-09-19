# Processed by Climate Data Operator (https://code.mpimet.mpg.de/projects/cdo)

cd /mnt/hgfs/cmip6
for s in "ssp126" "ssp245" "ssp558";do
for yr in {2020..2100};do
ifile="cmip6_raw/${s}/<filename>"
ofile="cmip6_remap/${s}/${yr}/<filename>"
cdo -yearmean -sellonlatbox,-180,180,-90,90 -remapbil,r360×180 -selyear,${yr} ${ifile} ${ofile}
echo $ofile
done
done
