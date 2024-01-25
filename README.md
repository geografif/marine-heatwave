Resources:
https://pjbartlein.github.io/REarthSysSci/netCDF.html


https://stackoverflow.com/questions/69676517/how-to-correctly-get-a-ncdf-file-into-a-data-frame
library(terra)
f <- "allecoors5augweer.nc"
r <- rast(f)
d <- as.data.frame(r)
