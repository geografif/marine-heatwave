# cek versi R
version

# hapus data di environment
rm(list = ls())

# cek working directory saat ini
getwd()

# ganti working directory ke folder berisi file
setwd("D:/Project/MHW")
getwd()

# panggil package dari library
library(terra)
library(ncdf4)
library(dplyr)
library(heatwaveR)


#==============================================================================
# Membuka NetCDF dan konversi ke dataframe (tabel)
#==============================================================================
#sst <- nc_open("ostia_sst_1982-2023.nc")
sst <- nc_open("nc_gabung.nc")
print(sst) #cek properti file

sst_lat <- ncvar_get(sst, "latitude") #ekstrak variabel lat/latitude
sst_lon <- ncvar_get(sst, "longitude") #ekstrak variabel lon/longitude
sst_time <- ncvar_get(sst, "time") #ekstrak variabel waktu

sst_t_units <- ncatt_get(sst, "time", "units")

sst_array <- ncvar_get(sst, "analysed_sst")
fillvalue <- ncatt_get(sst, "analysed_sst", "_FillValue")
sst_array[sst_array==fillvalue$value] <- NA

time_obs <- as.POSIXct(sst_time, "1970-01-01", tz="GMT") #ubah variabel waktu ke tanggal
range(time_obs) #cek periode data

lonlattime <- as.matrix(expand.grid(sst_lon, sst_lat, time_obs)) #gabungkan variabel

sst_vec_long <- as.vector(sst_array)
#print(sst_vec_long)
length(sst_vec_long)

sst_obs <- data.frame(cbind(lonlattime, sst_array)) #gabungkan variabel temperature
head(sst_obs) #cek 'kepala' data

colnames(sst_obs) <- c("Long", "Lat", "Date", "Temp_K") #ubah nama kolom
head(sst_obs)

sst_final <- na.omit(sst_obs)
head(sst_final)

str(sst_final)

sst_final$Long <- as.double(sst_final$Long)
sst_final$Lat <- as.double(sst_final$Lat)
sst_final$Date <- as.Date(sst_final$Date)
sst_final$Temp_K <- as.double(sst_final$Temp_K)

str(sst_final)

# ubah kelvin ke celcius
sst_final = sst_final %>%
  mutate(Temp_C = Temp_K-273.15)

sst_final <- sst_final[-c(4)] #hapus kolom kelvin
head(sst_final)

sst_final_copy <- sst_final #cadangkan

# menghitung hari dalam tahun / Day of Year (DOY)
sst_final = sst_final %>%
  mutate(DoY = lubridate::yday(Date))
head(sst_final)


#==============================================================================
# Deteksi marine heatwave dengan data series (tabel 1 cell grid) dan package heatwaveR
#==============================================================================
sst_grid2 <- sst_final %>%
  filter(Long == 115.225, Lat == -8.775000) #%>%
#filter(between(Date, as.Date('1982-01-01'), as.Date('2023-12-31'))) #ganti tanggal

head(sst_grid2)

#sst_grid2 <- sst_grid2[c("Date", "Temp_C")]
#head(sst_grid2)

#colnames(sst_grid2) <- c("t", "temp") #ubah nama kolom

colnames(sst_grid2) <- c("long", "lat", "t", "temp", "doy") #ubah nama kolom
head(sst_grid2)

# membuat climatology & threshold 99%
#ts <- ts2clm(sst_grid2, x = t, y = temp, climatologyPeriod = c("1982-01-01", "2023-12-31"),
#             windowHalfWidth=5, pctile=99, smoothPercentile=TRUE, smoothPercentileWidth=31)

# dengan threshold default 90%
ts <- ts2clm(sst_grid2, climatologyPeriod = c("1982-01-01", "2023-12-31"))
#ts <- ts2clm(sst_final, x=Date, y=Temp_C, climatologyPeriod = c("1982-01-01", "2023-12-31"))

print(ts)
head(ts)

# deteksi mhw
res <- detect_event(ts)
res <- detect_event(ts, categories=TRUE)
print(res)

# cek kategori mhw
#cat_mhw <- category(res)
#head(cat_mhw)

# plot
#event_line(res, spread = 100, metric = "intensity_cumulative",
#           start_date = "2010-12-01", end_date = "2011-06-30")

#event_line(res, spread = 300, start_date = "2022-01-01", #ganti angka spread untuk jumlah hari
#           end_date = "2023-12-31", category = TRUE)

#event_line(res, spread=600, start_date = "2022-01-01", end_date = "2023-12-31", category = FALSE)

# Cara di atas hanya untuk satu cell dan tidak mengembalikan kolom longitude dan latitude


#==============================================================================
# 2024.02.23 Loop
#==============================================================================
event_fun <- function(df){
  clim <- ts2clm(data = df, x = Date, y = Temp_C, climatologyPeriod= c("1982-01-01", "2023-12-31"))
  event <- detect_event(data = clim, x=Date, y=Temp_C)
  return(event$event)
}

climthresh_fun <- function(df){
  clim <- ts2clm(data = df, x = Date, y = Temp_C, climatologyPeriod= c("1982-01-01", "2023-12-31"))
  return(clim)
}

#way1
table_event <- sst_final %>%
  group_by(Long, Lat) %>%
  group_modify(~event_fun(.x))

table_climthresh <- sst_final %>%
  group_by(Long, Lat) %>%
  group_modify(~event_only2(.x))

# menyimpan dataframe sebagai csv
write.csv(table_event, "table_event.csv", row.names=T)
write.csv(table_climthresh, "table_climthresh.csv", row.names=T)


#==============================================================================
# Ke python
#==============================================================================

#==============================================================================
# Membuat variabel turunan (amplitude)
#==============================================================================
sst <- nc_open("test.nc")
summary(sst)
#cek properti file

sst <- rast("temp.nc") #buka netcdf sebagai SpatRast
print(sst) #cek properti file
plot(sst) #plot
rm(sst) #hapus dari environment

sst2 <- rast("thresh.nc") #buka netcdf sebagai SpatRast
print(sst2) #cek properti file
plot(sst2) #plot
rm(sst) #hapus dari environment

amplitude <- mean(sst - sst2)
plot(amplitude)
