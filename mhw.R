# cek versi R
version

# hapus data di environment
rm(list = ls())

# cek working directory saat ini
getwd()

# ganti working directory ke folder berisi file
setwd("D:/cpo")
getwd()

# install package
install.packages("lubridate")
install.packages("ggplot2")
install.packages("terra")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ncdf4")
install.packages("heatwaveR")
install.packages("heatwave3")

# panggil package dari library
library(terra)
library(ncdf4)
library(dplyr)
library(heatwaveR)
library(heatwave3)


#==============================================================================
# Membuka NetCDF sebagai Spatial Raster (Terra) dan plot
#==============================================================================
sst <- rast("ostia_sst_1982-2023.nc") #buka netcdf sebagai SpatRast
print(sst) #cek properti file
plot(sst) #plot
rm(sst) #hapus dari environment


#==============================================================================
# Membuka NetCDF dan konversi ke dataframe (tabel)
#==============================================================================
sst <- nc_open("ostia_sst_1982-2023.nc")
print(sst) #cek properti file

sst_lat <- ncvar_get(sst, "lat") #ekstrak variabel lat/latitude
sst_lon <- ncvar_get(sst, "lon") #ekstrak variabel lon/longitude
sst_time <- ncvar_get(sst, "time") #ekstrak variabel waktu

sst_t_units <- ncatt_get(sst, "time", "units")

sst_array <- ncvar_get(sst, "analysed_sst")
fillvalue <- ncatt_get(sst, "analysed_sst", "_FillValue")
sst_array[sst_array==fillvalue$value] <- NA

time_obs <- as.POSIXct(sst_time, "1981-01-01", tz="GMT") #ubah variabel waktu ke tanggal
range(time_obs) #cek periode data

lonlattime <- as.matrix(expand.grid(sst_lon, sst_lat, time_obs)) #gabungkan variabel

sst_vec_long <- as.vector(sst_array)
print(sst_vec_long)
length(sst_vec_long)

sst_obs <- data.frame(cbind(lonlattime, sst_array)) #gabungkan variabel temperature
head(sst_obs) #cek 'kepala' data

colnames(sst_obs) <- c("Long", "Lat", "Date", "Temp_K") #ubah nama kolom
head(sst_obs)

sst_final <- na.omit(sst_obs)
head(sst_final)

sst_final$Date <- as.Date(sst_final$Date)
sst_final$Temp_K <- as.double(sst_final$Temp_K)
head(sst_final)

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


# tanpa jendela (window)
rm(list=setdiff(ls(), "sst_final"))


#==============================================================================
# Menghitung climatology dan threshold berdasarkan data historis (tanpa smoothing)
#==============================================================================
# menghitung climatology berdasarkan rerata suhu per hari dalam periode data

sst_final_c <- sst_final %>%
  group_by(Long, Lat, DoY) %>%
  mutate( climatology = mean(`Temp_C`, na.rm=TRUE)) #%>%
  #mutate(p99 = quantile(`Temp_C`, 0.99, na.rm=TRUE)) %>%
  #summarize(climatology = mean(`Temp_C`, na.rm=TRUE))
  #summarize(p99 = quantile(sst_final$Temp_C, 0.99, na.rm=TRUE))

# menghitung threshold mhw berdasarkan percentil suhu per hari dalam periode data
# tanpa jendela (window)

sst_final_p <- sst_final %>%
  group_by(Long, Lat, DoY) %>%
  #mutate( climatology = mean(`Temp_C`, na.rm=TRUE)) %>%
  #mutate(p99 = quantile(`Temp_C`, 0.99, na.rm=TRUE)) #%>%
  #summarize(climatology = mean(`Temp_C`, na.rm=TRUE))
  summarize(p99 = quantile(`Temp_C`, 0.99, na.rm=TRUE))

head(sst_final_c)
head(sst_final_p)

#sst_final_cp <- merge(x = sst_final_c, y = sst_final_p, by = c("Long", "Lat", "DoY", "Date", "Temp_C"), all = TRUE)
sst_final_cp <- merge(x = sst_final_c, y = sst_final_p, by = c("Long", "Lat", "DoY"))

head(sst_final_cp)

# apabila perlu diurutkan berdasarkan cell grid (koordinat lat lon)
#sst_final_cp <- sst_final_cp[order(sst_final_cp$Long,sst_final_cp$Lat,sst_final_cp$DoY),]
#head(sst_final_cp)

# menyimpan dataframe sebagai csv
write.csv(sst_final_cp, "sst_final_cp.csv", row.names=T)

# hapus file
rm(sst_final_c, sst_final_p)


# filter grid 114.975, -8.025
sst_grid1 <- sst_final_cp %>%
  filter(Long == 114.975, Lat == -8.025) %>%
  filter(between(Date, as.Date('2022-01-01'), as.Date('2023-12-31'))) #ganti tanggal

head(sst_grid1)

# plot
ggplot(sst_grid1, aes(x = Date)) +
  geom_line(aes(y = Temp_C, color = "Temperature")) +
  geom_line(aes(y = p99, color = "Threshold")) +
  geom_line(aes(y = climatology, color = "Climatology")) +
  labs(title = "Temperature and Climatology over Time",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Temperature" = "blue", "Climatology" = "black", "Threshold" = "red")) +
  theme_minimal()


#==============================================================================
# Deteksi marine heatwave dengan data series (tabel 1 cell grid) dan package heatwaveR
#==============================================================================
sst_grid2 <- sst_final %>%
  filter(Long == 114.975, Lat == -8.025) #%>%
  #filter(between(Date, as.Date('1982-01-01'), as.Date('2023-12-31'))) #ganti tanggal

head(sst_grid2)

sst_grid2 <- sst_grid2[c("Date", "Temp_C")]
head(sst_grid2)

colnames(sst_grid2) <- c("t", "temp") #ubah nama kolom
head(sst_grid2)

# membuat climatology & threshold 99%
#ts <- ts2clm(sst_grid2, x = t, y = temp, climatologyPeriod = c("1982-01-01", "2023-12-31"),
#             windowHalfWidth=5, pctile=99, smoothPercentile=TRUE, smoothPercentileWidth=31)

# dengan threshold default 90%
ts <- ts2clm(sst_grid2, climatologyPeriod = c("1982-01-01", "2023-12-31"))
print(ts)

# deteksi mhw
res <- detect_event(ts)
#res <- detect_event(ts, categories=TRUE)
print(res)

# cek kategori mhw
cat_mhw <- category(res)
head(cat_mhw)

# plot
#event_line(res, spread = 100, metric = "intensity_cumulative",
#           start_date = "2010-12-01", end_date = "2011-06-30")

#event_line(res, spread = 300, start_date = "2022-01-01", #ganti angka spread untuk jumlah hari
#           end_date = "2023-12-31", category = TRUE)

event_line(res, spread=600, start_date = "2022-01-01", end_date = "2023-12-31", category = FALSE)

#==============================================================================
# Loop climatology dan percentile90 ke setiap grid
#==============================================================================
library(tidyr)
head(sst_final)
#nestdf <- sst_final %>% nest(Long:Lat)

aa <- split(sst_final, with(sst_final, interaction(Long,Lat)), drop = TRUE)


list_of_ts <- lapply(aa, function(df) {
  ts2clm(df, x = Date, y = Temp_C, climatologyPeriod = c("1982-01-01", "2023-12-31"))
})

#ts <- ts2clm(sst_final, x=Date, y=Temp_C, climatologyPeriod = c("1982-01-01", "2023-12-31"))

list_of_ts$`114.975.-8.025`

#ab <- as.data.frame(list_of_ts)
#xy <- Map(c, aa, list_of_ts)
#xy$`114.975.-8.025`

#==============================================================================
# Deteksi marine heatwave dengan data grid (raster) dan package heatwave3
#==============================================================================
library(heatwave3)
mhw_cube <- detect3(file_in = "D:/cpo/Portal/2013_1_1-2023_12_31.nc", return_type = "df", clim_period = c("1982-01-01", "2023-12-31"))

head(mhw_cube)
