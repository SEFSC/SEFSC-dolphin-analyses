
library(ncdf4)
library(maps)
library(lubridate)
library(rerddap)


rm(list = ls())

# get ERDDAP info  --------------------------------
id <- info('nceiErsstv5')   #https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.html

# download data
sst_grab <- griddap(id, fields = 'sst', 
                    time = c('1980-01-15', '2025-12-15'), 
                    longitude = c(260, 358), 
                    latitude = c(0, 70))

nc <- nc_open(sst_grab$summary$filename, write=FALSE, readunlim=TRUE, verbose=FALSE)
v1 <- nc$var[[1]]
sst <- ncvar_get(nc, v1)
lon <- v1$dim[[1]]$vals - 360
lat <- v1$dim[[2]]$vals
nc_close(nc)

tim <- as.POSIXct(v1$dim[[4]]$vals, origin = "1970-01-01")
mon <- as.numeric(substr(tim, 6, 7))
yr <- substr(tim, 1, 4)
dim(sst)

tim[1:5]
table(mon)
table(yr)

par(mfrow = c(1, 2))
image(lon, lat, sst[,,1])

sst[which(lon <= (-92)), which(lat <= 17.5), ] <- NA
sst[which(lon <= (-85)), which(lat <= 15), ] <- NA
sst[which(lon <= (-83)), which(lat <= 10), ] <- NA
sst[which(lon <= (-77)), which(lat <= 8.5), ] <- NA
sst[which(lon <= (-78) & lon >= (-80)), which(lat <= 9), ] <- NA

image(lon, lat, sst[,,100])


rec <- read.csv("data/recLandings.csv")
names(rec)[1] <- "Year"

rec$ATL <- rowSums(rec[3:6], na.rm = T)
rec$ALL <- rec$GOM + rec$ATL

rec <- rec[which(rec$Year >= 1990), ]
sortyr <- rec$Year[order(rec$ALL)]


momeans <- c()

for (i in 1:12)  { 
  j <- which(mon == i)
  momeans <- cbind(momeans, meansst[j])
}

mossts <- data.frame(cbind(unique(yr), momeans), stringsAsFactors = F)
names(mossts) <- c("Year", month.abb)
head(mossts)
mossts[] <- lapply(mossts, as.numeric)

rec <- read.csv("data/recLandings.csv")
names(rec)[1] <- "Year"

rec$ATL <- rowSums(rec[3:6], na.rm = T)
rec$ALL <- rec$GOM + rec$ATL


d <- merge(rec, mossts, by = "Year")
head(d)

d <- d[which(d$Year >= 1990), ]

lis <- names(mossts)[2:13]
par(mfrow = c(3, 4), mex = 0.8)
for(i in 1:12) {
  ind <- d[, which(names(d) == lis[i])]
  plot(ind, d$ALL, xlab = paste(lis[i], "SST"), ylab = "total recreational landings")
  out <- lm(d$ALL ~ ind)
  abline(out)
  p <- summary(out)$coef[2, 4]
  co <- 1
  if (p < 0.01)  { co <- 4 }
  if (p < 0.001) { co <- 6 }
  legend("topleft", paste0("R^2 = ", round(summary(out)$adj.r.squared, 2), "; p = ", 
                           round(p, 3)), cex = 1.2, bty = "n", text.col = co)
}


lis <- names(mossts)[2:13]
par(mfrow = c(3, 4), mex = 0.8)
for(i in 1:12) {
  ind <- d[, which(names(d) == lis[i])]
  plot(ind, d$N/d$ALL, xlab = paste(lis[i], "SST"), ylab = "total recreational landings")
  out <- lm(d$N/d$ALL ~ ind)
  abline(out)
  p <- summary(out)$coef[2, 4]
  co <- 1
  if (p < 0.01)  { co <- 4 }
  if (p < 0.001) { co <- 6 }
  legend("topleft", paste0("R^2 = ", round(summary(out)$adj.r.squared, 2), "; p = ", 
                           round(p, 3)), cex = 1.2, bty = "n", text.col = co)
}

