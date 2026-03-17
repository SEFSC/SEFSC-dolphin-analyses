
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


#cols <- rainbow(100, start = 0.1, end = 0.65)[100:1]
cols <- c(0, rainbow(3, start = 0.4, end = 0.6), 2)

nf <- layout(matrix(c(1:44, rep(89, 11), 45:88), 11, 9, byrow = FALSE), c(rep(3, 4), 0.8, rep(3, 4)), c(4, rep(2, 12)))
layout.show(nf)

yrs <- c(sortyr[1:4], sortyr[(length(sortyr)-3): length(sortyr)])
yrs
status <- c(rep("low", 4), rep("high", 4))
status[1] <- "lowest"
status[8] <- "highest"

for (j in 1: length(yrs))  {
  k <- which(yr == yrs[j])
  par(mex = 0.5, mar = c(2, 4, 2, 1)+1)
  plot(rec$Year, rec$ALL/10^6, type = "l", col = 4, ylim = c(0, 32), lwd = 2, ylab = "rec catch")
  text(1990, 3, status[j], pos = 4)
  abline(v = yrs[j], col = 8, lwd = 3)
  
  for (i in k[2:11])  { 
    par(mex = 0.3, mar = c(2, 2, 2, 0))
    map("world", xlim = c(-100, -15), ylim = c(0, 45), col = 0)
    axis(1); axis(2, las = 2); box()
    image(lon, lat, sst[, , i], add = T, col = cols, breaks = c(-2, 26, 27, 28.5, 30, 32))
    text(-33, 42, paste(mon[i], yr[i]), cex = 1)
    map("world", add = T, col = gray(0.5)); box()
    contour(lon, lat, sst[, , i], levels = c(30), add = T, col = c(2), lwd = 2, drawlabels = F)
    contour(lon, lat, sst[, , i], levels = c(28.5), add = T, col = 1, lwd = 2, drawlabels = FALSE)
    
  }
}

dev.off()


# look at May patterns --------------------

par(mfrow = c(6, 6), mex = 0.5)

for (j in 1: length(sortyr))  {
  k <- which(yr == sortyr[j])
  for (i in k[6])  { 
    map("world", xlim = c(-100, -10), ylim = c(0, 50), col = 0)
    axis(1); axis(2, las = 2); box()
    image(lon, lat, sst[, , i], add = T, col = cols, breaks = c(-2, 26, 27, 28.5, 30, 32))
    text(-33, 45, paste(mon[i], yr[i]), cex = 1)
    map("usa", add = T, col = gray(0.5)); box()
    contour(lon, lat, sst[, , i], levels = c(30), add = T, col = c(2), lwd = 1, drawlabels = F)
    contour(lon, lat, sst[, , i], levels = c(27.0), add = T, col = 1, lwd = 1, drawlabels = FALSE)
    rect(-100, 0, -30, 32, col = NA, border = 2, lwd = 2)
    rect(-99, 18, -78, 31, col = NA, border = 6, lwd = 2)
  }
}

# calculate percentage preferred habitat area in reach of U.S. 

lons <- which(lon >= (-100) & lon <= (-30))
lats <- which(lat >= (0) & lat <= (32))
lon[lons]
lat[lats]

lon[lons][1:12]  # should be -100 to -78
lat[lats][10:17] # should be 18 to 32

kar <- sst[lons, lats, which(mon == 5)]
ind <- data.frame(matrix(ncol = 2, nrow = dim(kar)[3], 
                         dimnames = list(NULL, c("Year", "perar"))))
ind$Year <- yr[which(mon == 5)]

for (i in 1:dim(kar)[3])  {
  k1 <- kar[, , i]
  k2 <- (k1 >28.5 & k1 < 30.0)
  ind$perar[i] <- length(which(k2[1:12, 10:17] == TRUE))/ length(which(k2 == TRUE))
}

ind$perar[which(is.na(ind$perar))] <- 0
ind

d <- merge(rec, ind, by = "Year")
d

dev.off()

par(mar = c(5, 5, 1, 1))
plot(d$perar, d$ATL, col = 0, 
     xlab = "proportion of preferred dolphin temperatures near\nU.S. jurisdictions (out of entire Caribbean basin) in May", 
     ylab = "total U.S. recreational catch (Gulf and Atlantic)")
text(d$perar, d$ATL, d$Year)
out <- lm(d$ATL ~ d$perar)
summary(out)
abline(out, lty = 2, col = 2)
abline(v = mean(d$perar), col = 8)
abline(h = mean(d$ATL), col = 8)


write.csv(ind, file = "indices/blue_blob_index.csv", row.names = F)



