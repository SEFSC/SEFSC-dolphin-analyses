
rm(list = ls())

library(sp)
library(sf)
library(maps)
library(yarrr)

d <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/PLL_1986_2022_catch_effort.csv")

head(d)
dim(d)

table(d$LATDEG, useNA = "always")
table(d$LONDEG, useNA = "always")

table(d$SET_MMYY, useNA = "always")  # set month and year
table(d$TDOL, useNA = "always")  # target dolphin?    - ~8% targeted dolphin trips
table(d$PLL, useNA = "always")   # PLL gear used Y/N  - ~91% PLL trips

d$lat <- d$LATDEG + d$LATMIN/60
d$lon <- -(d$LONDEG + d$LONMIN/60)

map("world")
points(d$lon, d$lat, col = 2, pch = 19, cex = 0.4)

d$area <- NA

ar <- read.csv("data/eez/dolphin_OM_areas.csv")

table(ar$region)
lis <- unique(ar$region)

for (i in lis) {
  pol <- ar[which(ar$region == i), 1:2]
  pts <- point.in.polygon(d$lon, d$lat, pol.x = pol$X, pol.y = pol$Y)
  d$area[which(pts == 1)] <- i
}

table(d$area, useNA = "always")
map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(d$lon, d$lat, pch = 19, cex = 0.4, col = as.numeric(as.factor(d$area))+1)

names(d)[grep("DOL", names(d))]

table(d$TDOL, useNA = "always")
table(d$DOLK, useNA = "always")
table(d$DOLA, useNA = "always")
table(d$DOLD, useNA = "always")
hist(d$DOLK)
hist(d$DOLA)
hist(d$DOLD)
hist(d$DOLPHIN_POUNDS)

d$DOLA[is.na(d$DOLA)] <- 0
d$DOLK[is.na(d$DOLK)] <- 0
d$DOLD[is.na(d$DOLD)] <- 0
d$DOLPHIN_POUNDS[is.na(d$DOLPHIN_POUNDS)] <- 0

d$DOLTOT <- d$DOLK + d$DOLA + d$DOLD
hist(d$DOLTOT)

d$DISCTOT <- (d$DOLA + d$DOLD) / d$DOLTOT
d$DISCDEAD <- d$DOLD / d$DOLTOT 

hist(d$DISCTOT)
mean(d$DISCTOT, na.rm = T) * 100  # total discarding rate is 2.9%
hist(d$DISCDEAD)
mean(d$DISCDEAD, na.rm = T) * 100 # dead discarding rate is 1.4%

map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(d$lon, d$lat, pch = 19, cex = log(d$DOLPHIN_POUNDS)/4, 
       col = transparent(as.numeric(as.factor(d$area))+1, 0.9))

dd <- d[which(d$TDOL == "Y"), ]
map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(dd$lon, dd$lat, pch = 19, cex = log(dd$DOLPHIN_POUNDS)/2, col = as.numeric(as.factor(dd$area))+1)

# make new date variables 
d$year <- as.numeric(substr(d$SDATE, 1, 4))
d$mon  <- as.numeric(substr(d$SDATE, 5, 6))
d$monold  <- d$mon
table(d$year, useNA = "always")
table(d$mon, useNA = "always")
table(d$year == d$SET_YEAR)

# convert month December to following year 
d$mon[which(d$mon == 12)] <- 0.5
d$year[which(d$mon == 0.5)] <- d$SET_YEAR[which(d$mon == 0.5)] + 1
table(d$year, useNA = "always")
table(d$mon, useNA = "always")

# specify quarters 
d$quarter <- cut(d$mon, breaks = c(0.3, 2.5, 5.5, 8.5, 11.5))
table(d$quarter, useNA = "always")
d$quarter <- as.numeric(d$quarter)
table(d$quarter, d$mon)
d$yrqrt <- d$year + (d$quarter - 1)/4

barplot(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T))
barplot(tapply(d$DOLPHIN_POUNDS, d$SET_YEAR, sum, na.rm = T), las = 2) # starts in 1997

d <- d[which(d$year >= 1997), ]
d <- d[which(d$year < 2023), ]

d$area <- factor(d$area, levels = c("NCA", "CAR", "FLK", "NCFL", "NNC", "VBM", "NED"))

barplot(tapply(d$DOLPHIN_POUNDS, d$yrqrt, sum, na.rm = T))


# plot seasonality of dolphin catch by month
tab <- tapply(d$DOLPHIN_POUNDS, list(d$monold, d$area), sum, na.rm = T)
barplot(tab, beside = T, col = rainbow(12, end = 0.9))

tab <- tab[-1, ]
tabp <- apply(tab, 2, function(x) x / sum(x, na.rm = T))

barplot(tabp, beside = T, col = rainbow(12, end = 0.8), main = "seasonality of dolphin catch by region", 
        xlab = "region", ylab = "proportion of total catch (in pounds landed)",
        legend = month.abb, args.legend = list(x = "topleft", col = rainbow(12, end = 0.8), bty = "n"))

# plot seasonality of dolphin catch by quarter
tab <- tapply(d$DOLPHIN_POUNDS, list(d$quarter, d$area), sum, na.rm = T)
barplot(tab, beside = T, col = rainbow(4))

tabp <- apply(tab, 2, function(x) x / sum(x, na.rm = T))
barplot(tabp, beside = T, col = rainbow(4), main = "seasonality of dolphin catch by region", 
        xlab = "region", ylab = "proportion of total catch (in pounds)",
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = "topleft", col = rainbow(4), bty = "n"))

# look to see if there is a difference when plotted by number
tab <- tapply(d$DOLK, list(d$quarter, d$area), sum, na.rm = T)
#barplot(tab, beside = T, col = rainbow(4))

tabp1 <- apply(tab, 2, function(x) x / sum(x, na.rm = T))
barplot(tabp1, beside = T, col = rainbow(4), main = "seasonality of dolphin catch by region", 
        xlab = "region", ylab = "proportion of total catch (in pounds)",
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = "topleft", col = rainbow(4), bty = "n"))

tabp/tabp1  # numbers are generally very similar - go with weight


tab <- tapply(d$DOLPHIN_POUNDS, list(d$quarter, d$area, d$year), sum, na.rm = T)

yrs <- 1998:2022
par(mfrow = c(5, 5), mex = 0.6)
for (i in 1:length(yrs)) {
  tabtemp <- tab[, , i]
  tabp2 <- apply(tabtemp, 2, function(x) x / sum(x, na.rm = T))
  barplot(tabp2, beside = T, col = rainbow(4), main = yrs[i], las = 2)
  }


# outputs for further analysis ----------------------

# Define seasonality (% of dolphin caught in each quarter), by year, for all regions.  
# For the three high seas regions we will use these percentages to parse the total catch (estimated by SAU) by year into quarters.
# For the four U.S. EEZ regions we will not use these numbers as the trip ticket data are reported by month; 
# however we will compare the reported percentages to make sure those numbers seem reasonable.  

tab <- tapply(d$DOLPHIN_POUNDS, list(d$quarter, d$year, d$area), sum, na.rm = T)

percatch <- tab
for (i in 1:7) {
  temp <- tab[, , i]
  percatch[, , i] <- apply(temp, 2, function(x) x / sum(x, na.rm = T))
}

percatch[is.na(percatch)] <- 0
percatch

# Look at how PLL catch is distributed by area.  We will use these numbers to fill in missing 
# areas of reporting (primarily NCA) from the trip ticket data.  

barplot(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T), col = 2:8, 
        main = "total dolphin catch by area", xlab = "total pounds landed")

tab <- tapply(d$DOLPHIN_POUNDS, list(d$area, d$year), sum, na.rm = T)

barplot(tab, beside = T, col = 2:8, las = 2)

tabp <- apply(tab, 2, function(x) x / sum(x, na.rm = T))

barplot(tabp, beside = T, col = 2:8, main = "distribution of catch by area and year", 
        xlab = "year", ylab = "proportion of total catch (in pounds)",
        legend = rownames(tabp), args.legend = list(x = "topleft", col = 2:8, horiz = T, bty = "n"))

matplot(1997:2022, t(tabp), col = 2:8, main = "distribution of catch by area and year", 
        xlab = "year", ylab = "proportion of total catch (in pounds)", 
        type = "b", pch = 19, lty = 1)
legend("right", rownames(tabp), col = 2:8, bty = "n", pch = 19, lty = 1)

# In most years, the majority of the catch comes from the NCFL region
# As much as quarter of the catch coming from VBM in early years; later tendency to come from NNC

round(rowMeans(tabp, na.rm = T) * 100, 2)
round(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T) / sum(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T)) * 100, 2)

# Reporting areas are here: https://grunt.sefsc.noaa.gov/ttrs/lu_areas_nmfs.jsp
# On average, 79% of catch comes from NCFL.  Only 1.8% comes from NCA; not reported but probably negligible. 


################### TRIP TICKET DATA  #################

rm(list = ls())

d <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/SA_COM_DOLPHIN_1986_2023.csv")

head(d)
dim(d)

apply(d, 2, table)

# from Kyle: SQL code:  SELECT * FROM sedat.xref_area_nmfs_fin@secapxdv_dblk.sfsc.noaa.gov

f <- read.csv("C://Users/mandy.karnauskas/Desktop/fin_nmfs_codes.csv")
f$latc <- f$lat + 0.5
f$lonc <- -(f$lon + 0.5)
f$area2 <- f$area

ar <- read.csv("data/eez/dolphin_OM_areas.csv")

table(ar$region)
lis <- unique(ar$region)

for (i in lis) {
  pol <- ar[which(ar$region == i), 1:2]
  pts <- point.in.polygon(f$lonc, f$latc, pol.x = pol$X, pol.y = pol$Y)
  f$area2[which(pts == 1)] <- i
}

map("world", xlim = c(-90, -50), ylim = c(20, 55))
axis(1); axis(2); box()
points(f$lonc, f$latc, pch = 19, cex = 1, col = as.numeric(as.factor(f$area2)))

f$area2[which(f$latc > 36 & f$area2 == "")] <- "VBM"
abline(h = 41)
abline(v = -72)

f[which(f$latc < 35 & f$latc > 28), ]
abline(v = c(-77.5, -81.5, -80.5))
abline(h = c(35, 28))
f$area2[which(f$latc < 35 & f$latc > 28)] <- "NCFL"

f$area2[which(f$latc < 28)] <- "FLK"

map("world", xlim = c(-90, -50), ylim = c(20, 55))
axis(1); axis(2); box()
points(f$lonc, f$latc, pch = 19, cex = 1, col = as.numeric(as.factor(f$area2)))

for (i in unique(ar$region)) { 
  polygon(ar$X[which(ar$region == i)], ar$Y[which(ar$region == i)], border = 8)}

d$region <- f$area2[match(d$AREA, f$FIN)]
table(d$STATE, d$region, d$GEAR)

d$gear2 <- d$GEAR
d$gear2[which(d$GEAR == "UNK")] <- "HL"

table(d$STATE, d$region, d$gear2)

unique(d$region)
d$region <- factor(d$region, levels = c("", "NCA", "CAR", "FLK", "NCFL", "NNC", "VBM", "NED"))

barplot(tapply(d$WW, d$YEAR, sum, na.rm = T), las = 2)
barplot(tapply(d$WW, d$MONTH, sum, na.rm = T))
barplot(tapply(d$WW, d$GEAR, sum, na.rm = T))
barplot(tapply(d$WW, d$STATE, sum, na.rm = T))
barplot(tapply(d$WW, d$AREA, sum, na.rm = T), las = 2)
barplot(tapply(d$WW, d$region, sum, na.rm = T))
barplot(tapply(d$WW, list(d$GEAR, d$region), sum, na.rm = T), beside = T, 
        legend.text = unique(d$GEAR), col = 2:4)

# convert month December to following year 
d$MONTH[which(d$MONTH == 12)] <- 0.5
d$YEAR2 <- d$YEAR
d$YEAR2[which(d$MONTH == 0.5)] <- d$YEAR[which(d$MONTH == 0.5)] + 1
table(d$YEAR2, useNA = "always")
table(d$MONTH, useNA = "always")

# specify quarters 
d$quarter <- cut(d$MONTH, breaks = c(0.3, 2.5, 5.5, 8.5, 11.5))
table(d$quarter, useNA = "always")
d$quarter <- as.numeric(d$quarter)
table(d$quarter, d$MONTH)
d$yrqrt <- d$YEAR2 + (d$quarter - 1)/4

tab <- tapply(d$WW, list(d$yrqrt, d$region, d$gear2), sum, na.rm = T)
tab[is.na(tab)] <- 0
tab2 <- tab

for (i in 1:2) { 
  for (j in 1:dim(tab)[1]) {
    unk <- tab[j, 1, i]
    if (unk > 0) {
    knw <- tab[j, 2:8, i]
    if (sum(knw) > 0)  { 
    rec <- knw/sum(knw, na.rm = T) * unk
    tab2[j, 2:8, i] <- tab[j, 2:8, i] + rec
    tab2[j, 1, i] <- 0
    }  else  { 
    knw <- tab[(j-1), 2:8, i]
    rec <- knw/sum(knw, na.rm = T) * unk
    tab2[j, 2:8, i] <- tab[j, 2:8, i] + rec
    tab2[j, 1, i] <- 0  
        }
      }
    }
  }

head(tab)
head(tab2)

table(rowSums(tab) == rowSums(tab2))
plot(rowSums(tab), rowSums(tab2))
hist(rowSums(tab) - rowSums(tab2))

tab3 <- tab2[, , 1] + tab2[, , 2]

head(tab2)
head(tab3)

tab3 <- tab3[, 2:8]

barplot(colSums(tab3))

matplot(rownames(tab3), tab3, type = "l", col = 2:8, lty = 1)
legend("topright", colnames(tab3), col = 2:8, lty = 1)
