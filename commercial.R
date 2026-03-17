
rm(list = ls())

# load libraries 
library(sp)
library(sf)
library(maps)
library(yarrr)

# load pelagic longline data 
# data request from June 23, 2023 - sent to M. Damiano by S. Alhale 
# file is "damiano_request623.xlsx" and folder is "Matt Damiano PLL request" on Google Drive

d <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/PLL_1986_2022_catch_effort.csv")

head(d)
dim(d)

table(d$LATDEG, useNA = "always")
table(d$LONDEG, useNA = "always")

table(d$SET_MMYY, useNA = "always")  # set month and year
table(d$TDOL, useNA = "always")  # target dolphin?    - ~8% targeted dolphin trips
table(d$PLL, useNA = "always")   # PLL gear used Y/N  - ~91% PLL trips

# format latitude and longitude 
d$lat <- d$LATDEG + d$LATMIN/60
d$lon <- -(d$LONDEG + d$LONMIN/60)

# check location of points
#map("world")
#points(d$lon, d$lat, col = 2, pch = 19, cex = 0.4)

# load area polygons and subset PLL data points by area
ar <- read.csv("data/eez/dolphin_OM_areas.csv")

d$area <- NA
table(ar$region)
lis <- unique(ar$region)

# for each area, find the PLL points that fall in the polygon and label the area
for (i in lis) {
  pol <- ar[which(ar$region == i), 1:2]
  pts <- point.in.polygon(d$lon, d$lat, pol.x = pol$X, pol.y = pol$Y)
  d$area[which(pts == 1)] <- i
}

d$area2 <- d$area
d$area2[which(d$area == "NCA" & d$lon < (-62))] <- "NCA_W"

# check that subsetting was done correctly - color-coded areas
table(d$area, useNA = "always")
map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(d$lon, d$lat, pch = 19, cex = 0.4, col = as.numeric(as.factor(d$area))+1)

# look at dolphin catch within PLL data
names(d)[grep("DOL", names(d))]

table(d$TDOL, useNA = "always")  # dolphin targeted trips
table(d$DOLK, useNA = "always")  # number of dolphin kept
table(d$DOLA, useNA = "always")  # number of dolphin discarded alive
table(d$DOLD, useNA = "always")  # number of dolphin discarded dead
hist(d$DOLK, main = "# dolphin kept")
hist(d$DOLA, main = "# dolphin discarded alive")
hist(d$DOLD, main = "# dolphin discarded dead")
hist(d$DOLPHIN_POUNDS, main = "pounds of dolphin kept")

# replace NAs with zeros
d$DOLA[is.na(d$DOLA)] <- 0
d$DOLK[is.na(d$DOLK)] <- 0
d$DOLD[is.na(d$DOLD)] <- 0
d$DOLPHIN_POUNDS[is.na(d$DOLPHIN_POUNDS)] <- 0

d$DOLTOT <- d$DOLK + d$DOLA + d$DOLD
hist(d$DOLTOT, main = "# dolphin caught")   # total dolphin kept or discarded

# calculate CPUE
d$cpue <- d$DOLPHIN_POUNDS / d$HOOKS

d$DISCTOT <- (d$DOLA + d$DOLD) / d$DOLTOT   # calculate discard rate
d$DISCDEAD <- d$DOLD / d$DOLTOT             # calculate dead discard rate

hist(d$DISCTOT, main = "rate of discarding (proportion)")
mean(d$DISCTOT, na.rm = T) * 100  # total discarding rate is 2.9%
hist(d$DISCDEAD, main = "rate of dead discarding (proportion)")
mean(d$DISCDEAD, na.rm = T) * 100 # dead discarding rate is 1.4%

d$area <- factor(d$area, levels = c("NCA", "CAR", "FLK", "NCFL", "NNC", "VBM", "NED"))

# plot out pounds caught of dolphin by area
map("world", xlim = c(-100, -30), ylim = c(5, 55)) 
    mtext(side = 3, line = 1, "Distribution of pounds of dolphin caught", cex = 1.2, font = 2)
axis(1); axis(2); box()
points(d$lon, d$lat, pch = 19, cex = log(d$DOLPHIN_POUNDS)/4, 
       col = transparent(as.numeric(as.factor(d$area))+1, 0.9))

dd <- d[which(d$TDOL == "Y"), ]
map("world", xlim = c(-100, -30), ylim = c(5, 55))
mtext(side = 3, line = 1, "Distribution of pounds of dolphin caught - dolphin target trips", cex = 1.2, font = 2)
axis(1); axis(2); box()
points(dd$lon, dd$lat, pch = 19, cex = log(dd$DOLPHIN_POUNDS)/4, 
       col = transparent(as.numeric(as.factor(dd$area))+1, 0.9))

barplot(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T)/10^6, 
        main = "Total dolphin catch by area 1986-2022 from PLL logbook", ylab = "millions of pounds")
barplot(tapply(dd$DOLPHIN_POUNDS, dd$area, mean, na.rm = T), main = "average catch per trip by area")

dp <- d[which(d$PLL == "Y" & d$TDOL == "Y"), ]  # subset PLL trips
map("world", xlim = c(-90, -40), ylim = c(15, 48))
mtext(side = 3, line = 1, "Distribution of PLL dolphin CPUE - dolphin target trips", cex = 1.2, font = 2)
#points(d$lon, d$lat, pch = 19, cex = 0.5)
axis(1); axis(2, las = 2); box()
points(dp$lon, dp$lat, pch = 19, cex = (log(dp$cpue)+6)/3, 
       col = transparent(as.numeric(as.factor(dp$area)), 0.9))
x <- c(0.1, 0.5, 1, 5)
legend("topleft", legend = x, pt.cex = (log(x)+6)/3, pch = 19, col = transparent(1, 0.7), title = "total pounds per # hooks")

dp <- d[which(d$PLL == "Y" & d$TDOL == "N"), ] # subset PLL trips not targeting dolphin
map("world", xlim = c(-90, -40), ylim = c(15, 48))
mtext(side = 3, line = 1, "Distribution of PLL dolphin CPUE - other species target trips", cex = 1.2, font = 2)
axis(1); axis(2, las = 2); box()
points(dp$lon, dp$lat, pch = 19, cex = (log(dp$cpue)+6)/3, 
       col = transparent(as.numeric(as.factor(dp$area)), 0.9))
x <- c(0.1, 0.5, 1, 5)
legend("topleft", legend = x, pt.cex = (log(x)+6)/3, pch = 19, col = transparent(1, 0.7), title = "total pounds per # hooks")

dp <- d[which(d$PLL == "Y" & d$TDOL == "Y"), ]  # subset PLL trips
plot(dp$cpue, dp$DOLPHIN_POUNDS)
plot(dp$cpue, dp$DOLPHIN_POUNDS, xlim = c(0, 10))

dp$cpue[which(dp$cpue == "Inf")] <- NA
boxplot((dp$cpue) ~ dp$area2, ylim = c(0, 5))
barplot(tapply(dp$cpue, dp$area2, mean, na.rm = T), main = "Average nominal PLL CPUE by region (dolphin target trips)", 
        ylab = "dolphin catch per unit effort (pounds / hooks)")
barplot(tapply(as.numeric(dp$cpue>0), dp$area2, mean, na.rm = T))

tab <- tapply(dp$cpue, list(dp$quarter, dp$area), mean, na.rm = T)
tab[which(is.na(tab))] <- 0

# look at distribution of dolphin PLL trips

map("world", xlim = c(-100, -30), ylim = c(5, 55))
mtext(side = 3, line = 1, "Distribution of logbook trips with dolphin present", cex = 1.2, font = 2)
points(d$lon[d$DOLPHIN_POUNDS == 0], d$lat[d$DOLPHIN_POUNDS == 0], pch = 19, cex = 1, 
       col = transparent(2, 0.9))
points(d$lon[d$DOLPHIN_POUNDS > 0], d$lat[d$DOLPHIN_POUNDS > 0], pch = 19, cex = 1, col = transparent(3, 0.6))
axis(1); axis(2, las = 2); box()
legend("topleft", c("no dolphin", "dolphin present"), pch = 19, col = c(2, 3))

for (i in unique(ar$region)) { 
  ar1 <- ar[which(ar$region == i), ]
  polygon(ar1$X, ar1$Y, border = 1, lwd = 2, lty = 1)
}
text(-80, 12, "CAR", cex = 1.2)
text(-50, 30, "NCA", cex = 1.2)
text(-50, 52, "NED", cex = 1.2)

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

barplot(tapply(d$DOLPHIN_POUNDS, d$yrqrt, sum, na.rm = T))

dev.off()
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
barplot(tabp, beside = T, col = rainbow(4, end = 0.8), main = "Seasonality of dolphin catch by region", 
        xlab = "region", ylab = "proportion of total catch (in pounds)", las = 1,
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = "topleft", col = rainbow(4, end = 0.8), bty = "n"))

# plot seasonality 
png(filename = "plots/PLL_seasonality.png", width = 600, height = 300)
barplot(tabp, beside = T, col = rainbow(4, end = 0.8), main = "Seasonality of dolphin catch by region -- logbook data", 
        xlab = "region", ylab = "proportion of total catch (in pounds)", las = 1, ylim = c(0, 0.8), 
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = 12, y = 0.8, col = rainbow(4, end = 0.8), bty = "n"))
dev.off()

# look to see if there is a difference when plotted by number
tab <- tapply(d$DOLK, list(d$quarter, d$area), sum, na.rm = T)
#barplot(tab, beside = T, col = rainbow(4))

tabp1 <- apply(tab, 2, function(x) x / sum(x, na.rm = T))
barplot(tabp1, beside = T, col = rainbow(4), main = "seasonality of dolphin catch by region", 
        xlab = "region", ylab = "proportion of total catch (in pounds)",
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = "topleft", col = rainbow(4), bty = "n"))

tabp/tabp1  # numbers are generally very similar - go with weight

tab <- tapply(d$DOLPHIN_POUNDS, list(d$quarter, d$area, d$year), sum, na.rm = T)

# look at variability in seasonality by year 
dev.off()
yrs <- 1998:2022
par(mfrow = c(5, 5), mex = 0.6)
for (i in 1:length(yrs)) {
  tabtemp <- tab[, , i]
  tabp2 <- apply(tabtemp, 2, function(x) x / sum(x, na.rm = T))
  barplot(tabp2, beside = T, col = rainbow(4), main = yrs[i], las = 2)
  }

# outputs for further analysis ----------------------

# Define seasonality (% of dolphin caught in each quarter), by year, for each of the regions.  
# For the three high seas regions we will use these percentages to parse the total catch (estimated by SAU) by year into quarters.
# For the four U.S. EEZ regions we will not use these numbers as the trip ticket data are reported by month; 
# however we will compare the reported percentages to make sure those numbers seem reasonable.  

# summarize total catch in pounds by quarter, year, and area
tab <- tapply(d$DOLPHIN_POUNDS, list(d$quarter, d$year, d$area), sum, na.rm = T)

# convert to percentages across quarters of the year, for each year/area combination
percatch <- tab
for (i in 1:7) {
  temp <- tab[, , i]
  percatch[, , i] <- apply(temp, 2, function(x) x / sum(x, na.rm = T))
}

percatch[is.na(percatch)] <- 0
percatch
colSums(percatch)  # there are a few zeros, we will interpolate these with the nearest-neighbor (previous year)

percatch[, 6, 3] <- percatch[, 5, 3]
percatch[, 22, 7] <- percatch[, 21, 7]
colSums(percatch) 

percatch
# save this object as we will use it later -- contains percentages by year-quarter by area for filling in international data

save(percatch, file = "data/outputs/per_PLLcatch_by_area_yearquarter.RData")

# Look at how PLL catch is distributed by area.  We can use these numbers to fill in missing 
# areas of reporting (primarily NCA) from the trip ticket data.  

dev.off()
# plot total catch by area
barplot(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T), col = 2:8, 
        main = "total dolphin catch by area", xlab = "total pounds landed")

tab <- tapply(d$DOLPHIN_POUNDS, list(d$area, d$yrqrt), sum, na.rm = T)
barplot(tab, beside = T, col = 2:8, las = 2)

tabp <- apply(tab, 2, function(x) x / sum(x, na.rm = T))

matplot(as.numeric(colnames(tab)), t(tab), col = 2:8, main = "distribution of catch by area and quarter", 
        xlab = "year", ylab = "total catch (in pounds)", 
        type = "l", pch = 19, lty = 1, lwd = 2)
legend("top", rownames(tabp), col = 2:8, bty = "n", pch = 19, horiz = T, lty = 1, lwd = 2)

matplot(as.numeric(colnames(tabp)), t(tabp), col = 2:8, main = "Proportional distribution of catch by area and quarter", 
        xlab = "year", ylab = "proportion of total catch (in pounds)", 
        type = "l", pch = 19, lty = 1, lwd = 2)
legend("top", rownames(tabp), col = 2:8, bty = "n", pch = 19, horiz = T, lty = 1, lwd = 2)

# In most years, the majority of the catch comes from the NCFL region
# As much as quarter of the catch coming from VBM in early years; later tendency to come from NNC

round(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T) / sum(tapply(d$DOLPHIN_POUNDS, d$area, sum, na.rm = T)) * 100, 2)

# Reporting areas are here: https://grunt.sefsc.noaa.gov/ttrs/lu_areas_nmfs.jsp
# On average, 79% of catch comes from NCFL.  Only 1.8% comes from NCA; not reported but probably negligible. 

# NCA   CAR   FLK  NCFL   NNC   VBM   NED 
# 1.80  1.38  0.92 78.96  7.01  9.34  0.60



###################  TRIP TICKET DATA  ######################

# clear workspace 
rm(list = ls())

# Trip ticket data provided to M. Karnauskas by K. Detloff (file name: SA_COM_DOLPHIN_1986_2023.xlsx)
# Notes attached to data: 
# Data are commercial dolphinfish landings for the Atlantic coast from 1986-2023
# Include all landings by year even if the gear type or area is unknown 
# Area 0 indicates an unknown that could be anywhere along the Atlantic coast and 
# areas 799 and 800 indicate an unknown somewhere in the South Atlantic. 
# Also included the state of landing to maybe help refine things in the case of unknown fishing areas. 
# Pounds are in units of whole weight.
# The map of trip ticket areas is here (the 3 digit series): https://grunt.sefsc.noaa.gov/ttrs/lu_areas_nmfs.jsp

# read trip ticket data
dorig <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/SA_COM_DOLPHIN_1986_2023.csv")

d <- dorig
head(d)
dim(d)

apply(d, 2, table, useNA = "always")

# read in file with NMFS area codes
f <- read.csv("data/fin_nmfs_codes.csv")
f$latc <- f$lat + 0.5
f$lonc <- -(f$lon + 0.5)
f$area2 <- f$area

# read in operating model shapefiles
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

# look at areas defined by region and make sure correctly specified
map("world", xlim = c(-90, -50), ylim = c(20, 55))
axis(1); axis(2); box()
points(f$lonc, f$latc, pch = 19, cex = 1, col = as.numeric(as.factor(f$area2)))
for (i in unique(ar$region)) { 
  polygon(ar$X[which(ar$region == i)], ar$Y[which(ar$region == i)], border = 8)}

# look at how landings are distributed by reported area and state landed
d$region <- f$area2[match(d$AREA, f$FIN)]
table(d$STATE, d$region, d$GEAR)

d$gear2 <- d$GEAR
d$gear2[which(d$GEAR == "UNK")] <- "HL"

table(d$STATE, d$region, d$gear2)

d$region[d$region == ""] <- "unknown"
unique(d$region)
d$region <- factor(d$region, levels = c("unknown", "NCA", "CAR", "FLK", "NCFL", "NNC", "VBM", "NED"))

barplot(tapply(d$WW, d$YEAR, sum, na.rm = T), las = 2)
barplot(tapply(d$WW, d$MONTH, sum, na.rm = T))
barplot(tapply(d$WW, d$GEAR, sum, na.rm = T))
barplot(tapply(d$WW, d$STATE, sum, na.rm = T))
barplot(tapply(d$WW, d$AREA, sum, na.rm = T), las = 2)
barplot(tapply(d$WW, d$region, sum, na.rm = T)/10^6, main = "Total dolphin catch by area 1986 - 2023 from trip tickets", 
        ylab = "millions of pounds")
tab <- tapply(d$WW, list(d$GEAR, d$region), sum, na.rm = T)
barplot(tab/10^6, beside = T, 
        legend.text = rownames(tab), col = 2:4, 
        main = "Total trip ticket landings by gear and reported area 1986-2023", 
        ylab = "millions of pounds")

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

matplot(rownames(tab3), tab3/10^6, type = "l", col = 2:8, lty = 1, lwd = 2, 
        main = "U.S. commercial landings by region", 
        xlab = "year", ylab = "total landings (millions of pounds)")
legend("topright", colnames(tab3), col = 2:8, lty = 1, bty = "n", lwd = 2)

round(colSums(tab3, na.rm = T) / sum(tab3, na.rm = T) * 100, 2)

# NCA   CAR   FLK  NCFL   NNC   VBM   NED 
# 0.10  0.15 36.18 34.63 25.45  3.37  0.12 

# reformat for input into MSE operating model 

labs <- c()
for (i in colnames(tab3)) { labs <- c(labs, rep(i, nrow(tab3)))}

yrmon <- as.numeric(rownames(tab3))
yrs <- floor(yrmon)
qrt <- (yrmon - floor(yrmon)) * 4 + 1
flt <- rep("UScom", length(qrt))
findat <- data.frame(yrs, qrt, flt, labs, matrix(tab3))
findat
names(findat) <- c("Year", "Quarter", "Fleet", "Area", "Catch_lbs")

head(findat)

plot(findat$Catch_lbs, type = "l")
plot(findat$Catch_lbs ~ factor(findat$Area))
plot(findat$Catch_lbs ~ factor(findat$Quarter))
plot(findat$Catch_lbs ~ factor(findat$Year))

findat <- findat[which(findat$Year <= 2022), ]

apply(findat, 2, table, useNA = "always")

dorig <- dorig[which(dorig$YEAR <= 2022), ]
check1 <- tapply(dorig$WW, dorig$YEAR, sum, na.rm = T)
check2 <- tapply(findat$Catch_lbs, findat$Year, sum, na.rm = T)
plot(check1, check2)
round(check1 / check2, 2)  # note these should be slightly different because December catch moved to following year

write.csv(findat, file = "data/FINAL_files/commercial_TomFormat.csv", row.names = FALSE)

findat$Area <- factor(findat$Area, levels = c("NCA", "CAR", "FLK", "NCFL", "NNC", "VBM", "NED"))

tab <- tapply(findat$Catch_lbs, list(findat$Quarter, findat$Area), sum, na.rm = T)
barplot(tab, beside = T, col = rainbow(4))

tabp <- apply(tab, 2, function(x) x / sum(x, na.rm = T))
barplot(tabp, beside = T, col = rainbow(4, end = 0.8), main = "Seasonality of dolphin catch by region -- trip ticket data", 
        xlab = "region", ylab = "proportion of total catch (in pounds)",
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = 10, y = 0.7, col = rainbow(4, end = 0.8), bty = "n"))

dev.off()
png(filename = "plots/tripticket_seasonality.png", width = 600, height = 300)
barplot(tabp, beside = T, col = rainbow(4, end = 0.8), main = "Seasonality of dolphin catch by region -- trip ticket data", 
        xlab = "region", ylab = "proportion of total catch (in pounds)", ylim = c(0, 0.8), 
        legend = c("DJF", "MAM", "JJA", "SON"), args.legend = list(x = 12, y = 0.8, col = rainbow(4, end = 0.8), bty = "n"))
dev.off()


