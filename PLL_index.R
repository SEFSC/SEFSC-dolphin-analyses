
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

par(mfrow = c(6, 6), mex = 0.3)
for (i in 1990:2022) { 
  map('world', xlim = c(-90, -50), ylim = c(10, 30))
  axis(1); axis(2, las = 2); box()
  d1 <- d[which(d$SET_YEAR == i) ,]
  points(d1$lon, d1$lat, pch = 19, col = "#FF000055")
  mtext(side = 3, i)
  rect(xleft = -75, ybottom = 12, xright = -60, ytop = 23, col = NA, border = 4)
}

#  find the PLL points that fall in the polygon
d$car <- 0
d$car[which(d$lon > (-70) & d$lon < (-60) & d$lat > 12 & d$lat < 23)] <- 1

# check that subsetting was done correctly - color-coded areas
map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(d$lon, d$lat, pch = 19, cex = 0.4, col = as.numeric(as.factor(d$col))+1)

table(d$car, useNA = "always")
dc <- d[which(d$car == 1), ]
dim(dc)

# look at dolphin catch within PLL data
names(dc)[grep("DOL", names(dc))]

table(dc$TDOL, useNA = "always")  # dolphin targeted trips
table(dc$DOLK, useNA = "always")  # number of dolphin kept
table(dc$DOLA, useNA = "always")  # number of dolphin discarded alive
table(dc$DOLD, useNA = "always")  # number of dolphin discarded dead
hist(dc$DOLK, main = "# dolphin kept")
hist(dc$DOLA, main = "# dolphin discarded alive")
hist(dc$DOLD, main = "# dolphin discarded dead")
hist(dc$DOLPHIN_POUNDS, main = "pounds of dolphin kept")

# replace NAs with zeros
dc$DOLA[is.na(dc$DOLA)] <- 0
dc$DOLK[is.na(dc$DOLK)] <- 0
dc$DOLD[is.na(dc$DOLD)] <- 0
dc$DOLPHIN_POUNDS[is.na(dc$DOLPHIN_POUNDS)] <- 0

dc$DOLTOT <- dc$DOLK + dc$DOLA + dc$DOLD
hist(dc$DOLTOT, main = "# dolphin caught")   # total dolphin kept or discarded

dc <- dc[-which(dc$HOOKS == 0), ]

# calculate CPUE
#dc$cpue <- dc$DOLPHIN_POUNDS / dc$HOOKS
dc$cpue <- dc$DOLTOT / dc$HOOKS

# make new date variables 
dc$year <- as.numeric(substr(dc$SDATE, 1, 4))
dc$mon  <- as.numeric(substr(dc$SDATE, 5, 6))
dc$monold  <- dc$mon
table(dc$year, useNA = "always")
table(dc$mon, useNA = "always")
table(dc$year == d$SET_YEAR)


d <- dc[which(dc$mon <=4), ]


barplot(tapply(d$cpue, d$year, mean, na.rm = T), las = 2)
barplot(tapply(d$cpue, d$mon, mean, na.rm = T))
barplot(tapply(d$cpue, d$PLL, mean, na.rm = T))
barplot(tapply(d$cpue, d$TDOL, mean, na.rm = T))


rec <- read.csv("data/recLandings.csv")
names(rec)[1] <- "Year"
rec$ATL <- rowSums(rec[3:6], na.rm = T)

d <- merge(rec, dat, by = "Year")

plot(d$ind, d$ATL, col = 0)
text(d$ind, d$ATL, d$Year, col = 1)
out <- lm(d$ATL ~ d$ind)
abline(out, col = 8)
summary(out)


par(mfrow = c(6, 6), mex = 0.3)
for (i in 1990:2022) { 
  map('world', xlim = c(-90, -50), ylim = c(10, 30))
  axis(1); axis(2, las = 2); box()
  d1 <- dc[which(dc$SET_YEAR == i) ,]
  points(d1$lon, d1$lat, pch = 19, col = "#FF000055")
  mtext(side = 3, i)
  rect(xleft = -70, ybottom = 12, xright = -60, ytop = 23, col = NA, border = 4)
}

#out <- lm(log(dat$cpue) ~ dat$year + 0 + dat$mon + dat$zone)
#summary(out)
#anova(out)

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
