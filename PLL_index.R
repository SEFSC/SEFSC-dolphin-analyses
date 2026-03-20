
# clear workspace
rm(list = ls())

# load libraries 
library(sp)
library(sf)
library(maps)
library(yarrr)

# load pelagic longline data ------------------------------------------
# data request from June 23, 2023 - sent to M. Damiano by S. Alhale 
# file is "damiano_request623.xlsx" and folder is "Matt Damiano PLL request" on Google Drive

dat <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/PLL_1986_2022_catch_effort.csv")

head(dat)
dim(dat)

table(dat$TDOL, useNA = "always")  # target dolphin?    - ~8% targeted dolphin trips
table(dat$PLL, useNA = "always")   # PLL gear used Y/N  - ~91% PLL trips

# format latitude and longitude 
dat$lat <- dat$LATDEG + dat$LATMIN/60
dat$lon <- -(dat$LONDEG + dat$LONMIN/60)

# check location of points
#map("world")
#points(d$lon, d$lat, col = 2, pch = 19, cex = 0.4)

# look at distribution around Caribbean region ------------------
par(mfrow = c(6, 6), mex = 0.3)
for (i in 1990:2022) { 
  map('world', xlim = c(-90, -50), ylim = c(10, 30))
  axis(1); axis(2, las = 2); box()
  d1 <- dat[which(dat$SET_YEAR == i) ,]
  points(d1$lon, d1$lat, pch = 19, col = "#FF000055")
  mtext(side = 3, i)
  rect(xleft = -75, ybottom = 12, xright = -60, ytop = 23, col = NA, border = 4)
}

#  find the PLL points that fall in the Caribbean ----------------
dat$car <- 0
dat$car[which(dat$lon > (-70) & dat$lon < (-60) & dat$lat > 12 & dat$lat < 23)] <- 1

# check that subsetting was done correctly - color-coded areas
dev.off()
map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(dat$lon, dat$lat, pch = 19, cex = 0.4, col = as.numeric(as.factor(dat$col))+1)

table(dat$car, useNA = "always")
d <- dat[which(dat$car == 1), ]
dim(d)

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

d <- d[-which(d$HOOKS == 0), ]

# calculate CPUE
#dc$cpue <- dc$DOLPHIN_POUNDS / dc$HOOKS
d$cpue <- d$DOLTOT / d$HOOKS

# make new date variables 
d$year <- as.numeric(substr(d$SDATE, 1, 4))
d$mon  <- as.numeric(substr(d$SDATE, 5, 6))
d$monold  <- d$mon
table(d$year, useNA = "always")
table(d$mon, useNA = "always")
table(d$year == d$SET_YEAR)

d <- d[which(d$mon <=4), ]

par(mfrow = c(4, 4))
for (i in 26:52) {
  f <- tapply(d$cpue, d[, i], mean, na.rm = T)
  barplot(f, las = 1, main = names(d[i]))
}

barplot(tapply(d$cpue, d$year, mean, na.rm = T), las = 2)
barplot(tapply(d$cpue, d$mon, mean, na.rm = T))
barplot(tapply(d$cpue, d$PLL, mean, na.rm = T))
barplot(tapply(d$cpue, d$TDOL, mean, na.rm = T))

apply(d[,26:52], 2, table, useNA = "always")

# prepare factors for standardization --------------------

d$year <- as.factor(d$year)

d$tempbin <- cut(d$TEMP, breaks = c(0, seq(70, 85, 5)))
table(d$tempbin, useNA = "always")

d$mon <- as.factor(d$mon)
table(d$mon, useNA = "always")

table(d$HBFL)
d$HBFL[which(d$HBFL > 10)] <- NA
d$HBFLbin <- cut(d$HBFL, breaks = c(0, 2, 3, 4, 5, 10))
table(d$HBFLbin, useNA = "always")

table(d$BAIT)
hist(d$LIGHTS/d$HOOKS)

for (i in 32:38) {
  d[,i] <- d[,i] == "Y"
  d[,i] <- as.numeric(d[,i])
  d[,i] <- as.factor(d[, i])
}

par(mfrow = c(7, 2), mex = 0.5)
for (i in 32:38) {
  f <- tapply(d$cpue, d[, i], mean, na.rm = T)
  barplot(f, las = 1, main = names(d[i]))
  barplot(table(d[, i]), main = names(d[i]))
}

table(d$cpue == 0, useNA = "always")                     # # of observations > 1
dim(d)
d <- d[-is.na(d$cpue), ]
dim(d)
d$pres <- d$cpue
d$pres[which(d$pres > 0)] <- 1
table(d$pres, useNA = "always")

outp <- glm(pres ~ year + mon + tempbin + HBFLbin + TMIX + TSWO, data = d, family = "binomial")
summary(outp)        
anova(outp) 

dp <- d[which(d$cpue > 0), ]

out <- glm(log(cpue) ~ year + mon + tempbin + HBFLbin + TMIX + TSWO, data = dp, family = "gaussian")
summary(out)        
anova(out) 

table(predpos$year == predlogit$year)

# combined variance function 
comb.var <- function(A, Ase, P, Pse, p) { (P^2 * Ase^2 + A^2 * Pse^2 + 2 * p * A * P * Ase * Pse)  }   # combined var

predlogit <- as.data.frame(emmeans(outp, specs = ~ year, type = "response"))
predpos  <- as.data.frame(emmeans(out, specs = ~ year, type = "response"))

co <- as.numeric(cor(predlogit$prob, predpos$response, method="pearson"))
predse <- sqrt(comb.var(predpos$response, predpos$SE, predlogit$prob, predlogit$SE, co))
predind <-  predlogit$prob * predpos$response    # estimated abundance is prob. of occurrence * estimated abundance when present

yrs <- as.numeric(as.vector(predpos$year))

nom <- tapply(d$cpue, d$year, mean, na.rm = T)
table(as.numeric(names(nom)) == yrs)

plot(yrs, predind, type = "l", lwd = 2, col = 4, main = "Nominal versus standardized CPUE from PLL data",
     xlab = "year", ylab = "CPUE (fish / hooks)", ylim = c(0, 0.015))
points(yrs, predind, pch = 1, lwd = 2, col = 4) 
lines(yrs, predind - predse, col = 4, lty = 2)
lines(yrs,predind + predse, col = 4, lty = 2)
lines(yrs, nom, col = 2, lwd = 2)
points(yrs, nom, col = 2, lwd = 2, pch = 1)
legend("topleft", c("nominal", "standardized"), lwd = 2, pch = 19, col = c(2, 4), bty = "n")

ind <- data.frame(cbind(yrs, predind, predse, predlogit$prob, predpos$response, nom))
names(ind) <- c("Year", "index", "SE", "predpos", "Npres", "nominal")


rec <- read.csv("data/recLandings.csv")
names(rec)[1] <- "Year"
rec$ATL <- rowSums(rec[3:6], na.rm = T)
rec <- rec[which(rec$Year >= 1990), ]

d1 <- merge(rec, ind, by = "Year")


par(mfrow = c(2, 2))
plot(d1$index, d1$ATL, col = 0)
text(d1$index, d1$ATL, d1$Year, col = 1)
out <- lm(d1$ATL ~ d1$index)
abline(out, col = 8)
summary(out)
r2 <- summary(out)$adj.r.squared
p_val <- summary(out)$coefficients[2, 4]
p_display <- ifelse(p_val < 0.001, "p < 0.001", paste("p =", round(p_val, 3)))
legend("bottomright", 
       legend = bquote(R^2 == .(round(r2, 2)) ~ "; " ~ p == .(round(p_val, 3))),
       bty = "n", cex = 1.2, text.col = 4)

plot(d1$nominal, d1$ATL, col = 0)
text(d1$nominal, d1$ATL, d1$Year, col = 1)
out <- lm(d1$ATL ~ d1$nominal)
abline(out, col = 8)
summary(out)
r2 <- summary(out)$adj.r.squared
p_val <- summary(out)$coefficients[2, 4]
p_display <- ifelse(p_val < 0.001, "p < 0.001", paste("p =", round(p_val, 3)))
legend("bottomright", 
       legend = bquote(R^2 == .(round(r2, 2)) ~ "; " ~ p == .(round(p_val, 3))),
       bty = "n", cex = 1.2, text.col = 4)

plot(d1$predpos, d1$ATL, col = 0)
text(d1$predpos, d1$ATL, d1$Year, col = 1)
out <- lm(d1$ATL ~ d1$predpos)
abline(out, col = 8)
summary(out)
r2 <- summary(out)$adj.r.squared
p_val <- summary(out)$coefficients[2, 4]
p_display <- ifelse(p_val < 0.001, "p < 0.001", paste("p =", round(p_val, 3)))
legend("bottomleft", 
       legend = bquote(R^2 == .(round(r2, 2)) ~ "; " ~ p == .(round(p_val, 3))),
       bty = "n", cex = 1.2, text.col = 4)

plot(d1$Npres, d1$ATL, col = 0)
text(d1$Npres, d1$ATL, d1$Year, col = 1)
out <- lm(d1$ATL ~ d1$Npres)
abline(out, col = 8)
summary(out)
r2 <- summary(out)$adj.r.squared
p_val <- summary(out)$coefficients[2, 4]
p_display <- ifelse(p_val < 0.001, "p < 0.001", paste("p =", round(p_val, 3)))
legend("bottomright", 
       legend = bquote(R^2 == .(round(r2, 2)) ~ "; " ~ p == .(round(p_val, 3))),
       bty = "n", cex = 1.2, text.col = 4)


