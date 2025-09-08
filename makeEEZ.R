


# U.S. Atlantic EEZ shpaefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=8456
# used FAO regions to define other boundaries for Western Central Atlantic https://www.fao.org/fishery/en/area/search

#install.packages("sf")

library(sf)

rm(list = ls())

shp_data <- st_read("data/eez/eez.shp")

coords <- st_coordinates(shp_data)
head(coords)
co <- as.data.frame(coords)
#plot(co$X, co$Y)

co <- co[which(co$X > (-82)), ]
plot(co$X, co$Y)

co <- co[-which(co$X < (-81) & co$Y > 25 & co$Y < 28), ]
plot(co$X, co$Y)

#co <- co[seq(1, nrow(co), 10), ]

#plot(co$X, co$Y, col = 0)
#text(co$X, co$Y, 1:nrow(co), cex = 0.7)
#plot(co$X, co$Y, col = 0, xlim = c(-85, -80), ylim = c(22, 25))
#text(co$X, co$Y, 1:nrow(co), cex = 0.7)

co1 <- co[2100:3727, ]
plot(co1$X, co1$Y)

co2 <- co[-c(2100:3727), ]
co3 <- co2[which(co2$Y > 35), ]
co4 <- co2[which(co2$Y <= 35), ]

co3 <- co3[seq(1, nrow(co3), 1100), ]
co4 <- co4[seq(1, nrow(co4), 800), ]

points(co3$X, co3$Y, col = 2)
points(co4$X, co4$Y, col = 4)

co2 <- rbind(co3, co4)
co2 <- co2[order(co2$Y), ]
cofin <- rbind(co1, co2)

polygon(cofin$X, cofin$Y)

library(maps)

map(database = "usa", xlim = c(-83, -65), ylim = c(22, 47), col = 8)
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2, lwd = 2)
abline(h = 37.5)
abline(v = -74)

cofin <- cofin[-which(cofin$X < (-74) & cofin$Y > 37.6), ]

text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.9)

lis <- c(1634, 1639, 1688, 1710, 1711, 1712)

cofin <- cofin[-lis, ]

map(database = "usa", xlim = c(-70, -65), ylim = c(43, 47), col = 8)
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2, lwd = 2)
text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.9)

map(database = "usa", xlim = c(-72, -71), ylim = c(34.8, 35.1))
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2)
text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.6)
abline(h=35)
abline(v = -60)

cofin <- cofin[, 1:2]

map(database = "usa", xlim = c(-83, -50), ylim = c(5, 47))
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2)

cofin <- cofin[-which(cofin$Y < 24), ]

cofin[1624, 2] <- 24

eez <- cofin
#write.csv(eez, file = "eez.csv")

# other regions

which.min(cofin$Y)
cofin[1624, ]

map(database = "usa", xlim = c(-77, -76.4), ylim = c(28, 28.9))
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2)
text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.6)
car1 <- cofin[1143:1624, ]
car2 <- data.frame(cbind(c(-90, -90, -60, -60, -71.1), c(24, 8, 8, 24, 24)))
names(car2) <- c("X", "Y")
CAR <- rbind(car1, car2)
names(CAR) <- c("X", "Y")
polygon(CAR$X, CAR$Y, col = 2)

nca1 <- cofin[569:1143, ]
nca2 <- data.frame(cbind(c(-71.1, -60, -60, -40, -40), c(24, 24, 13, 13, 35)))
names(nca2) <- c("X", "Y")
NCA <- rbind(nca1, nca2)
names(NCA) <- c("X", "Y")
polygon(NCA$X, NCA$Y, col = 3)

which.max(cofin$Y)
cofin[1702, ]

ned1 <- cofin[1:569, ]
ned2 <- data.frame(cbind(c(-42, -42, -67.1), c(35, 55, 55)))
names(ned2) <- c("X", "Y")
NED <- rbind(ned1, ned2)
names(NED) <- c("X", "Y")
polygon(NED$X, NED$Y, col = 4)

which.min(abs(co1$Y - 28))

map(database = "usa", xlim = c(-81, -78), ylim = c(27.5, 28.9))
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2)
text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.6)
points(-80.46, 28)
cofin[1632, ] <- c(-80.46, 28)
abline(h = 28)
FLK <- cofin[1289:1632, ]

map(database = "usa", xlim = c(-72, -70), ylim = c(34.5, 35.4))
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2)
text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.6)
abline(h = 35)
cofin[1663, 2] <- 35
NCFL <- rbind(cofin[1632:1663, ], cofin[569:1289, ])

map(database = "usa", xlim = c(-72, -70), ylim = c(36.5, 37.4))
axis(1); axis(2)
polygon(cofin$X, cofin$Y, border = 2)
text(cofin$X, cofin$Y, 1:nrow(cofin), cex = 0.6)
abline(h = 37)
cofin[425, 2] <- 37
NNC <- rbind(cofin[1663:1673, ], cofin[425:569, ])

VBM <- rbind(cofin[1673:1702, ], cofin[1:425, ])

# plot final regions --------------------------

map(database = "world", xlim = c(-92, -38), ylim = c(5, 55), col = 2)
axis(1); axis(2); box()

polygon(CAR$X, CAR$Y, border = 1)
polygon(NCA$X, NCA$Y, border = 1)
polygon(NED$X, NED$Y, border = 1)
polygon(FLK$X, FLK$Y, border = 1)
polygon(NCFL$X, NCFL$Y, border = 1)
polygon(NNC$X, NNC$Y, border = 1)
polygon(VBM$X, VBM$Y, border = 1)

text(-75, 15, "CAR", cex = 1.2)
text(-57, 28, "NCA", cex = 1.2)
text(-57, 42, "NED", cex = 1.2)
text(mean(FLK$X), mean(FLK$Y), "FLK", cex = 1.2)
text(-79.3, mean(NCFL$Y), "NCFL", cex = 1.2)
text(-73.8, mean(NNC$Y), "NNC", cex = 1.2)
text(-71.5, mean(VBM$Y), "VBM", cex = 1.2)

class(CAR)

CAR$region <- "CAR"
FLK$region <- "FLK"
NCFL$region <- "NCFL"
NNC$region <- "NNC"
VBM$region <- "VBM"
NCA$region <- "NCA"
NED$region <- "NED"

zones <- rbind(FLK, NCFL, NNC, VBM, NED, NCA, CAR)

plot(zones$X, zones$Y, col = as.numeric(as.factor(zones$region)))

write.csv(zones, file = "data/eez/dolphin_OM_areas.csv", row.names = FALSE)



