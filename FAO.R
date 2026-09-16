
# clear workspace
rm(list = ls())

# libraries
library(pals)

df <- read.csv("data/FAO_dolphin_data.csv")

apply(df, 2, table)

df$lbs <- df$Landings_Tonnes * 2204.62

df_recent <- df[which(df$YEAR >= 2015), ]

par(mar = c(14, 4, 1, 1), mfrow = c(2, 1))
barplot(sort(tapply(df$lbs/10^6, df$JURISDICTION, mean, na.rm = T)), las = 2, horiz = F, cex.names = 1,
        ylab = "millions of pounds", main = "average annual landings (FAO 1950 - 2022)")
barplot(sort(tapply(df_recent$lbs/10^6, df_recent$JURISDICTION, mean, na.rm = T)), las = 2, horiz = F, 
        ylab = "millions of pounds", main = "average annual recent landings (FAO 2015 - 2022)")

df_int <- df[which(df$JURISDICTION != "United States of America"), ]

dev.off()
tot <- tapply(df_int$lbs, df_int$YEAR, sum, na.rm = T)
plot(names(tot), tot/10^6, type = "l", xlab = "year", ylab = "millions of pounds", las = 2,
     main = "total international dolphin catch reported by FAO in zones 21 and 31")


lis <- names(sort(tapply(df_int$lbs/10^6, df_int$JURISDICTION, mean, na.rm = T), decreasing = T))

df_int$JURISDICTION <- factor(df_int$JURISDICTION, levels = lis)

fao <- tapply(df_int$lbs, list(df_int$JURISDICTION, df_int$YEAR), sum, na.rm = T)
fao[is.na(fao)] <- 0
fao
cols <- glasbey(26)

# plot FAO vs SAU

b <- barplot(fao/10^6, names.arg = 1950:2022, col = cols, border = NA, las = 2, ylim = c(0, 15), 
        ylab = "millions of pounds", main = "FAO reported landings by jurisdiction")
legend("topleft", ncol = 2, rownames(fao), col = cols, pch = 15, bty = "n", title = "FAO landings")
abline(h = 0)

#d <- read.csv("data/FINAL_files/complete_catch_dataset_09052025b.csv")
#d <- d[which(d$Fleet == "Intl"), ]
#dtot <- tapply(d$Catch_lbs, d$Year, sum, na.rm = T)
#xs <- b[which(1950:2022 >= 1986)]
#lines(xs, dtot/10^6, lwd = 4)

# load data
load("data/outputs/SAU_EEZs_WCA.RData")  # SAU EEZ data for NED and NCA saved from earlier
ds <- dwest

# remove bad catches from the database prior to summarizing
dim(ds)
ds <- ds[-which(ds$area_name == "Venezuela" & ds$fishing_entity == "Unknown Fishing Country"), ]
ds <- ds[-which(ds$area_name == "Curaçao (Netherlands)" & ds$fishing_entity == "USA"), ]
dim(ds)

tot <- tapply(ds$lbs, ds$year, sum, na.rm = T)  # total catch from SAU
xs <- b[which(1950:2022 <= 2019)]

tab_ds <- tapply(ds$lbs, list(ds$year, ds$catch_type, ds$reporting_status), sum, na.rm = T)
tail(tab_ds)
repSAU <- tab_ds[, 2, 1]                        # reported catch from SAU
#unrepSAU <- tab_ds[, 2, 2]
#discSAU <- tab_ds[, 1, 2]

lines(xs, tot/10^6, lty = 3, col = "#00000070", lwd = 3)
lines(xs, repSAU/10^6, lty = 1, col = 1, lwd = 3)

legend("left", c("reported + unreported", "reported"), title = "SAU landings", 
                 lwd = 3, lty = c(3, 1), col = c("#00000070", 1), bty = "n")


sau <- tapply(ds$lbs, list(ds$year, ds$reporting_status, ds$area_name), sum, na.rm = T)
dim(sau)
sau[is.na(sau)] <- 0
sau

lk <- read.csv("data/FAO_SAU_lookup.csv", fileEncoding = "Windows-1252")

par(mfrow = c(5, 9), mar = c(1, 2.5, 1.5, 0.5), mgp = c(1.5, 0.5, 0))
for (i in 1:dim(sau)[3])  { 
    b <- barplot(t(sau[, , i])/10^6, col = c(3, 2), 
          names.arg = rep(NA, length(1950:2019)),
          beside = F, border = NA, space = 0, 
          xlab = "", ylab = "", ylim = c(0, 4),
          axes = F)
    repunrep <- round(rowMeans(t(sau[, , i])/10^6, na.rm = T), 2)
    axis(1, at = b[seq(10, 70, 20)], lab = c("60", "80", "00", "20"), 
         tick = F, pos = 0.2)
  axis(2, las = 2, tcl = -0.2, at = 0:3)
  maintext <- dimnames(sau)[[3]][i]
  maintext <- gsub("\\band\\b", "&", maintext, ignore.case = TRUE)
  maintext <- gsub("\\b(Saint|St\\.)\\b", "St", maintext, ignore.case = TRUE)
  mtext(side = 3, maintext, cex = 0.8)
  
  mtch <- lk$FAO[which(lk$SAU == dimnames(sau)[[3]][i])]
  if (mtch != "") { 
    f <- fao[which(rownames(fao) == mtch), 1:70]
    lines(b, f/10^6, col = 1)
    repunrep[3] <- round(mean(f, na.rm = T) / 10^6, 2)
  }
  lpos <- "topleft"
  if (i == 5) { lpos = "topright"}
  legend(lpos, as.character(repunrep)[3:1], text.col = c(1, 2, 3), bty = "n")
}
plot.new()
legend("center", c("FAO", "SAU unreported", "SAU reported"), 
       col = 1:3, lty = c(1, 0, 0), pch = c(NA, 15, 15))



SAU <- tapply(ds$lbs, list(ds$year, ds$area_name), sum, na.rm = T)
maxland <- SAU
dim(maxland)
maxland[is.na(maxland)] <- 0

maxland <- rbind(maxland, maxland[nrow(maxland),], maxland[nrow(maxland),], maxland[nrow(maxland),])


for (i in 1:ncol(maxland))  { 
  mtch <- lk$FAO[which(lk$SAU == dimnames(maxland)[[2]][i])]
  if (mtch != "")  { 
  f <- fao[which(rownames(fao) == mtch), ]   
  temp <- cbind(f, maxland[, i])
  maxl <- apply(temp, 1, max, na.rm = TRUE)
  maxland[, i] <- maxl
  }
}

par(mfrow = c(5, 9), mar = c(1, 2.5, 1.5, 0.5), mgp = c(1.5, 0.5, 0))

for (i in 1:dim(maxland)[2])  { 
  plot(1950:2022, maxland[, i]/10^6, type = "l", 
       xlab = "", ylab = "", ylim = c(0, 4), axes = F)
  axis(1, at = seq(1960, 2020, 20), lab = c("60", "80", "00", "20"), 
       tick = F, pos = 0.2)
  axis(2, las = 2, tcl = -0.2, at = 0:4)
  maintext <- dimnames(sau)[[3]][i]
  maintext <- gsub("\\band\\b", "&", maintext, ignore.case = TRUE)
  maintext <- gsub("\\b(Saint|St\\.)\\b", "St", maintext, ignore.case = TRUE)
  mtext(side = 3, maintext, cex = 0.8)
  }

par(mar = c(14, 4, 1, 1), mfrow = c(2, 1), mgp = c(2.5, 0.75, 0))
barplot(sort(colMeans(maxland, na.rm = T)/10^6), las = 2, horiz = F, cex.names = 1,
        ylab = "millions of pounds", main = "average annual landings (maximum of FAO or SAU, 1950 - 2022)")
barplot(sort(colMeans(tail(maxland, 8), na.rm = T)/10^6), las = 2, horiz = F, 
        ylab = "millions of pounds", main = "average annual recent landings (maximum of FAO or SAU, 2015 - 2022)")

dim(maxland)
dim(SAU)
dim(t(fao))

dev.off()
plot(1950:2022, rowSums(maxland, na.rm = T)/10^6, type = "l", ylim = c(0, 22), las = 1, lwd = 2,
     xlab = "year", ylab = "millions of pounds", 
     main = "Total estimated international dolphin landings from different data sources")
lines(1950:2019, rowSums(SAU, na.rm = T)/10^6, col = 2, lty = 2, lwd = 2)
lines(1950:2022, colSums(fao, na.rm = T)/10^6, col = 4, lty = 3, lwd = 2)
text(2022, round(sum(maxland[nrow(maxland), ])/10^6, 1), round(sum(maxland[nrow(maxland), ])/10^6, 1), pos= 1)
text(2022, round(sum(SAU[nrow(SAU), ], na.rm = T)/10^6, 1), round(sum(SAU[nrow(SAU), ], na.rm = T)/10^6, 1), pos= 1, col = 2)
text(2022, round(sum(fao[, ncol(fao)], na.rm = T)/10^6, 1), round(sum(fao[, ncol(fao)], na.rm = T)/10^6, 1), pos= 1, col = 4)
legend("topleft", c("maximum of either SAU or FAO", "SAU (Sea Around Us)", "FAO (Food and Agriculture Organization)"), 
       col = c(1, 2, 4), lty = c(1, 2, 3), lwd = 3)

