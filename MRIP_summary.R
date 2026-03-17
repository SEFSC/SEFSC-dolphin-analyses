# code for summarizing MRIP data at regional level
# M. Karnauskas Mar 17 2026

# clear workspace
rm(list = ls())

# read in data file
load("data/mrip_fes_rec81_25wv2_23June25.RData")  # most recent file on SFD server
head(dat)

#apply(dat[2:18], 2, table, useNA = "always")

# confirm nomenclature for dolphin and subset by species --------------------------
table(dat$NEW_COM[grep("dolphin", dat$NEW_COM)])
table(dat$NEW_SCI[grep("dolphin", dat$NEW_COM)])

d <- dat[which(dat$NEW_COM == "dolphin"), ]
d <- d[which(d$YEAR < 2025), ]    # take out 2025 because incomplete year 
tot <- tapply(d$LBSEST_SECWWT, d$YEAR, sum, na.rm = T)

# Separate Gulf and Atlantic recreational catches
# codes for WFL regions: 1 (NW FL Panhandle- Escambia to Dixie co.), 2 (SW FL Peninsula- Levy to Collier co), 3 (FL Keys- Monroe co.)
# subregional codes: 6 is Atlantic and 7 is Gulf

table(d$FL_REG, d$SUB_REG, useNA = "always")
unique(d$NEW_STA)

d1 <- d[which(d$COUNCILREG == 6), ]
dg <- d[which(d$COUNCILREG == 7), ]
table(d1$COUNCILREG)

# summarize by year and compare ---------------------------------------

tot1 <- tapply(d1$LBSEST_SECWWT, d1$YEAR, sum, na.rm = T)
old <- read.csv("data/RecreationalDolphinLandings_SAandGOM_MLarkin.csv", skip = 6)
plot(old$Year, old$ATL, type = "l", xlab = "year", ylab = "total catch (pounds)", lwd = 4, 
     xlim = c(1980, 2025), ylim = c(7*10^6, 3.3*10^7), bty = "n")
lines(as.numeric(names(tot1)), tot1, lwd = 2, col = 2, lty = 2)
legend("topright", c("previous data", "new data"), col = c(1, 2), lwd = c(4, 2), lty = c(1, 2))

# Partition catches by area and sector --------------------------------

#The areas are as follows:
#  -   FLK: Florida Keys to Indian River County County (FL)
#  -   NCFL: Brevard (FL) to Southern NC (South of Hatteras)
#  -   NNC: Northern NC (North of Hatteras to NC/VA border)
#  -   VBM: Virginia to Maine

# separate out 4 regions --------------------------

d1$region <- "VBM"

# select for FLK
# FL_REG codes: 4: SE FL- Miami-Dade to Indian River County.; 5: NE FL- Brevard to Nassau County
table(d1$FL_REG, d1$NEW_STA, useNA = "always")
d1$region[which(d1$FL_REG == 3 | d1$FL_REG == 4)] <- "FLK"

# select for NCFL
d1$region[which(d1$FL_REG == 5)] <- "NCFL"
lis <- c("SC", "GA", "FLE/GA")           # NCFL states
d1$region[which(d1$NEW_STA %in% lis)] <- "NCFL"

# NC_REG codes: N: North of Cape Hatteras; S: South of Cape Hatteras
table(d1$NC_REG, d1$NEW_STA, useNA = "always")
d1$region[which(d1$NC == "S")] <- "NCFL"

# select for NNC
d1$region[which(d1$NC == "N")] <- "NNC"

table(d1$FL_REG, d1$NEW_STA, d1$region)
table(d1$NC_REG, d1$NEW_STA, d1$region)  # check classification

# summarize by Gulf and Atlantic regions and check against total -----------------------
atl <- tapply(d1$LBSEST_SECWWT, list(d1$YEAR, d1$region), sum, na.rm = T)
GOM <- tapply(dg$LBSEST_SECWWT, dg$YEAR, sum, na.rm = T)

table(rownames(atl) == names(GOM))

rec <- cbind(GOM, atl)

plot(rowSums(rec), tot)
rowSums(rec)/tot

# plot landings -------------------------------------------------
matplot(rownames(rec), rec, type = "l", lty = 1, col = 1:5)
legend("topright", colnames(rec), col = 1:5, lty = 1)

write.csv(rec, file = "data/recLandings.csv")

## END ## 


