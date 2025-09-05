## Merge high seas with catch with exclusive economic zones

Now we will merge in the high seas catch from the steps above into the EEZ catches to get a better picture of total catch in FAO zones 31 and 21. Recall that the U.S. Gulf and Atlantic catches are not necessarily parsed apart correctly in the public databases which get compiled into Sea Around Us, so these will have the same deficiencies as noted in Section 2. Later we will merge in the corrected data.  

```{r}
#| echo: true

hs <- read.csv("data/SAU/high_seas_by_year.csv")
names(hs)[1] <- "year"

# check that columns match
#head(tab)
#head(hs)
table(rownames(tab) == hs$year)  

tab[which(is.na(tab))] <- 0
tab_atl <- cbind(tab, hs[2:3])

# check the merge
head(tab_atl)

# separate all WCA catches 
unique(d$reg)  # use Western Central, U.S. and Canada
dwc <- d[which(d$reg == "Western Central Atlantic EEZs" | d$reg == "United States EEZ" | d$reg == "Canadian EEZ"),]

#unique(dwc$area_name[which(dwc$reg == "United States EEZ")])
#unique(dwc$area_name[which(dwc$reg == "Canadian EEZ")])
#unique(dwc$area_name[which(dwc$reg == "Western Central Atlantic EEZs")])

# relabel U.S. by area
dwc$reg[which(dwc$area_name == "USA (East Coast)")] <- "United States East Coast"
dwc$reg[which(dwc$area_name == "USA (Gulf of Mexico)")] <- "United States Gulf"
dwc$reg[which(dwc$area_name == "Puerto Rico (USA)")] <- "United States Caribbean"
dwc$reg[which(dwc$area_name == "US Virgin Isl.")] <- "United States Caribbean"
#table(dwc$area_name, dwc$reg)

tab_wc <- tapply(dwc$lbs, list(dwc$year, dwc$reg), sum, na.rm = T)
#head(tab_wc)

# reorder S to N
tab_wc <- tab_wc[, c(5, 2, 4, 3, 1)]
head(tab_wc)

# merge in the high seas catch - check years match
#head(hs)
table(rownames(tab_wc) == hs$year)

high_seas <- hs$Atlantic..Western.Central + hs$Atlantic..Northwest
high_seas[high_seas == 0] <- NA
tab_wc <- cbind(tab_wc, high_seas)
head(tab_wc)
```

## Plot Western Atlantic catches

Now we plot the total Western Atlantic catches -- both within the EEZs and the high seas -- separating by U.S., Canada, and the Western Central Atlantic (i.e., the Wider Caribbean). Most of the catch is coming from the United States and the 40 combined Caribbean jurisdictions which comprise the Western Central Atlantic.  

```{r}
#| echo: true

par(mar = c(5, 4, 4, 3), mgp = c(3, 1, 0))
matplot(yrs, tab_wc/10^6, las = 2, type = "l", lty = 1, lwd = 3, col = c(1, 3:7), 
        xlab = "", ylab = "total catch (millions of pounds)", 
        main = "Western Atlantic dolphin catches (commercial + recreational)")
legend("topleft", colnames(tab_wc), col = c(1, 3:7), lty = 1, lwd = 3, bty = "n")

```

Separating out the mainland United States catches, here are the international and domestic (mainland) catches plotted over time.   

```{r}
#| echo: true

tab_int <- tab_wc[, c(1, 2, 5, 6)]
head(tab_int)
int_catch <- round(rowSums(tab_int, na.rm = T), 2)
dom_catch <- rowSums(tab_wc[, c(3, 4)], na.rm = T)

matplot(yrs, cbind(int_catch, dom_catch)/10^6, las = 2, type = "l", lty = 1, lwd = 3, col = c(2, 4),
        xlab = "", ylab = "total catch (millions of pounds)", 
        main = "Western Atlantic dolphin catches in international vs domestic waters\n(FAO areas 21 and 31, commercial + recreational)")
legend("topleft", c("international", "United States mainland"), col = c(2, 4), lty = 1, lwd = 3, bty = "n")

# output international catches
write.csv(data.frame(int_catch), file = "data/international_catches.csv", row.names = T)
```


