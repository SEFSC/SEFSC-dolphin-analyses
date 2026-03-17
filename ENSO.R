# ENSO 

library(readr)

url <- "https://www.psl.noaa.gov/enso/mei/data/meiv2.data"
download.file(url = url, destfile = "data/enso.txt")

all_lines <- readLines("data/enso.txt")
footer_start <- grep("-999", all_lines)

e <- read.table("data/enso.txt", skip = 1, nrows = footer_start[1] - 2)
names(e) <- c("Year", "DJ", "JF", "FM", "MA", "AM", "MJ", "JJ", "JA", "AS", "SO", "ON", "ND")
e$mean <- rowMeans(e[2:13], na.rm = T)

rec <- read.csv("data/recLandings.csv")
names(rec)[1] <- "Year"

rec$ATL <- rowSums(rec[3:6], na.rm = T)
rec$ALL <- rec$GOM + rec$ATL

d <- merge(rec, e, by = "Year")
head(d)

lis <- names(d)[2:7]
par(mfrow = c(2, 3), mex = 0.8)
for(i in 1:6) {
  catch <- d[, which(names(d) == lis[i])]
  plot(d$mean, catch, xlab = "annual ENSO index", ylab = "recreational landings", main = paste0("region = ", lis[i]))
  out <- lm(catch ~ d$mean)
  abline(out)
  p <- summary(out)$coef[2, 4]
  co <- 1
  if (p < 0.01)  { co <- 4 }
  if (p < 0.001) { co <- 6 }
  legend("topleft", paste0("R^2 = ", round(summary(out)$adj.r.squared, 2), "; p = ", 
                           round(p, 3)), cex = 1.2, bty = "n", text.col = co)}

d <- d[which(d$Year >= 1990), ]

lis <- names(d)[2:7]
par(mfrow = c(2, 3), mex = 0.8)
for(i in 1:6) {
  catch <- d[, which(names(d) == lis[i])]
  plot(d$mean, catch, xlab = "annual ENSO index", ylab = "recreational landings", main = paste0("region = ", lis[i]))
  out <- lm(catch ~ d$mean)
  abline(out)
  p <- summary(out)$coef[2, 4]
  co <- 1
  if (p < 0.01)  { co <- 4 }
  if (p < 0.001) { co <- 6 }
  legend("topleft", paste0("R^2 = ", round(summary(out)$adj.r.squared, 2), "; p = ", 
                           round(p, 3)), cex = 1.2, bty = "n", text.col = co)}

lis <- names(e)[2:13]
par(mfrow = c(3, 4), mex = 0.8)
for(i in 1:12) {
  ind <- d[, which(names(d) == lis[i])]
  plot(ind, d$ALL, xlab = paste(lis[i], "ENSO index"), ylab = "total recreational landings")
  out <- lm(d$ALL ~ ind)
  abline(out)
  p <- summary(out)$coef[2, 4]
  co <- 1
  if (p < 0.01)  { co <- 4 }
  if (p < 0.001) { co <- 6 }
  legend("topleft", paste0("R^2 = ", round(summary(out)$adj.r.squared, 2), "; p = ", 
                                    round(p, 3)), cex = 1.2, bty = "n", text.col = co)
}

par(mfrow = c(1, 1))
plot(d$JJ, d$ALL, xlab = "June-July ENSO index", ylab = "total recreational landings", col = 0)
text(d$JJ, d$ALL, d$Year)
out <- lm(d$ALL ~ d$JJ)
abline(out, col = 6)
p <- summary(out)$coef[2, 4]
legend("topleft", paste0("R^2 = ", round(summary(out)$adj.r.squared, 2), "; p = ", 
                         round(p, 3)), cex = 1.2, bty = "n", text.col = 1)
summary(out)

ind <- data.frame(cbind(d$Year, d$JJ))
write.csv(ind, file = "indices/enso_index.csv", row.names = F)
