## Látszik-e az xlsx?
dir()
## Beolvasás
library(readxl)
data.raw <- as.data.frame(read_excel("F_L_2023_complete_soil_dataset_T_probara.xlsx"))
names(data.raw) <- c("ID", "InOut", "wOkt", "wDec", "wFeb", "wMar", "meanh(cm)", "Dens(db/m2)")
for(ttcolname in c("wOkt", "wDec", "wFeb", "wMar"))
    data.raw[, ttcolname] <- round(as.numeric(data.raw[, ttcolname]),3)
data.raw[, "InOut"] <- factor(data.raw[, "InOut"])

## F és T próbák
var.test(wOkt ~ InOut, data.raw)
t.test(wOkt ~ InOut, data.raw, var.eq = TRUE)
t.test(wMar ~ InOut, data.raw)

## Boxplot vagy doboz ábra
boxplot(wOkt ~ InOut, data.raw, ylab = "WWC Október")


## Nedvesség változás
WWC <- data.frame(InOut = rep(data.raw$InOut, 4), Month = factor(rep(c("Okt", "Dec", "Feb", "Mar"), each = nrow(data.raw))), WWC = c(data.raw$wOkt, data.raw$wDec, data.raw$wFeb, data.raw$wMar))

boxplot(WWC ~ InOut, WWC)

## InOut tömbben
WWC$InOMonth  <- factor(paste0(WWC$InOut, WWC$Month),
                        levels = c("IOkt", "IDec", "IFeb", "IMar", "OOkt", "ODec", "OFeb", "OMar"))

## InOut havonta
WWC$InOMonth  <- factor(paste0(WWC$InOut, WWC$Month),
                        levels = c("IOkt", "OOkt", "IDec", "ODec", "IFeb", "OFeb", "IMar", "OMar"))

boxplot(WWC ~ InOMonth, WWC)

## havontára
boxplot(WWC ~ InOMonth, WWC, xlab = "", xaxt = "n")
axis(1, at = 1:8, labels = rep(c("I", "O"), 4))
par(mgp = c(3,2.5,0))
axis(1, at = c(1.5, 3.5, 5.5, 7.5), labels = c("Október", "December", "Február", "Március"), tcl = 0)
par(mgp = c(3,1,0))
