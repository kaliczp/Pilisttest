## Látszik-e az xlsx?
dir()
## Beolvasás
library(readxl)
data.raw <- as.data.frame(read_excel("F_L_2023_complete_soil_dataset_T_probara.xlsx"))
names(data.raw) <- c("ID", "InOut", "wOkt", "wDec", "wFeb", "wMar", "meanh.cm", "dens.db.m2", "wJun", "wJul", "wAug", "wSept")
for(ttcolname in c("wOkt", "wDec", "wFeb", "wMar"))
    data.raw[, ttcolname] <- round(as.numeric(data.raw[, ttcolname]),3)
data.raw[, "InOut"] <- factor(data.raw[, "InOut"])

## F és T próbák
var.test(wOkt ~ InOut, data.raw)
t.test(wOkt ~ InOut, data.raw, var.eq = TRUE)
t.test(wMar ~ InOut, data.raw)

## Szórás azonos, átlag szignifikánsan különbözik kívül-belül között
var.test(wJul ~ InOut, data.raw)
t.test(wJul ~ InOut, data.raw, var.eq = TRUE)
boxplot(wJul ~ InOut, data.raw, ylab = "WWC Július")

## A meanh szórás és átlag is különböző
var.test(meanh.cm ~ InOut, data.raw)
t.test(meanh.cm ~ InOut, data.raw, var.eq = FALSE)
boxplot(meanh.cm ~ InOut, data.raw, ylab = "")

## A dens.db.m2 nincs szigifikáns különbség
var.test(dens.db.m2 ~ InOut, data.raw)
t.test(dens.db.m2 ~ InOut, data.raw, var.eq = TRUE)
boxplot(dens.db.m2 ~ InOut, data.raw, ylab = "")

## Boxplot vagy doboz ábra
boxplot(wOkt ~ InOut, data.raw, ylab = "WWC Október")

## Nedvesség változás
WWC <- data.frame(InOut = rep(data.raw$InOut, 8), Month = factor(rep(c("Jún", "Júl", "Aug", "Szep", "Okt", "Dec", "Feb", "Mar"), each = nrow(data.raw))), WWC = c(data.raw$Jun, data.raw$Jul, data.raw$Aug, data.raw$Sept, data.raw$wOkt, data.raw$wDec, data.raw$wFeb, data.raw$wMar))

boxplot(WWC ~ InOut, WWC)

## InOut tömbben
WWC$InOMonth  <- factor(paste0(WWC$InOut, WWC$Month),
                        levels = c("IJún", "IJúl", "IAug", "ISzep", "IOkt", "IDec", "IFeb", "IMar", "OJún", "OJúl", "OAug", "OSzep", "OOkt", "ODec", "OFeb", "OMar"))

## InOut havonta
WWC$InOMonth  <- factor(paste0(WWC$InOut, WWC$Month),
                        levels = c("IJún", "OJún", "IJúl", "OJúl", "IAug", "OAug", "ISzep", "OSzep", "IOkt", "OOkt", "IDec", "ODec", "IFeb", "OFeb", "IMar", "OMar"))

boxplot(WWC ~ InOMonth, WWC)

## havontára
boxplot(WWC ~ InOMonth, WWC, xlab = "", xaxt = "n", col = c("#fc8d59", "#99d594"), ylab = "VWC")
axis(1, at = 1:16, labels = rep(c("I", "O"), 8))
par(mgp = c(3,2.5,0))
axis(1, at = seq(1.5, by = 2, length.out = 8), labels = c("Június", "Július", "Augusztus", "Szeptember", "Október", "December", "Február", "Március"), tcl = 0)
par(mgp = c(3,1,0))

## Másik adatbázis
data2.raw <- as.data.frame(read_excel("F_L_porosity_T_proba.xlsx"))
names(data2.raw) <- c("pont", "n", "InOut")
boxplot(n ~ InOut, data2.raw)
## F és t próba
var.test(n ~ InOut, data2.raw)
t.test(n ~ InOut, data2.raw, var.eq = TRUE)
