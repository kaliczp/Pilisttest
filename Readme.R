## Látszik-e az xlsx?
dir()
## Beolvasás
library(readxl)
data.raw <- as.data.frame(read_excel("F_L_2023_complete_soil_dataset_T_probara.xlsx"))
names(data.raw) <- c("ID", "InOut", "wOkt", "wDec", "wFeb", "wMar", "meanh(cm)", "Dens(db/m2)")
for(ttcolname in c("wOkt", "wDec", "wFeb", "wMar"))
    data.raw[, ttcolname] <- round(as.numeric(data.raw[, ttcolname]),3)
data.raw[, "InOut"] <- factor(data.raw[, "InOut"])
t.test(wOkt ~ InOut, data.raw)
boxplot(wOkt ~ InOut, data.raw)
t.test(wMar ~ InOut, data.raw)
