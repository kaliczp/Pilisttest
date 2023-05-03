## Látszik-e az xlsx?
dir()
## Beolvasás
library(readxl)
data.raw <- as.data.frame(read_excel("F_L_2023_complete_soil_dataset_T_probara.xlsx"))
names(data.raw) <- c("ID", "InOut", "wOkt", "wDec", "wFeb", "wMar", "meanh(cm)", "Dens(db/m2)")
data.raw$wOkt <- round(as.numeric(data.raw$wOkt),3)
