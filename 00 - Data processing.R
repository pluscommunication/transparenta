# Installing and loading libraries
if(!require(foreign)) install.packages("foreign")

library(foreing)

# Importing data
ds <- foreign::read.spss("Lucrare-cu-isabela.sav", use.value.labels = T, to.data.frame = T); names(ds)
colnames(ds) <- c("regiune", "judet", "localitatea", "rang",
                  paste("i", 1:13, sep = "")); head(ds)

# Recoding factor
ds$rang <- factor(ds$rang, labels = c("Municipality", "City", "Village"))

# Saving data
save(ds, file = "Date.RData")

