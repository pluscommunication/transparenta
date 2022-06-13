# Installing and loading libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(memisc)) install.packages("memisc")

library(ggplot2); library(dplyr)

# Loading dataset ####
load("Date.RData"); names(ds)

# Analyzing Region ####
## Item 1 
chi.R.I1 <- chisq.test(ds$i1, ds$rang); chi.R.I1
mosaicplot(chi.R.I1$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Normative documents",
           xlab = "Normative documents", ylab = "Rank of locality")
R.I.1.prop <- round(prop.table(chi.R.I1$observed) * 100, 2); R.I.1.prop
ggplot(data = ds, mapping = aes(x = rang, fill = i1)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Normative \ndocuments")
