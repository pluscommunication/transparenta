# Installing and loading libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(scale)) install.packages("scale")

library(ggplot2); library(scale)

# Loading dataset ####
load("Date.RData")

# Analyzing Region vs Item 1 ####
chi.R.I1 <- chisq.test(ds$i1, ds$rang,  simulate.p.value = T); chi.R.I1
F.R.I1 <- fisher.test(ds$rang, ds$i1, simulate.p.value = T); F.R.I1
tabCont <- table(ds$rang, ds$i1)
mosaicplot(tabCont, shade = TRUE,
           main = "Contingenta: - Locality rank vs. Normative documents")
ggplot(data = ds, mapping = aes(x = rang, fill = i1)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Normative \ndocuments")


