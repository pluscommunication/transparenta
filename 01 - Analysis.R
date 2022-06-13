# Installing and loading libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(memisc)) install.packages("memisc")

library(ggplot2)

# Loading dataset ####
load("Date.RData"); names(ds)

# Analyzing Region ####
## Item 1 - Actele normative care reglementează organizarea și funcționarea autorității sau instituției publice
chi.R.I1 <- chisq.test(ds$i1, ds$rang); chi.R.I1
mosaicplot(chi.R.I1$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Normative documents",
           xlab = "Normative documents", ylab = "Rank of locality")
R.I.1.prop <- round(prop.table(chi.R.I1$observed) * 100, 2); R.I.1.prop
R.I.1.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i1)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Normative \ndocuments"); R.I.1.plot

## Item 2 - Structura organizatorică, atribuțiile departamentelor, programul de funcționare, programul de audiențe al autorității sau instituției publice
chi.R.I2 <- chisq.test(ds$i2, ds$rang); chi.R.I2
mosaicplot(chi.R.I2$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Oganizatoric strucure",
           xlab = "Oganizatoric strucure", ylab = "Rank of locality")
R.I.2.prop <- round(prop.table(chi.R.I2$observed) * 100, 2); R.I.2.prop
R.I.2.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i2)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Oganizatoric \nstructure"); R.I.2.plot

## Item 3 - Numele și prenumele persoanelor din conducerea autorității sau a instituției publice și ale funcționarului responsabil cu difuzarea informațiilor publice
chi.R.I3 <- chisq.test(ds$i3, ds$rang); chi.R.I3
mosaicplot(chi.R.I3$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Management",
           xlab = "Management", ylab = "Rank of locality")
R.I.3.prop <- round(prop.table(chi.R.I3$observed) * 100, 2); R.I.3.prop
R.I.3.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i3)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Management"); R.I.3.plot

## Item 4 - Coordonatele de contact ale autorității sau instituției publice, respectiv: denumirea, sediul, numerele de telefon, fax, adresa de e-mail și adresa paginii de Internet
chi.R.I4 <- chisq.test(ds$i4, ds$rang); chi.R.I4
mosaicplot(chi.R.I4$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Contact information",
           xlab = "Contact information", ylab = "Rank of locality")
R.I.4.prop <- round(prop.table(chi.R.I4$observed) * 100, 2); R.I.4.prop
R.I.4.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i4)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Contact \ninformation"); R.I.4.plot

## Item 5 - Sursele financiare, bugetul și bilanțul contabil
chi.R.I5 <- chisq.test(ds$i5, ds$rang); chi.R.I5
mosaicplot(chi.R.I5$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Financial information",
           xlab = "Financial information", ylab = "Rank of locality")
R.I.5.prop <- round(prop.table(chi.R.I5$observed) * 100, 2); R.I.5.prop
R.I.5.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i5)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Financial \ninformation"); R.I.5.plot

## Item 6 - Programele și strategiile proprii
chi.R.I6 <- chisq.test(ds$i6, ds$rang); chi.R.I6
mosaicplot(chi.R.I6$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Programs and strategies",
           xlab = "Programs and strategies", ylab = "Rank of locality")
R.I.6.prop <- round(prop.table(chi.R.I6$observed) * 100, 2); R.I.6.prop
R.I.6.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i6)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Programs and \nstrategies"); R.I.6.plot

## Item 7 - Lista cuprinzând documentele de interes public
chi.R.I7 <- chisq.test(ds$i7, ds$rang); chi.R.I7
mosaicplot(chi.R.I7$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Public documents list",
           xlab = "Public documents list", ylab = "Rank of locality")
R.I.7.prop <- round(prop.table(chi.R.I7$observed) * 100, 2); R.I.7.prop
R.I.7.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i7)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Public \ndocuments list"); R.I.7.plot

## Item 8 - Lista cuprinzând categoriile de documente produse și/sau gestionate, potrivit legii
chi.R.I8 <- chisq.test(ds$i8, ds$rang); chi.R.I8
mosaicplot(chi.R.I8$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Legal documents list",
           xlab = "Legal documents list", ylab = "Rank of locality")
R.I.8.prop <- round(prop.table(chi.R.I8$observed) * 100, 2); R.I.8.prop
R.I.8.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i8)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Legal \ndocuments list"); R.I.8.plot

## Item 9 - Modalitățile de contestare a deciziei autorității sau a instituției publice în situația în care persoana se considera vătămată în privința dreptului de acces la informațiile de interes public solicitate
chi.R.I9 <- chisq.test(ds$i9, ds$rang); chi.R.I9
mosaicplot(chi.R.I9$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Ways of contesting",
           xlab = "Ways of contesting", ylab = "Rank of locality")
R.I.9.prop <- round(prop.table(chi.R.I9$observed) * 100, 2); R.I.9.prop
R.I.9.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i9)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Ways of \ncontesting"); R.I.9.plot

## Item 10 - Declarații de avere și interese
chi.R.I10 <- chisq.test(ds$i10, ds$rang); chi.R.I10
mosaicplot(chi.R.I10$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Wealth statements",
           xlab = "Wealth statements", ylab = "Rank of locality")
R.I.10.prop <- round(prop.table(chi.R.I10$observed) * 100, 2); R.I.10.prop
R.I.10.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i10)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Wealth \nstatements"); R.I.10.plot

## Item 11 - Hotărâri de Consiliu
chi.R.I11 <- chisq.test(ds$i11, ds$rang); chi.R.I11
mosaicplot(chi.R.I11$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Council decisions",
           xlab = "Council decisions", ylab = "Rank of locality")
R.I.11.prop <- round(prop.table(chi.R.I11$observed) * 100, 2); R.I.11.prop
R.I.11.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i11)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Council \ndecisions"); R.I.11.plot

## Item 12 - Contracte de achiziții publice
chi.R.I12 <- chisq.test(ds$i12, ds$rang); chi.R.I12
mosaicplot(chi.R.I12$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Public procurement",
           xlab = "Public procurement", ylab = "Rank of locality")
R.I.12.prop <- round(prop.table(chi.R.I12$observed) * 100, 2); R.I.12.prop
R.I.12.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i12)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Public \nprocurement"); R.I.12.plot

## Item 13 - Anunțuri de interes public
chi.R.I13 <- chisq.test(ds$i13, ds$rang); chi.R.I13
mosaicplot(chi.R.I13$observed, shade = TRUE,
           main = "Contingency: - Locality rank vs. Ads of public interest",
           xlab = "Ads of public interest", ylab = "Rank of locality")
R.I.13.prop <- round(prop.table(chi.R.I13$observed) * 100, 2); R.I.13.prop
R.I.13.plot <- ggplot(data = ds, mapping = aes(x = rang, fill = i13)) + 
  geom_bar(position = "dodge") + xlab("Locality rank") + 
  ylab("Frequency") + scale_fill_discrete(name = "Ads of \npublic interest"); R.I.13.plot


# Analyzing Region ####
## Item 1 - Actele normative care reglementează organizarea și funcționarea autorității sau instituției publice





