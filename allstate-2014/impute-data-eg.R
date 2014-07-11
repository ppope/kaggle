######  IMPUTE DATA ######

install.packages("Amelia")

all.testing.data <- read.csv("test_v2.csv")


######  EXAMPLE ######

#Load Amelia package and freetrade dataset
require(Amelia)
data(freetrade)

# View summary of freetrade dataset 
# and a summary of a linear model prediciting average tariff rates (tariff) from Polity IV score (polity), population (pop), 
# gross domestic product per capita (gdp.pc), gross international reserves (intresmi), and country (country).
# this model uses listwise deletion
summary(freetrade)
summary(lm(tariff ~ polity + pop + gdp.pc + year + country, data = freetrade))


a.out <- amelia(freetrade, m=5, ts = "year", cs = "country")
a.out

hist(a.out$imputations[[3]]$tariff, col="grey", border="white")
