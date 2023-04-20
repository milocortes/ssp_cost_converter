# SSPCostConverter
SSPCostConverter R package.

# Installation

Run in R: 

```
# Install necessary packages
list.of.packages <- c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)

install_github("milocortes/ssp_cost_converter")

library(SSPCostConverter)

packageVersion("SSPCostConverter")


### Test function

# Individual query
ssp_cost_converter(5.15, "USA", 2020, "EACH_LAC_COUNTRY", 2020)
ssp_cost_converter(5.15, "USA", 2020, "BRA", 2020)


# Dataframe query

cost_df<-as.data.frame(matrix(c(5.15, "USA", 2017, "BRA", 2020, 67.80, "USA", 2018, "ARG", 2020, 34.60, "FRA", 2016, "LAC_AVERAGE", 2020, 34.60, "DEU", 2020, "MEX", 2020, 5, "USA", 2015, "BRA", 2019, 3.11696503966219, "BRA", 2019, "USA", 2015, 5, "USA", 2015, "LAC_AVERAGE", 2019, 5, "USA", 2015, "GLOBAL_AVERAGE", 2019, 50, "LAC_AVERAGE", 2010, "GLOBAL_AVERAGE", 2019, 50, "GLOBAL_AVERAGE", 2010, "GLOBAL_AVERAGE", 2019, 50, "GLOBAL_AVERAGE", 2010, "LAC_AVERAGE", 2019, 50,"LAC_AVERAGE", 2010, "LAC_AVERAGE", 2019), 12,5, byrow = TRUE))
colnames(cost_df)<-c("original_amount","original_region","original_year","target_region", "target_year")

cost_df
ssp_cost_converter_df(cost_df)

```