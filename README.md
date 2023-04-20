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
```