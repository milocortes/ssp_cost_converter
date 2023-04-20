library(tools)

# source path
source_path <- file.path(file_path_as_absolute(".."),"src")

source(file.path(source_path, "cost_function_bilateral.R"))


##### TEST

## COUNTRY_1 ----> COUNTRY_2
ssp_cost_converter(5, "USA", 2015, "BRA", 2019)

## COUNTRY_2 ----> COUNTRY_1
ssp_cost_converter(3.11696503966219, "BRA", 2019, "USA", 2015)


## COUNTRY_1 ----> EACH_LAC_COUNTRY
lac_average_cost <- ssp_cost_converter(5, "USA", 2015, "EACH_LAC_COUNTRY", 2019)
lac_average_cost
mean(lac_average_cost$value)

## COUNTRY_1 ----> LAC_AVERAGE
ssp_cost_converter(5, "USA", 2015, "LAC_AVERAGE", 2019)

## EACH_LAC_COUNTRY ----> COUNTRY_1
ssp_cost_converter(lac_average_cost, "EACH_LAC_COUNTRY", 2019, "USA", 2015)



## COUNTRY_1 ----> EACH_GLOBAL_COUNTRY
global_average_cost <- ssp_cost_converter(5, "USA", 2015, "EACH_GLOBAL_COUNTRY", 2019)
global_average_cost
mean(global_average_cost$value)

## COUNTRY_1 ----> LAC_AVERAGE
ssp_cost_converter(5, "USA", 2015, "GLOBAL_AVERAGE", 2019)

## EACH_GLOBAL_COUNTRY ----> COUNTRY_1
ssp_cost_converter(global_average_cost, "EACH_GLOBAL_COUNTRY", 2019, "USA", 2015)


## LAC_AVERAGE ----> GLOBAL_AVERAGE
AVG_VALUE <- 50
global_avg_value <- ssp_cost_converter(AVG_VALUE, "LAC_AVERAGE", 2010, "GLOBAL_AVERAGE", 2019)
global_avg_value

## GLOBAL_AVERAGE ----> GLOBAL_AVERAGE
global_avg_value <- ssp_cost_converter(AVG_VALUE, "GLOBAL_AVERAGE", 2010, "GLOBAL_AVERAGE", 2019)
global_avg_value

## GLOBAL_AVERAGE ----> LAC_AVERAGE
lac_avg_value <- ssp_cost_converter(AVG_VALUE, "GLOBAL_AVERAGE", 2010, "LAC_AVERAGE", 2019)
lac_avg_value

## LAC_AVERAGE ----> LAC_AVERAGE
lac_avg_value <- ssp_cost_converter(AVG_VALUE, "LAC_AVERAGE", 2010, "LAC_AVERAGE", 2019)
lac_avg_value

#### TEST RECIPROCAL

## GLOBAL_AVERAGE ---> GLOBAL_AVERAGE
global_avg_value <- ssp_cost_converter(AVG_VALUE, "GLOBAL_AVERAGE", 2010, "GLOBAL_AVERAGE", 2019)
global_avg_value


all_global_average_cost <- ssp_cost_converter(AVG_VALUE, "GLOBAL_AVERAGE", 2010, "EACH_GLOBAL_COUNTRY", 2019)
reciprocal_global_avg_value <- ssp_cost_converter(all_global_average_cost, "EACH_GLOBAL_COUNTRY", 2019, "GLOBAL_AVERAGE", 2010)
reciprocal_global_avg_value



## GLOBAL_AVERAGE ---> LAC_AVERAGE
lac_avg_value <- ssp_cost_converter(AVG_VALUE, "GLOBAL_AVERAGE", 2010, "LAC_AVERAGE", 2019)
lac_avg_value


all_lac_average_cost <- ssp_cost_converter(AVG_VALUE, "GLOBAL_AVERAGE", 2010, "EACH_LAC_COUNTRY", 2019)
reciprocal_global_avg_value <- ssp_cost_converter(all_lac_average_cost, "EACH_LAC_COUNTRY", 2019, "GLOBAL_AVERAGE", 2010)
reciprocal_global_avg_value

