print("*********************************************")
print("                                             ")
print("          ssp_cost_converter                 ")
print("                                             ")
print("          Version : 1.0.0                    ")
print("*********************************************")

# Install necessary packages
list.of.packages <- c("ncdf4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the ncdf4 package
library(ncdf4)
library(tools)

# Open a netCDF file
my_ncdf <- nc_open('data/ppp_er_data.h5')

# Open inflation data
WB_inflation <- read.csv("data/INFLATION_wb.csv")

row.names.countries <-   c('ALB','DZA','AGO','ATG','ARG','ARM','ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN','BMU','BTN','BOL','BIH','BWA','BRA','BRN','BGR','BFA','BDI','CPV','KHM','CMR','CAN','CAF','TCD','CHL','CHN','COL','COM','COD','COG','CRI','CIV','HRV','CYP','CZE','DNK','DMA','DOM','ECU','EGY','SLV','GNQ','EST','SWZ','ETH','FJI','FIN','FRA','GAB','GMB','GEO','DEU','GHA','GRC','GRD','GTM','GNB','GUY','HTI','HND','HKG','HUN','ISL','IND','IDN','IRN','IRQ','IRL','ISR','ITA','JAM','JPN','JOR','KAZ','KEN','KIR','KOR','KGZ','LAO','LVA','LBN','LSO','LBY','LTU','LUX','MAC','MDG','MYS','MDV','MLI','MLT','MRT','MUS','MEX','FSM','MDA','MNG','MNE','MAR','MOZ','NAM','NRU','NPL','NLD','NZL','NIC','NER','MKD','NOR','OMN','PAK','PLW','PAN','PNG','PRY','PER','PHL','POL','PRT','QAT','ROU','RUS','RWA','WSM','SAU','SEN','SRB','SYC','SLE','SGP','SVK','SVN','SLB','ZAF','ESP','LKA','KNA','LCA','VCT','SUR','SWE','CHE','TJK','TZA','THA','TLS','TGO','TON','TTO','TUN','TUR','UGA','UKR','ARE','GBR','USA','URY','VUT','VNM','ZMB')
column.names.countries <- row.names.countries
matrix.names.countries <- as.character(c(2005:2021))

er_array  <- array(ncvar_get(my_ncdf, attributes(my_ncdf$var)$names[1]), dim = c(170, 170, 17),
                  dimnames = list(row.names.countries, column.names.countries,matrix.names.countries))
		
	
ppp_array <- array(ncvar_get(my_ncdf, attributes(my_ncdf$var)$names[2]), dim = c(170, 170, 17),
                  dimnames = list(row.names.countries, column.names.countries,matrix.names.countries) )


countries_lac <- c('ARG','BHS','BRB','BLZ','BOL','BRA','CHL','COL','CRI','DOM','ECU','SLV','GTM','GUY','HTI','HND','JAM','MEX','NIC','PAN','PRY','PER','SUR','TTO','URY')
all_world_countries <- c('ALB','DZA','AGO','ATG','ARG','ARM','ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN','BMU','BTN','BOL','BIH','BWA','BRA','BRN','BGR','BFA','BDI','CPV','KHM','CMR','CAN','CAF','TCD','CHL','CHN','COL','COM','COD','COG','CRI','CIV','HRV','CYP','CZE','DNK','DMA','DOM','ECU','EGY','SLV','GNQ','EST','SWZ','ETH','FJI','FIN','FRA','GAB','GMB','GEO','DEU','GHA','GRC','GRD','GTM','GNB','GUY','HTI','HND','HKG','HUN','ISL','IND','IDN','IRN','IRQ','IRL','ISR','ITA','JAM','JPN','JOR','KAZ','KEN','KIR','KOR','KGZ','LAO','LVA','LBN','LSO','LBY','LTU','LUX','MAC','MDG','MYS','MDV','MLI','MLT','MRT','MUS','MEX','FSM','MDA','MNG','MNE','MAR','MOZ','NAM','NRU','NPL','NLD','NZL','NIC','NER','MKD','NOR','OMN','PAK','PLW','PAN','PNG','PRY','PER','PHL','POL','PRT','QAT','ROU','RUS','RWA','WSM','SAU','SEN','SRB','SYC','SLE','SGP','SVK','SVN','SLB','ZAF','ESP','LKA','KNA','LCA','VCT','SUR','SWE','CHE','TJK','TZA','THA','TLS','TGO','TON','TTO','TUN','TUR','UGA','UKR','ARE','GBR','USA','URY','VUT','VNM','ZMB')

get_countries_region_named_list = list(EACH_LAC_COUNTRY=countries_lac, 
                            EACH_GLOBAL_COUNTRY=all_world_countries) 

refactor_target_region <- function(target_region){
    if(nchar(target_region)==3){
        return(target_region)
    }else if(grepl("AVERAGE",target_region)){
        if(grepl("LAC",target_region)){
            return("EACH_LAC_COUNTRY")
        }else if(grepl("GLOBAL",target_region)){
            return("EACH_GLOBAL_COUNTRY")
        }
    }else{
        if(grepl("LAC",target_region)){
            return("EACH_LAC_COUNTRY")
        }else if(grepl("GLOBAL",target_region)){
            return("EACH_GLOBAL_COUNTRY")
        }
    }
}


get_countries_region <- function(target_region, hashmap_regions){
    if(nchar(target_region)==3){
        return(target_region)
    }else if(grepl("AVERAGE",target_region)){
        if(grepl("LAC",target_region)){
            return(hashmap_regions[["EACH_LAC_COUNTRY"]])
        }else if(grepl("GLOBAL",target_region)){
            return(hashmap_regions[["EACH_GLOBAL_COUNTRY"]])
        }
    }else{
        if(grepl("LAC",target_region)){
            return(hashmap_regions[["EACH_LAC_COUNTRY"]])
        }else if(grepl("GLOBAL",target_region)){
            return(hashmap_regions[["EACH_GLOBAL_COUNTRY"]])
        }
    }
}

compound_rate <- function(r, actualization){

    if(actualization == "Futuro"){
        return (1+r)
    }else if(actualization == "Pasado"){
        return (1/(1+r))
    }
}

apply_inflation <- function(amount, inflation, actualization){
    if(actualization == "Futuro"){
        return(amount * prod(compound_rate(inflation, actualization)))
    }else if(actualization == "Pasado"){
        return(amount * prod(compound_rate(inflation, actualization)))
    }
}

update_amount <- function(original_amount,original_region,original_year,target_region,target_year){
    if (original_year < target_year){
        inflation <- subset(WB_inflation, economy == original_region & (time >= (original_year +1) & time <= target_year))$FP.CPI.TOTL.ZG/100
        updated_amount <- apply_inflation(original_amount, inflation, "Futuro")

        return(updated_amount)

    }else if(original_year > target_year){

        inflation <- subset(WB_inflation, economy == target_region & (time <= original_year  & time >=(target_year +1) ))$FP.CPI.TOTL.ZG/100
        updated_amount <- apply_inflation(original_amount, inflation, "Pasado")
        
        return(updated_amount)

    }else{
        return(original_amount)
    }
}

ssp_cost_converter <- function(original_amount,original_region,original_year,target_region,target_year){

    tmp_target_region <- target_region
    target_region <- refactor_target_region(target_region)

    countries_in_region <- get_countries_region(target_region, get_countries_region_named_list)
    countries_in_region_original_country <- get_countries_region(refactor_target_region(original_region), get_countries_region_named_list)


    if(grepl("AVERAGE",original_region)){
        original_amount <- data.frame(
            "region" = countries_in_region_original_country, 
            "value" = replicate(length(countries_in_region_original_country),original_amount)
            )
        
    }
    #print(target_region)
    if(nchar(original_region) == 3 & nchar(target_region) == 3){
        #print(1)
        if (original_year <= target_year){
            updated_amount <- update_amount(original_amount,original_region,original_year,target_region,target_year)
            
            ppp_bilateral_value <- ppp_array[original_region, target_region, as.character(target_year)]
            er_bilateral_value <- er_array[target_region, original_region, as.character(target_year)]

            amount_foreign_currency <- updated_amount/ppp_bilateral_value

            amount_dolars <- amount_foreign_currency / er_bilateral_value

            general_output <- amount_dolars
        }else{

            ppp_bilateral_value <- ppp_array[target_region, original_region, as.character(original_year)]
            er_bilateral_value <- er_array[target_region, original_region, as.character(original_year)]

            amount_foreign_currency <- original_amount/er_bilateral_value
            amount_dolars <- amount_foreign_currency * ppp_bilateral_value

            amount_dolars <- update_amount(amount_dolars,original_region,original_year,target_region,target_year)

            general_output <- amount_dolars

        }
    }else if(nchar(original_region) == 3 & nchar(target_region) > 3){
        #print(2)
        countries_costs_vector <- c()

        for(country in countries_in_region){
            cost_value_country <- ssp_cost_converter(original_amount,original_region,original_year,country,target_year)
            countries_costs_vector <- append(countries_costs_vector, cost_value_country)
        }
        cost_df <- data.frame(
            "region" = countries_in_region, 
            "value" = countries_costs_vector
            )
        
        general_output <- cost_df

    }else if(nchar(original_region) != 3 & nchar(target_region) == 3){
        #print(3)
        countries_costs_vector <- c()
        
        for(i in c(1:NROW(original_amount))){
            individual_cost <- ssp_cost_converter(original_amount[i,]$value, original_amount[i,]$region, original_year, target_region, target_year)
            countries_costs_vector <- append(countries_costs_vector, individual_cost)
        }

        general_output <- mean(countries_costs_vector)

    }else if(nchar(original_region) != 3 & nchar(target_region) != 3){
        #print(4)

        #print(countries_in_region)
        if (original_year <= target_year){
            all_dataframes <- data.frame()

            for(i in c(1:NROW(original_amount))){
                countries_costs_vector <- c()
                original_region_name <- c()
                target_region_name <- c()

                for(country in countries_in_region){
                    individual_cost <- ssp_cost_converter(original_amount[i,]$value, original_amount[i,]$region, original_year, country, target_year)
                    countries_costs_vector <- append(countries_costs_vector, individual_cost)  
                    original_region_name <- append(original_region_name, original_amount[i,]$region)
                    target_region_name <- append(target_region_name, country)
                }
                cost_df <- data.frame(
                    "original_region" = original_region_name,
                    "target_region" = target_region_name, 
                    "value" = countries_costs_vector
                    )

                all_dataframes <- rbind.data.frame(all_dataframes, cost_df)
            }

            general_output <- all_dataframes
            
        }else{
            countries_costs_vector <- c()
            original_region_name <- c()
            
            or_reg_flag <- ""

            for(i in c(1:NROW(original_amount))){
                individual_cost <- ssp_cost_converter(original_amount[i,]$value, original_amount[i,]$target_region, original_year, original_amount[i,]$original_region, target_year)
                
                countries_costs_vector <- append(countries_costs_vector, individual_cost)  
                original_region_name <- append(original_region_name, original_amount[i,]$original_region)

                if(or_reg_flag!=original_amount[i,]$original_region){
                    or_reg_flag <- original_amount[i,]$original_region
                    #print(or_reg_flag)
                }

            }

            cost_df <- data.frame(
                "original_region" = original_region_name,
                "value" = countries_costs_vector
                )


            general_output <- aggregate(value ~ original_region, cost_df, mean)
        }
    }

    if(grepl("AVERAGE",tmp_target_region)){
        general_output <- mean(general_output$value)
    }

    return(general_output)
}



ssp_cost_converter_df <- function(df_costs){
    
    
    df_costs$cost_converted <- apply(cost_df,1, function (x) {ssp_cost_converter(as.numeric(x['original_amount']),
                                                    as.character(x['original_region']),
                                                    as.numeric(x['original_year']),
                                                    as.character(x['target_region']),
                                                    as.numeric(x['target_year']))})
                                                    
    

    general_output <- df_costs

    return(general_output)
}