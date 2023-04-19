

ER_bilateral <- read.csv("/home/milo/Documents/egap/NIDHI/PPP/data/ER_bilateral.csv")
PPP_bilateral <- read.csv("/home/milo/Documents/egap/NIDHI/PPP/data/PPP_bilateral.csv")
WB_inflation <- read.csv("/home/milo/Documents/egap/NIDHI/PPP/data/INFLATION_wb.csv")

countries_lac <- c('ARG','BHS','BRB','BLZ','BOL','BRA','CHL','COL','CRI','DOM','ECU','SLV','GTM','GUY','HTI','HND','JAM','MEX','NIC','PAN','PRY','PER','SUR','TTO','URY')


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

    if (target_region == 'LAC_AVERAGE' & original_region!='LAC_AVERAGE' ){
        #print(1)
        return(mean(ssp_cost_converter(original_amount,original_region,original_year,"EACH_LAC_COUNTRY", target_year)$value))

    }else if(target_region == 'EACH_LAC_COUNTRY' & original_region != 'EACH_LAC_COUNTRY' & original_region != 'LAC_AVERAGE' ){
        #print(2)

        lac_costs_vector <- c()

        for(lac_country in countries_lac){
            cost_value_country <- ssp_cost_converter(original_amount,original_region,original_year,lac_country,target_year)
            lac_costs_vector <- append(lac_costs_vector, cost_value_country)
        }

        cost_df <- data.frame(
            "region" = countries_lac, 
            "value" = lac_costs_vector
            )
        
        return(cost_df)
    }else if(original_region == 'EACH_LAC_COUNTRY' & target_region != 'EACH_LAC_COUNTRY'){
        #print(3)
        lac_costs_vector <- c()
        
        for(i in c(1:NROW(original_amount))){
            individual_cost <- ssp_cost_converter(lac_average_cost[i,]$value, lac_average_cost[i,]$region, original_year, target_region, target_year)
            lac_costs_vector <- append(lac_costs_vector, individual_cost)
        }

        return(mean(lac_costs_vector))
    }else if(original_region == 'EACH_LAC_COUNTRY' & target_region == 'EACH_LAC_COUNTRY' & (original_year < target_year)){
        #print(4)
        all_dataframes <- data.frame()

        for(i in c(1:NROW(original_amount))){
            print(original_amount[i,]$region)
            lac_costs_vector <- c()
            original_region_name <- c()
            target_region_name <- c()

            for(lac_country in countries_lac){
                individual_cost <- ssp_cost_converter(original_amount[i,]$value, original_amount[i,]$region, original_year, lac_country, target_year)
                lac_costs_vector <- append(lac_costs_vector, individual_cost)  
                original_region_name <- append(original_region_name, original_amount[i,]$region)
                target_region_name <- append(target_region_name, lac_country)
            }
            cost_df <- data.frame(
                "original_region" = original_region_name,
                "target_region" = target_region_name, 
                "value" = lac_costs_vector
                )

            all_dataframes <- rbind.data.frame(all_dataframes, cost_df)
        }

        return(all_dataframes)
    }else if(target_region == 'EACH_LAC_COUNTRY' &  original_region== 'EACH_LAC_COUNTRY' & (original_year > target_year)){
        #print(5)
        lac_costs_vector <- c()
        original_region_name <- c()
        
        or_reg_flag <- ""

        for(i in c(1:NROW(original_amount))){
            individual_cost <- ssp_cost_converter(original_amount[i,]$value, original_amount[i,]$target_region, original_year, original_amount[i,]$original_region, target_year)
            
            lac_costs_vector <- append(lac_costs_vector, individual_cost)  
            original_region_name <- append(original_region_name, original_amount[i,]$original_region)

            if(or_reg_flag!=original_amount[i,]$original_region){
                or_reg_flag <- original_amount[i,]$original_region
                print(or_reg_flag)
            }

            #print(paste(original_amount[i,]$original_region, original_amount[i,]$target_region, individual_cost))

        }

        cost_df <- data.frame(
            "original_region" = original_region_name,
            "value" = lac_costs_vector
            )


        return(aggregate(value ~ original_region, cost_df, mean))
    }else if(target_region == 'EACH_LAC_COUNTRY' & original_region == 'LAC_AVERAGE'){
        #print(6)
        #print(target_region)
        #print(original_amount)
        avg_cost_df <- data.frame(
            "region" = countries_lac,
            "value" = replicate(length(countries_lac),original_amount)
        )
        #print(avg_cost_df)
        all_lac_average_cost <- ssp_cost_converter(avg_cost_df, "EACH_LAC_COUNTRY", original_year, "EACH_LAC_COUNTRY", target_year)

        return(all_lac_average_cost)
    }else if(target_region == 'LAC_AVERAGE' & original_region == 'LAC_AVERAGE' & (original_year < target_year) ){
        #print(7)
        all_lac_average_cost <- ssp_cost_converter(original_amount, "LAC_AVERAGE", original_year, "EACH_LAC_COUNTRY", target_year)
        return(mean(all_lac_average_cost$value))

    }else if(target_region == 'LAC_AVERAGE' &  original_region== 'EACH_LAC_COUNTRY' & (original_year > target_year)){
        #print(8)
        reciprocal_all_lac_average_cost <- ssp_cost_converter(original_amount, "EACH_LAC_COUNTRY", original_year, "EACH_LAC_COUNTRY", target_year)
        return(mean(reciprocal_all_lac_average_cost$value) )

    }else{

        if (original_year <= target_year){
            updated_amount <- update_amount(original_amount,original_region,original_year,target_region,target_year)
            
            ppp_bilateral_value <- subset(PPP_bilateral, original == original_region & target == target_region & Year == target_year)$PPP
            er_bilateral_value <- subset(ER_bilateral, original == target_region & target == original_region & Year == target_year)$EXR

            amount_foreign_currency <- updated_amount/ppp_bilateral_value

            amount_dolars <- amount_foreign_currency / er_bilateral_value

            return(amount_dolars)
        }else{
            ppp_bilateral_value <- subset(PPP_bilateral, original == target_region & target == original_region & Year == original_year)$PPP
            er_bilateral_value <- subset(ER_bilateral, original == target_region & target == original_region & Year == original_year)$EXR

            amount_foreign_currency <- original_amount/er_bilateral_value
            amount_dolars <- amount_foreign_currency * ppp_bilateral_value

            amount_dolars <- update_amount(amount_dolars,original_region,original_year,target_region,target_year)

            return(amount_dolars)

        }
    }
}
        