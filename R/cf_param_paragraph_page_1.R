cf_param_paragraph_page_1=function(ctry_code,ctry_name,
                                    dt_housing_structure_path="data/OECD_HHP2020_CF_data_structure.xlsx"){
  
  #' @title generate the paragraph of text in page 1 of the country fiche
  #' @description generate the paragraph of text in page 1 of the country fiche
  #' @param ctry_code the iso3 code of the country of interest
  #' @param ctry_name the country name of the country of interest
  #' @param dt_housing_structure_path path to the dataset of housing structure
  #' @return block of text on housing structure
  #' @author Manuel Betin, Federica Depace
  #' @export
  #'
  
  struc_cross=rio::import(dt_housing_structure_path,sheet="structural") #change path accordingly
  house_price_data=rio::import(dt_housing_structure_path,sheet="real_house_price") #change path accordingly
  rent_price_data=rio::import(dt_housing_structure_path,sheet="rent_price") #change path accordingly
  house_tenure_data=rio::import(dt_housing_structure_path,sheet="housing_tenure") #change path accordingly
  
  if (ctry_code=="USA"){
    country_name<-"the United States"
  } else if (ctry_code!="USA"){
    country_name<-ctry_name
  }
  
  # 1 home-ownership stats: min max and below above or close t average
  
  # by category: summarize and add 
  avg_ht<- house_tenure_data  %>% group_by(Tenure_cat)  %>% 
    summarize(Tenure_share = mean(Tenure_share, na.rm=T)) %>% 
    mutate(ISO3_code="OECD")%>%
    select(ISO3_code, Tenure_cat, Tenure_share)
  # add OECD to house_tenure_data 
  house_tenure_data<- rbind(house_tenure_data, avg_ht)
  
  
  stats_ht<- house_tenure_data  %>% 
    filter( Tenure_cat == "Own_outright"| Tenure_cat == "Owner_with_mortgage") %>% 
    group_by(ISO3_code)  %>% 
    summarize(tot_own = sum(Tenure_share, na.rm=T))
  
  stats_ht<- stats_ht %>% mutate(country_name=countrycode(ISO3_code,origin="iso3c",destination="country.name"))
  
  stats_ht<-stats_ht %>% mutate(country_name=ifelse(ISO3_code=="OECD", "OECD", country_name))
  
  min_ht<-min(stats_ht$tot_own, na.rm=T)
  max_ht<-max(stats_ht$tot_own, na.rm=T)
  
  max_ctry_ht<-stats_ht$country_name[which.max(stats_ht$tot_own)]
  min_ctry_ht<-stats_ht$country_name[which.min(stats_ht$tot_own)]
  
  avg_OECD=mean(stats_ht$tot_own, na.rm=T)
  myctry_hh=stats_ht$tot_own[which(stats_ht$ISO3_code==params$ctry_code)]
  
  what=function(avg_OECD, myctry_hh ){
    if (myctry_hh<=avg_OECD-5){
      my_adv="lower than"
    } else if (myctry_hh>=avg_OECD+5){
      my_adv="higher than"
    } else if (myctry_hh>avg_OECD-5&myctry_hh<avg_OECD+5){
      my_adv="close to"
    }
    return(my_adv)
  }
  
  
  #################################################################################
  # 2 house prices 
  # show latest year in bar: 2019
  # index to minimum year I can use for all: 2010
  # show in a symbol 1995 [for countries for which we have it]
  
  
  stats_hp<- house_price_data  %>% mutate(year= year(period))  %>% 
    filter(year>=1970)
  stats_hp<- stats_hp  %>% mutate(month= month(period)) 
  stats_hp<- stats_hp  %>% filter(month==1)
  stats_hp_minmaxy=stats_hp %>% group_by(ISO3_code)  %>% 
    arrange(desc(year), .by_group = TRUE) %>% 
    summarise(max_y=first(year), min_y=last(year))
  stats_hp<-merge(stats_hp,stats_hp_minmaxy, by="ISO3_code")
  #nas<-c(unique(stats_hp$ISO3_code[stats_hp$min_y>2010])) all start from min 2010
  #unique(stats_hp$min_y[stats_hp$min_y>2005])
  stats_hp<- stats_hp %>% group_by(ISO3_code) %>% 
    mutate(rhp_2010= ifelse(year==2010, rhp,NA ))%>% 
    fill(rhp_2010) %>% fill(rhp_2010, .direction = "up")%>% 
    mutate(rhp_ind2010=rhp/rhp_2010*100)
  
  #keep 2019 and 1995 for who has it 
  stats_hp_bar<-stats_hp %>% mutate(min_y_show=2000)
  stats_hp_bar<-stats_hp_bar %>% group_by(ISO3_code) %>%
    filter(year==min_y_show | year==max_y) %>%
    select(ISO3_code, year, rhp, rhp_ind2010, min_y, max_y) %>% 
    filter(ISO3_code!="OECD")
  
  
  gr_hp<-stats_hp_bar %>% group_by(ISO3_code) %>% mutate(rate=((rhp-lag(rhp))/lag(rhp))*100)%>%
    ungroup()%>%
    mutate(OECD_av=mean(rate, na.rm =T))%>%
    filter(ISO3_code==params$ctry_code)%>%
    slice(tail(row_number(), 1))
  
  # latest value than 2005 --> consider 1st available
  # stats_hp<-stats_hp %>%group_by(ISO3_code)  %>% mutate(year_min= min(year))
  # stats_hp<-stats_hp %>% group_by(ISO3_code)  %>% 
  #     mutate(year_min=ifelse(ISO3_code %in% nas, min(year), 2005)) %>% 
  #     mutate(year_max=max(year)) %>%
  #   filter(year==year_min | year==year_max) %>%
  #   mutate(rate=((rhp-lag(rhp))/lag(rhp))*100) %>%
  #   filter(year==2019)%>%
  #   filter(ISO3_code==params$ctry_code)%>%slice(tail(row_number(), 1))
  
  #nas=stats_hp$ISO3_code[which(is.na(stats_hp$rate))]
  what_rhp=function(gr_hp){
    if (gr_hp$rate<0){
      my_sentence=paste0("However, in few countries real house prices remained stable or even decreased, such as in ", country_name)
    } else  if (gr_hp$rate>0 & gr_hp$rate<20){
      my_sentence=paste0("However, in few countries real house prices remained stable, such as in ", params$ctry_name)
    } else if (gr_hp$rate>=20 & gr_hp$rate<=50){
      my_sentence=paste0(" However, ", country_name, " is one of the few countries exhibiting moderate increase in real house prices")
    } else if (gr_hp$rate>50 & gr_hp$rate<100 & params$ctry_code=="ESP"){
      my_sentence=paste0( "In the years preceding the onset of the Global Financial Crisis ", country_name, " exhibited a steep increase in real house prices")
    }else if (gr_hp$rate>50 & gr_hp$rate<=100 & params$ctry_code!="ESP"){
      my_sentence=paste0( "In the period under review ", country_name, " exhibited a steep increase in real house prices")
    }else if (gr_hp$rate>100){
      my_sentence=paste0(" ", country_name, " is among the countries exhibiting the highest increase in house prices")
    }
    return(my_sentence)
  }
  #################################################################################
  # 3 rent prices 
  #################################################################################
  stats_rentp<- rent_price_data  %>% mutate(year= year(period))%>% 
    filter(year>=1970)
  stats_rentp<- stats_rentp  %>% mutate(month= month(period)) 
  stats_rentp<- stats_rentp  %>% filter(month==1)
  stats_rentp<-data.frame(stats_rentp)
  
  stats_rentp_minmaxy=stats_rentp %>% group_by(ISO3_code)  %>% 
    arrange(desc(year), .by_group = TRUE) %>% 
    summarise(max_y=first(year), min_y=last(year))
  
  stats_rentp<-merge(stats_rentp,stats_rentp_minmaxy, by="ISO3_code")
  nas<-c(unique(stats_rentp$ISO3_code[stats_rentp$min_y>2010])) #all start from min 2010
  nas1<-c(unique(stats_rentp$ISO3_code[stats_rentp$min_y>1995])) #all start from min 2010
  
  #unique(stats_hp$min_y[stats_hp$min_y>2005])
  stats_rentp<- stats_rentp %>% group_by(ISO3_code) %>% 
    mutate(rentp_2010= ifelse(year==2010, rent_prices,NA ))%>% 
    fill(rentp_2010) %>% fill(rentp_2010, .direction = "up")%>% 
    mutate(rentp_ind2010=rent_prices/rentp_2010*100)
  
  #keep 2019 and 1995 for who has it 
  stats_rentp_bar<-stats_rentp %>% mutate(min_y_show=2000)
  stats_rentp_bar<-stats_rentp_bar %>% group_by(ISO3_code) %>%
    filter(year==min_y_show | year==max_y) %>%
    select(ISO3_code, year, rent_prices, rentp_ind2010, min_y, max_y) %>% 
    filter(ISO3_code!="OECD")
  
  
  gr_rentp<-stats_rentp_bar %>% group_by(ISO3_code) %>% mutate(rate=((rentp_ind2010-lag(rentp_ind2010))/lag(rentp_ind2010))*100)%>% 
    filter(ISO3_code==params$ctry_code)%>%
    slice(tail(row_number(), 1))
  #nas=stats_hp$ISO3_code[which(is.na(stats_hp$rate))]
  
  
  ################################################################################################
  # 4 mortgage claims 
  ########################################################################################
  mortgage_data <- struc_cross %>% select("ISO3_code", "Structural_resilience_LMHQ")
  mortgage_data <- na.omit(mortgage_data  )
  avg_mortg<-mean(mortgage_data$Structural_resilience_LMHQ)
  mortgage_data_myctr<-mortgage_data%>%   filter(ISO3_code==params$ctry_code)
  mortgage_data_myctr<-mortgage_data_myctr$Structural_resilience_LMHQ
  
  what_mortg=function(avg_mortg, mortgage_data_myctr,myctry_hh, avg_OECD){
    if (mortgage_data_myctr<avg_mortg-5 & myctry_hh<avg_OECD-5){
      my_adv="modest, reflecting a relatively low homeownership rate"
    } else if (mortgage_data_myctr<avg_mortg-5 ){
      my_adv="relatively modest"
    } else if (mortgage_data_myctr>avg_mortg-5.0 & mortgage_data_myctr<avg_mortg+5){
      my_adv="close to average by international comparison"
    }else if (mortgage_data_myctr>avg_mortg+5 & myctry_hh>avg_OECD+5){
      my_adv="high, reflecting a relatively high homeownership rate"
    }else if (mortgage_data_myctr>avg_mortg+5 ){
      my_adv="relatively high by international comparison"
    }
    return(my_adv)
  }
  
  # by category: summarize and add OECD
  OECD_mrg_avg<- mortgage_data  %>% 
    summarize(Structural_resilience_LMHQ = mean(Structural_resilience_LMHQ, na.rm=T)) %>% 
    mutate(ISO3_code="OECD")%>%
    select(ISO3_code, Structural_resilience_LMHQ)
  # add OECD to house_tenure_data 
  mortgage_data<- rbind(mortgage_data, OECD_mrg_avg)
  
  
  ########################################################################################
  
  sentence1=paste0("Housing policies affect people’s well-being through a wide range of channels 
                   including access to decent shelter, environmental quality, 
                   efficient use of scarce resources, type and extent of commuting 
                   as well as its contribution to strong and resilient economic growth. 
                   This note provides a cross-country perspective on" ,country_name, "’s 
                   housing-related indicators and policy settings.")
  
  
  sentence2=paste0("Households’ tenure choices depend on demographics and/or socio-economic factors, 
                    such as population ageing, as well as policies related to housing taxation and rental regulations.
                    There are large differences in tenure structure across OECD and key partner countries: 
                    homeownership in", country_name, "is", what(avg_OECD, myctry_hh),
                   "OECD average (Figure a).")
  
  sentence3=paste0("Real house prices and rent prices have risen strongly in many countries since the 1990s with prices increasing by more than 100%
              in those countries experiencing the largest increases (Figure b, c). ",what_rhp(gr_hp), 
                   "Finally, mortgage markets play a crucial role in housing markets since housing 
              generally constitutes the household's single largest
              financial outlay. The share of households' mortgage claims in terms of GDP 
              in ",ctry_name," is ", 
                   what_mortg(avg_mortg, mortgage_data_myctr,myctry_hh, avg_OECD)," (Figure d).")
  
  return(paste0(sentence1,sentence2,sentence3,sep=" "))
  
}

