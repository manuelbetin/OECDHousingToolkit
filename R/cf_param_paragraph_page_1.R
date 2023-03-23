cf_param_paragraph_page_1=function(ctry_code,ctry_name,
                                   stats_ht,ht_avg_OECD,myctry_hh,
                                   gr_hp,
                                   avg_mortg,
                                   mortgage_data,
                                   mortgage_data_myctr, IH_GDP_stats){


  #' @title generate the paragraph of text in page 1 of the country fiche
  #' @description generate the paragraph of text in page 1 of the country fiche
  #' @param ctry_code the iso3 code of the country of interest
  #' @param ctry_name the country name of the country of interest
  #' @param stats_ht dataset of housing tenure
  #' @param gr_hp dataset on house prices
  #' @param avg_mortg dataset on mortgage claims
  #' @param mortgage_data dataset to better define
  #' @param mortgage_data_myctr dataset to better define
  #'
  #' @return block of text on housing structure
  #' @author Manuel Betin, Federica Depace
  #' @export
  #'

  # 1 home-ownership stats: min max and below above or close t average

  sentence_homewonership=function(ht_avg_OECD, myctry_hh ){
    if (is.na(myctry_hh)) {
      my_adv="We do not have data for the country in this dimension"
    }else{
      if (myctry_hh<=ht_avg_OECD-5){
        my_adv="lower than"
      } else if (myctry_hh>=ht_avg_OECD+5){
        my_adv="higher than"
      } else if (myctry_hh>ht_avg_OECD-5&myctry_hh<ht_avg_OECD+5){
        my_adv="close to"
      }
    }
    return(my_adv)
  }

  #################################################################################
  # 2 house prices
  #################################################################################

  sentence_rhp=function(gr_hp, ctry_code, ctry_name){
    if ( nrow(gr_hp) == 0){
      my_sentence="We do not have data for the country in this dimension"
    }else if (nrow(gr_hp) == 2){
    if (gr_hp$rate[gr_hp$before_GFC==1]<=0 &gr_hp$rate[gr_hp$before_GFC==0]<=0){ # euqal to <=0 in both periods
      my_sentence=paste0("However, in few countries real house prices remained stable or even decreased, such as in ", ctry_name)
    } else  if (gr_hp$rate[gr_hp$before_GFC==1]>0 & gr_hp$rate[gr_hp$before_GFC==1]<20 & gr_hp$rate[gr_hp$before_GFC==0]>0 & gr_hp$rate[gr_hp$before_GFC==0]<20){ #moderate overall
      my_sentence=paste0("However, in few countries real house prices remained stable, such as in ", ctry_name)
    } else if (gr_hp$rate[gr_hp$before_GFC==1]>=20 & gr_hp$rate[gr_hp$before_GFC==1]<=50 & gr_hp$rate[gr_hp$before_GFC==0]>=20 & gr_hp$rate[gr_hp$before_GFC==0]<=50){
      my_sentence=paste0(" However, ", ctry_name, " is one of the few countries exhibiting moderate increase in real house prices")
    } else if (gr_hp$rate[gr_hp$before_GFC==1]>50 & gr_hp$rate[gr_hp$before_GFC==1]<=100 & gr_hp$rate[gr_hp$before_GFC==0]>50 & gr_hp$rate[gr_hp$before_GFC==0]<=100){
      my_sentence=paste0( "In the period under review ", ctry_name, " exhibited a steep increase in real house prices")
    }else if (gr_hp$rate[gr_hp$before_GFC==1]>100 & gr_hp$rate[gr_hp$before_GFC==0]>100){
      my_sentence=paste0(" ", ctry_name, " is among the countries exhibiting the highest increase in house prices")
    }else if (gr_hp$rate[gr_hp$before_GFC==1]<gr_hp$OECD_av[gr_hp$before_GFC==1] & gr_hp$rate[gr_hp$before_GFC==0]>=gr_hp$OECD_av[gr_hp$before_GFC==0]){
      my_sentence=paste0("In ", ctry_name, ", after growing at a slower pace in the early 2000s, house prices started to rise faster than the OECD average in 2011")
    }else if (gr_hp$rate[gr_hp$before_GFC==1]>=gr_hp$OECD_av[gr_hp$before_GFC==1] & gr_hp$rate[gr_hp$before_GFC==0]<gr_hp$OECD_av[gr_hp$before_GFC==0]){
      my_sentence=paste0("In ", ctry_name, ", after growing faster than OECD average in the early 2000s, house prices started to slow down after the Global Financial Crisis")
    }else if (gr_hp$rate[gr_hp$before_GFC==1]>=gr_hp$OECD_av[gr_hp$before_GFC==1] & gr_hp$rate[gr_hp$before_GFC==0]>=gr_hp$OECD_av[gr_hp$before_GFC==0]){
      my_sentence=paste0("In ", ctry_name, ", house prices have grown faster than OECD average")
    }else if (gr_hp$rate[gr_hp$before_GFC==1]<gr_hp$OECD_av[gr_hp$before_GFC==1] & gr_hp$rate[gr_hp$before_GFC==0]<gr_hp$OECD_av[gr_hp$before_GFC==0]){
      my_sentence=paste0("In ", ctry_name, ", house prices have grown slower than OECD average")
    } else if  (ctry_code=="DEU"){
      my_sentence="In Germany, after declining in the early 2000s, house prices started to rise faster than the OECD average in 2011 and its growth accelerated from 2016 onwards. The increase in rent prices was moderate compared to OECD countries"
    }else if (ctry_code=="ESP"){
      my_sentence=paste0( "In the years preceding the onset of the Global Financial Crisis, ", ctry_name, " exhibited a steep increase in real house prices")
    }
    } else if (nrow(gr_hp) == 1){
      if (gr_hp$rate<=0){ # euqal to <=0 in both periods
        my_sentence=paste0("However, in few countries real house prices remained stable or even decreased, such as in ", ctry_name)
      } else  if (gr_hp$rate>0 & gr_hp$rate<20){ #moderate overall
        my_sentence=paste0("However, in few countries real house prices remained stable, such as in ", ctry_name)
      } else if (gr_hp$rate>=20 & gr_hp$rate<=50 ){
        my_sentence=paste0(" However, ", ctry_name, " is one of the few countries exhibiting moderate increase in real house prices")
      } else if (gr_hp$rate>50 & gr_hp$rate<=100 ){
        my_sentence=paste0( "In the period under review ", ctry_name, " exhibited a steep increase in real house prices")
      }else if (gr_hp$rate>100 ){
        my_sentence=paste0(" ", ctry_name, " is among the countries exhibiting the highest increase in house prices")
      }
    }
    return(my_sentence)
  }

  #no sentences refering to rent prices BUT COULD BE ADDED
  ################################################################################################
# 3 Investment over GDP
  ################################################################################################
  sentence_INV_GDP=function(IH_GDP_stats, ctry_code, ctry_name){
    IH_GDP_stats<-IH_GDP_stats %>% filter(ISO3_code==ctry_code)
    if ( nrow(IH_GDP_stats) == 0){
      my_sentence="We do not have data for the country in this dimension"
    }else if (nrow(IH_GDP_stats) == 1){
      if  ( ( IH_GDP_stats$av_rate<(IH_GDP_stats$OECD_gr-0.1))  &  (IH_GDP_stats$av_sd < (IH_GDP_stats$OECD_sd-0.1))) { #LOW rate - LOW SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is relatively low and very stable by comparison with other countries")
      }
      else if ( (IH_GDP_stats$av_rate<(IH_GDP_stats$OECD_gr-0.1)) & ( IH_GDP_stats$av_sd >= IH_GDP_stats$OECD_sd-0.1) & (IH_GDP_stats$av_sd <=(IH_GDP_stats$OECD_sd+0.1) ) ) { #LOW rate - avg SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is relatively low and fairly stable by comparison with other countries")
      }
      else if( (IH_GDP_stats$av_rate<(IH_GDP_stats$OECD_gr-0.1)) & (IH_GDP_stats$av_sd > IH_GDP_stats$OECD_sd+0.1) ){ #LOW rate - high SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is relatively low and  quite volatile by comparison with other countries")
      }
      else if ( (IH_GDP_stats$av_rate >= (IH_GDP_stats$OECD_gr-0.1)) & (IH_GDP_stats$av_rate <=(IH_GDP_stats$OECD_gr+0.1))  &  IH_GDP_stats$av_sd < (IH_GDP_stats$OECD_sd-0.1)) { #average rate - LOW SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is about average  and very stable by comparison with other countries")
      }
      else if (((IH_GDP_stats$av_rate >= (IH_GDP_stats$OECD_gr-0.1) & IH_GDP_stats$av_rate <=(IH_GDP_stats$OECD_gr+0.1))) & (IH_GDP_stats$av_sd <=(IH_GDP_stats$OECD_sd+0.1) ))  { #average rate - avg SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is about average and fairly stable by comparison with other countries")
      }
      else if ( (IH_GDP_stats$av_rate >= (IH_GDP_stats$OECD_gr-0.1) & IH_GDP_stats$av_rate <=(IH_GDP_stats$OECD_gr+0.1)) & (IH_GDP_stats$av_sd > IH_GDP_stats$OECD_sd+0.1)) { #average rate - high SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is about average and  quite volatile by comparison with other countries")
      }
      else if ( (IH_GDP_stats$av_rate >IH_GDP_stats$OECD_gr+0.1)  &  IH_GDP_stats$av_sd < (IH_GDP_stats$OECD_sd-0.1)) { #HIGH rate - LOW SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is relatively high and very stable by comparison with other countries")
      }
      else if ( (IH_GDP_stats$av_rate >IH_GDP_stats$OECD_gr+0.1)  & ( IH_GDP_stats$av_sd >= (IH_GDP_stats$OECD_sd-0.1)) & IH_GDP_stats$av_sd <=(IH_GDP_stats$OECD_sd+0.1) )  { #HIGH rate - avg SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is relatively high and fairly stable by comparison with other countries")
      }
      else if ( (IH_GDP_stats$av_rate >IH_GDP_stats$OECD_gr+0.1)  & (IH_GDP_stats$av_sd > IH_GDP_stats$OECD_sd+0.1)) { #HIGH rate - high SD
        my_sentence=paste0(ctry_name, "'s housing investment rate is relatively high and  quite volatile by comparison with other countries")
      }
    }
    return(my_sentence)
  }


  ################################################################################################
  # 4 mortgage claims
  ########################################################################################


  sentece_mortg=function(avg_mortg, mortgage_data_myctr,myctry_hh, ht_avg_OECD){
    if (is.na(mortgage_data_myctr) )  {
      my_adv="We do not have data for the country in this dimension"
    }  else if (is.na(mortgage_data_myctr)==F & is.na(myctry_hh)==F  ){
      if (mortgage_data_myctr<avg_mortg-5 & myctry_hh<ht_avg_OECD-5){
        my_adv="modest, reflecting a relatively low homeownership rate"
      } else if (mortgage_data_myctr<avg_mortg-5 ){
        my_adv="relatively modest"
      } else if (mortgage_data_myctr>avg_mortg-5.0 & mortgage_data_myctr<avg_mortg+5){
        my_adv="close to average by international comparison"
      }else if (mortgage_data_myctr>avg_mortg+5 & myctry_hh>ht_avg_OECD+5){
        my_adv="high, reflecting a relatively high homeownership rate"
      }else if (mortgage_data_myctr>avg_mortg+5 ){
        my_adv="relatively high by international comparison"
      }
      }  else if (is.na(mortgage_data_myctr)==F & is.na(myctry_hh)==T ){
      if (mortgage_data_myctr<avg_mortg-5) {
        my_adv="modest"
      } else if (mortgage_data_myctr<avg_mortg-5 ){
        my_adv="relatively modest"
      } else if (mortgage_data_myctr>avg_mortg-5.0 & mortgage_data_myctr<avg_mortg+5){
        my_adv="close to average by international comparison"
      }else if (mortgage_data_myctr>avg_mortg+5 ){
        my_adv="relatively high by international comparison"
      }
    }

    return(my_adv)
  }

  ########################################################################################

  sentence1=paste0("Housing policies affect people’s well-being through a wide range of channels including access to decent shelter, environmental quality, efficient use of scarce resources, type and extent of commuting, as well as its contribution to strong and resilient economic growth. This note provides a cross-country perspective on " ,ctry_name, "’s housing-related indicators and policy settings.")

  sentence2=paste0("Households’ tenure choices depend on demographics and/or socio-economic factors, such as population ageing, as well as policies related to housing taxation and rental regulations. There are large differences in tenure structure across OECD and key partner countries: homeownership in ", ctry_name," is ", sentence_homewonership(ht_avg_OECD, myctry_hh)," OECD average (Figure a). ")

  sentence3=paste0("Real house prices and rent prices have risen strongly in many countries since the 1990s, with prices increasing by more than 100 percent in those countries experiencing the largest increases (Figure b). ",sentence_rhp(gr_hp, ctry_code, ctry_name), ".")

  sentence4=paste0(" ", sentence_INV_GDP(IH_GDP_stats, ctry_code, ctry_name), " (Figure c).")

  sentence5=paste0(" Finally, mortgage markets play a crucial role in housing markets since housing generally constitutes the household's single largest financial outlay. The ratio of outstanding households' mortgage claims to GDP in ", ctry_name," is ", sentece_mortg(avg_mortg, mortgage_data_myctr,myctry_hh, ht_avg_OECD)," (Figure d).")

  return(paste0(sentence1,sentence2,sentence3,sentence4,sentence5, sep=" "))

}
