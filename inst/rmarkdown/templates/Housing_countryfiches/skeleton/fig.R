
packages <- c("dplyr",
              "rio",
              "tictoc",
              "lubridate",
              "stringr",
              "crayon",
              "tidyr",
              "tidytext",
              "scales",
              "countrycode",
              "ggrepel",
              "ggplot2")


## load common packages

library(SetUpProject)
library(countrycode)
load.my.packages(packages)

vars_cluster=rio::import("Vars_cluster.xlsx")
vars_cluster <- vars_cluster %>% mutate(Iso_code3=countrycode(country,origin="country.name",destination="iso3c"))
vars_cluster <- vars_cluster %>%  select(Iso_code3, everything()) %>%
  rename(country_name=country,
         country=Iso_code3)

#######################################################################

ctry="FRA"
var_codes<-c("ovc_tot", "tenure_oo")
var_names=c("mortage_test","owner-outright")
mydata=vars_cluster
htk_CyC=function(mydata,ctry,var_codes,var_names){

  country_name=countrycode(ctry,origin = "iso3c",destination="country.name")

vars_needed<- mydata %>%  select(country, var_codes) %>%
  mutate_at(vars(var_codes),.funs=list(mean=~mean(.,na.rm=T),
                                    min=~min(.na.rm=T),
                                    max=~max(.,na.rm=T)))
# 2. create min, max, mean, valu
for (var in var_codes) {
  name_col=paste0(var, '_country_min')
  vars_needed<- vars_needed %>%
    mutate(!!name_col := vars_needed$country[which.min(get(var))] )
}

for (var in var_codes) {
  name_col=paste0(var, '_country_max')
  vars_needed<- vars_needed %>%
    mutate(!!name_col := vars_needed$country[which.max(get(var))] )

}

temp_long<- vars_needed %>%  filter(country==ctry)%>% dplyr::select(-country) %>%
  gather(key = "variable", value = "value") %>%
  mutate(main_v=ifelse( (str_detect(variable, "ovc_tot")), "ovc_tot", "tenure_oo"),
         main_v=ifelse( (str_detect(variable, "tenure_oo")), "tenure_oo", main_v)) %>%
  mutate(ext=ifelse( (str_detect(variable, "mean")), "mean", "value"),
         ext=ifelse( (str_detect(variable, "min")), "min", ext),
         ext=ifelse( (str_detect(variable, "max")), "max", ext),
         ext=ifelse( (str_detect(variable, "country_max")), "country_max", ext),
         ext=ifelse( (str_detect(variable, "country_min")), "country_min", ext)) %>%
  dplyr::select(-variable)

final<-reshape(temp_long, idvar = "main_v" , timevar =  "ext" , direction = "wide")

name_vars<-c("value.value","value.mean","value.min","value.max")
final<-  final %>% mutate_at(vars(name_vars),as.numeric) %>%
  mutate(value_scaled = (value.value-value.min) / (value.max-value.min ),
          mean_scaled =  ( value.mean-value.min) / (value.max-value.min ) )%>%
  mutate(group = ifelse(main_v=="ovc_tot" , "A", NA),
         group = ifelse(main_v=="tenure_oo" , "B", group))

ggplot(data=final)+
  geom_point(aes(x=main_v, y= value_scaled ), shape=19, color='red', size=4)  +
  geom_point( aes(x=main_v, y=mean_scaled  ) , shape=19, color='darkblue', size=3) +
  geom_point(aes(x=main_v, y=0), shape=1, color='grey', size=2)  +
  geom_point(aes(x=main_v, y=1 ), shape=1, color='grey', size=2) +
  geom_segment( aes(x=main_v ,xend=main_v, y=0, yend=1), color="grey") +
  coord_flip() +
  labs(x = "", y="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks =element_blank() )+
  scale_x_discrete(breaks=final$main_v,labels=var_names) +
  geom_text(aes(x=main_v, y= value_scaled   , label=paste(country_name,": ", round(value.value, digits = 2))),
            size=3, nudge_x = -0.1, nudge_y = 0.0,  check_overlap = TRUE) +
  geom_text(aes(x=main_v, y= mean_scaled   , label=paste("OECD: ", round(value.mean, digits = 2))),
            size=3, nudge_x = 0.1, nudge_y = 0.0,  check_overlap = TRUE) +
  geom_text(aes(x=main_v, y= 0   , label=paste(value.country_min,":" , round(value.min, digits = 2))),
            size=3, nudge_x = 0.1, nudge_y = 0.0,  check_overlap = TRUE) +
  geom_text(aes(x=main_v, y= 1   , label=paste(value.country_max,":", round(value.max, digits = 2))),
            size=3, nudge_x = 0.1, nudge_y = -0.01,  check_overlap = TRUE)+
  annotate("text", x =2.3, y = 0.05, label = "Bottom OECD performer", size=3) +
  annotate("text", x =2.3, y = 0.95, label = "Top OECD performer", size=3)

}

htk_CyC(vars_cluster,"USA",var_codes,var_names)







############################################################################################################





