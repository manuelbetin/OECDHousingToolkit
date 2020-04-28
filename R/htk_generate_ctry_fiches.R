htk_generate_ctry_fiches = function(country_code_list) {
  #' @title generate the country fiche for the selected country
  #' @description generate the country fiche for the selected country
  #' @param country_code_list a vector of iso3 codes for the
  #' group of countries that you want to generate the fiches
  #' @return pdf document from rmardown skeleton
  #' @author Manuel Betin
  #' @export
  lapply(country_code_list,function(country_code){
    rmarkdown::render(
      "skeleton.Rmd", params = list(
        ctry_code = country_code,
        ctry_name=countrycode(country_code,origin="iso3c",destination="country.name")
      ),
      output_file = paste0("CountryFiches-", country_name, ".pdf")
    )
  })

}
