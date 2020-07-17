htk_generate_ctry_fiches = function(Rmdfile="skeleton.Rmd",country_code_list,path=NULL) {
  #' @title generate the country fiche for the selected country
  #' @description generate the country fiche for the selected country
  #' @param Rmdfile the name of the file (.Rmd) that is used as template
  #' @param country_code_list a vector of iso3 codes for the
  #' @param path name of the directory where to store the pdf output
  #' group of countries that you want to generate the fiches
  #' @return pdf document from rmardown skeleton
  #' @author Manuel Betin
  #' @export
  #'


  if(!is.null(path)){
    dir.create(path)
  }

  lapply(country_code_list,function(country_code){
   country_name=countrycode::countrycode(country_code,origin="iso3c",destination="country.name")
    rmarkdown::render(
      Rmdfile, params = list(
        ctry_code = country_code,
        ctry_name=country_name
        #ctry_adj=country_adj
      ),
      output_file = ifelse(!is.null(path),paste0(path,"/OECD_Housing_Country_Fiches-", country_code, ".pdf"),paste0("CountryFiches-", country_name, ".pdf"))
    )
  })

}



