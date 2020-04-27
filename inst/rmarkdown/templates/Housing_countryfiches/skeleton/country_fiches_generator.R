
library(rmarkdown)

render_report = function(country_code,country_name) {
  rmarkdown::render(
    "skeleton.Rmd", params = list(
      ctry_code = country_code,
      ctry_name=country_name
    ),
    output_file = paste0("CountryFiches-", country_name, ".pdf")
  )
}


ctries=c("ARG"="Argentina",
         "FRA"="France")

lapply(1:length(ctries),function(x){
  name=ctries[[x]]
  code=names(ctries)[x]
  render_report(code,name)
})


