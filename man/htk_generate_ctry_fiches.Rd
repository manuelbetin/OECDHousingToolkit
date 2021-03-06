% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/htk_generate_ctry_fiches.R
\name{htk_generate_ctry_fiches}
\alias{htk_generate_ctry_fiches}
\title{generate the country fiche for the selected country}
\usage{
htk_generate_ctry_fiches(
  Rmdfile = "skeleton.Rmd",
  country_code_list,
  path = NULL
)
}
\arguments{
\item{Rmdfile}{the name of the file (.Rmd) that is used as template}

\item{country_code_list}{a vector of iso3 codes for the}

\item{path}{name of the directory where to store the pdf output
group of countries that you want to generate the fiches}
}
\value{
pdf document from rmardown skeleton
}
\description{
generate the country fiche for the selected country
}
\author{
Manuel Betin
}
