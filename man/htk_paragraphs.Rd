\name{htk_paragraphs}
\alias{htk_paragraphs}
\docType{data}
\title{
Country fiches: Country specific paragraphs source
}
\description{
Dataset containing the country specific paragraphs
used to generate the country fiches. Depending on the
country selected it will grab the paragraph corresponding
to the given country
}
\usage{data("htk_paragraphs")}
\format{
  A data frame with 46 observations on the following 6 variables.
  \describe{
    \item{\code{country}}{a character vector}
    \item{\code{Intro}}{a character vector}
    \item{\code{Efficiency}}{a character vector}
    \item{\code{Inclusiveness}}{a character vector}
    \item{\code{Sustainability}}{a character vector}
    \item{\code{...6}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(htk_paragraphs)
## maybe str(htk_paragraphs) ; plot(htk_paragraphs) ...
}
\keyword{datasets}
