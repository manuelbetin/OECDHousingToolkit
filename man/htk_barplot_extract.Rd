\name{htk_barplot_extract}
\alias{htk_barplot_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract and reorder ECO/Prognoz data
}
\description{
Function extracts data from ECO/Prognoz on OECD countries and rearranges the data in an ascending order
The function takes two arguments:
- the variable that should be written in its metadata code and preceded by a dot. The metadata code can be found by runnning Fe_GetAllSeriesMetadata("OECD","FE_English"), after having imported all ECO functions
- the year of the data that wants to be plotted
}
\usage{
htk_barplot_extract(variable, date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{variable to be displayed, written in its ECO metadata code}
  \item{date}{chosen year to be plotted}
}
\value{
returns a barplot of all OECD countries on the chosen variable at the chosen date
}
\author{
Maxime Nguyen
Manuel Betin
}

