\name{htk_lineplot}
\alias{htk_lineplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Line barplots with OECD template style to display timeseries evolutions in the housing toolkit chapters}
\description{
Function that plots a lines in a standardized way.
The function is built on the assumption that the data is
structured in the agreed format (first column as countries,
second as period, others as key variables)
}
\usage{
htk_lineplot(data = resilience_database,
                   subtitle = ".", title = NULL, Xlabel = "Date", Ylabel
                   = "Price index", ymin = 0, ymax = 120, ybreak = 10,
                   xbreak = "5 years", start_date = 1970, end_date =
                   2018, file = NULL, path = NULL, width = 4, height = 4,
                   dpi = 300)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame to be used for the plot}
  \item{country_code}{ISO3 code of the chosen country}
  \item{subtitle}{subtitle of the plot}
  \item{title}{title of the plot}
  \item{Xlabel}{label of the x axis}
  \item{Ylabel}{label of the y axis}
  \item{ymin}{minimum y value to be displayed}
  \item{ymax}{maximum y value to be displayed}
  \item{ybreak}{difference between Y ticks}
  \item{xbreak}{differnece between X ticks}
  \item{start_date}{data start date}
  \item{end_date}{data end date}
  \item{file}{name of the file containing the plot were it to be saved, name must be followed by .png}
  \item{path}{path of where the file is to be saved}
  \item{width}{width of the image to be saved}
  \item{heigth}{height of the image to be saved}
  \item{dpi}{resolution}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a standardized single line plot
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maxime Nguyen,
Manuel Betin
}
