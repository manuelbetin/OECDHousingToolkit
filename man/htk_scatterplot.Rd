\name{htk_scatterplot}
\alias{htk_scatterplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scatter barplots with OECD template style to display correlation
between two variables in the housing toolkit chapters
}
\description{
The functions generates scatter plots in an agreed format to
ensure presentation consistency in the dashboard
}
\usage{
htk_scatterplot(data_source,myvar_x,myvar_y,my_label,
Xlabel=NULL,Ylabel=NULL,title=NULL,subtitle=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_source}{dataframe that contains the variables to plot}
  \item{myvar_x}{x axis variable name}
  \item{myvar_y}{y axis  variable name}
  \item{my_label}{labels used to label the points in the scatter plot}
  \item{Xlabel}{label of the X axis}
  \item{Ylabel}{label of the Y axis}
  \item{title}{title of the plot}
  \item{subtitle}{subtitle of the plot}

}
\details{
Packages plyr,dplyr, ggplot and ggrepel are to be installed  before
}
\value{
returns a standardized scatter plot
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maxime Nguyen,
Manuel Betin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
