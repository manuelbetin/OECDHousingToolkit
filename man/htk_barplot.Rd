\name{htk_barplot}
\alias{htk_barplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
barplots with OECD template style to display cross country
comparisons in the housing toolkit chapters
}
\description{
The functions generates barplots displaying the value of the
selected indicator for each country and with the average OECD
value in highlighted. The plot respect the OECD style and allow
the generation of standardize figures for all the chapters of the
housing toolkit
}
\usage{
htk_barplot(data_source,x,y,y2=NULL,y3=NULL,Xlabel=NULL,
Ylabel=NULL,title=NULL,subtitle=NULL, file=NULL, path=NULL)
}

\arguments{
  \item{data_source}{dataframe that contains the
  variables and dates to be chosen}
  \item{x}{x, often OECD countries}
  \item{y}{y, variable to be plotted}
  \item{y2}{other variable to be plotted}
  \item{y3}{other variable to be plotted}
  \item{Xlabel}{label of the X axis}
  \item{Ylabel}{label of the Y axis}
  \item{title}{title of the plot}
  \item{subtitle}{subtitle of the plot}
  \item{file}{name of the file that contains the plot if
  you decide to save it, filename must end with .png}
  \item{path}{specify exit path of the file}
}

\value{
returns a standardized barplot
}

\author{
Maxime Nguyen,
Manuel Betin
}
