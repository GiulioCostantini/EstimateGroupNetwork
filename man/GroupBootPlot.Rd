\name{GroupBootPlot}
\alias{GroupBootPlot}
\title{
Create a plot of bootstrapped confidence intervals for all edges of a Joint Graphical Lasso model.
}
\description{
This function plots output from bootstrapped networks computed with \code{GroupNetworkBoot}.
}
\usage{
GroupBootPlot <- function(
      BootOut, 
      GroupNames, 
      edges.x, 
      edges.y, 
      labels = TRUE, 
      transparency = 0.15, 
      point.size = 1.5, 
      line.size = 1, 
      legend.position = "none",
      GroupNamesCheck = FALSE
)          
                     
                     
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BootOut}{The output from \code{GroupNetworkBoot}.
}
  \item{GroupNames}{
A vector of optional group names that will be printed as facet labels in plot. By default, names of the networks are taken. If indicated, GroupNames should match the alphabetical order of names of networks. If unsure, check match of names by setting \code{GroupNamesCheck = TRUE}.
}
\item{edges.x}{
  If only a subset of edge combinations is of interest for the plot, this subset can be specified by setting \code{edges.x} and \code{edges.y}. Specifically, node names can be specified as vectors for \code{edges.x} and \code{edges.y} and all unique combinations of \code{edges.x} and \code{edges.y} will be plotted. For example, \code{edges.x = c("a", "b")} and \code{edges.y = "c"} will plot edges a-c and b-c but not a-b.
}
\item{edges.y}{
  See \code{edges.x}.
}
  \item{labels}{
  Logical, should edge labels be included in plots. Default is \code{labels = TRUE}.
}
\item{transparency}{
  Set alpha channel (transparency) for confidence interval ribbon in plot.
}
\item{point.size}{
  Set point size. 
}
\item{line.size}{
  Set line size. 
}
\item{legend.position}{
  Define legend position to indicate colour for sample and bootstrap means. See ?theme in ggplot2.
}
\item{GroupNamesCheck}{
  Option to print match of indicated GroupNames to console. Only prints if GroupNames is specified. See \code{GroupNames = TRUE} as well.
}
}

\details{
The code for the Joint Graphical Lasso procedure was adapted from the R package \pkg{JGL}. Some of the code for the cross-validation procedure was adapted from package \pkg{parcor}. Some of the code was inspired by package \pkg{qgraph}.
}
\value{
The output of GroupBootPlot returns a plot based on \pkg{ggplot2} with containing the bootstrapped confidence intervals of edges across groups.
}

\references{
Danaher, P., Wang, P., and Witten, D. M. (2014). The joint graphical lasso for inverse covariance estimation across multiple classes. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 76(2), 373-397. http://doi.org/10.1111/rssb.12033
}
\author{
Nils Kappelmann <n.kappelmann@gmail.com>, Giulio Costantini
}
\seealso{
\pkg{JGL}, \pkg{qgraph}, \pkg{parcor}, \link{covNoBessel}
}
\examples{

\dontrun{
## Load packages:
library("qgraph")
library("parallel")
library("psych")
library("mvtnorm")
library("tidyverse")
library("EstimateGroupNetwork")

# In this example, the BFI network of males and females are compared for the subset of Agreeableness items
# Load BFI data
data(bfi)

## The bfi data is subset to Agreeableness items only for the first 500 individuals to decrease computational time
bfi <- bfi[, c(paste("A", 1:5, sep = ""), "gender")]

# remove observations with missing values on items or gender
bfi <- na.omit(bfi)

# Create list split by gender
bfi_list <- list(males = bfi[bfi$gender == 1, 1:5],
                 females = bfi[bfi$gender == 2, 1:5])

# Estimate JGL:
bfi_net <- EstimateGroupNetwork(bfi_list, inputType = "list.of.dataframes", simplifyOutput = FALSE)

# Bootstrap network 10 times (this will take a few minutes)
boot_bfi_net <- GroupNetworkBoot(data_list = bfi_list, groupNetwork = bfi_net, 
                                 nboots = 10, seed = 1234, ncores = 1)

# use BootTable to obtain a table with information for each boostrapped edge
BootTable(boot_bfi_net)

## Use GroupBootPlot to obtain plots as a list with each group plot as one element
GroupBootPlot(boot_bfi_net) 

## Get plot for a subset of edges (here: all edges including A1). Also check Groupnames
GroupBootPlot(boot_bfi_net, edges.x = "A1", edges.y = c("A2", "A3", "A4", "A5"), 
              GroupNames = c("Females", "Males"), GroupNamesCheck = TRUE, legend.position = "top")




}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}% use one of RShowDoc("KEYWORDS")
\keyword{multivariate}