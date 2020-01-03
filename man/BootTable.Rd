\name{BootTable}
\alias{BootTable}
\title{
Create a table of bootstrapped confidence intervals for all edges of a Joint Graphical Lasso model.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BootTable(BootOut)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BootOut}{
The output from \code{GroupNetworkBoot}
}
}
}
\details{
Summary table of the output of \code{GroupNetworkBoot}
}
\value{
\item{Var1}{Nodes included in each edge}
\item{Var2}{Nodes included in each edge}
\item{edges}{Edge identifier}
\item{sample}{sample value of each edge}
\item{boot.mean}{mean of boostrapped values of each edge}
\item{ci.lb}{lower bound of the .95 confidence interval}
\item{ci.up}{upper bound of the .95 confidence interval}
\item{boot.zero}{proportion of times an edge was estimated as equal to zero}
\item{g}{group in which the edge was estimated}

}
\references{
Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological networks and their accuracy: A tutorial paper. Behavior Research Methods, 50(1), 195–212. https://doi.org/10.3758/s13428-017-0862-1
Danaher, P., Wang, P., & Witten, D. M. (2014). The joint graphical lasso for inverse covariance estimation across multiple classes. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 76(2), 373–397. https://doi.org/10.1111/rssb.12033
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