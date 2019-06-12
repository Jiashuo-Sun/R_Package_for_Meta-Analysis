\name{poolp}
\alias{poolp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pooled p-values for each biomarker across K Data
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Implements Fisher, Stouffer, Minimum p-value (minP), Maximum p-value (maxP) methods for pooling p-values
}
\usage{
poolp(p, method = c("fisher", "stouffer", "minP", "maxP"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{p} here~~
the vector of p-values to be pooled
}
  \item{method}{
%%     ~~Describe \code{method} here~~
selection of which methods to include when pooling p-values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Ma     Zhiyan  
Sun    Jiashuo  
Sun     Oliver  
Zhao      Qiang

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## generate vector of p-values
p = runif(100)

## call poolp with p-values vector and method selection
poolp(p, "fisher")

## The function is currently defined as
function (p, method = c("fisher", "stouffer", "minP", "maxP")) 
{
    fisher = function(p) {
        k = length(p)
        X2 = -2 * sum(log(p))
        p.value = pchisq(X2, df = 2 * k, lower.tail = F)
        return(p.value)
    }
    stouffer = function(p) {
        k = length(p)
        T.stouffer = sum(qnorm(p)/sqrt(k))
        p.value = pnorm(T.stouffer)
        return(p.value)
    }
    minP = function(p) {
        k <- length(p)
        min.p <- min(p)
        p.value <- pbeta(min.p, k, 1)
        return(p.value)
    }
    maxP = function(p) {
        k <- length(p)
        max.p <- max(p)
        p.value <- pbeta(max.p, 1, k)
    }
    p = matrix(p, nrow = 1)
    p.value = apply(p, 1, method)
    return(p.value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
