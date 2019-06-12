\name{metaGroup8}
\alias{metaGroup8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Main Function
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Takes in the dataframes and method of pooling p-values as arguments and returns pooled p-values
}
\usage{
metaGroup8(a, b, c = NULL, d = NULL, e = NULL, method = c("fisher", "stouffer", "minP", "maxP"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
first dataframe
}
  \item{b}{
%%     ~~Describe \code{b} here~~
second dataframe
}
  \item{c}{
%%     ~~Describe \code{c} here~~
third dataframe (optional)
}
  \item{d}{
%%     ~~Describe \code{d} here~~
fourth dataframe (optional)
}
  \item{e}{
%%     ~~Describe \code{e} here~~
fifth dataframe (optional)
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Choose between fisher, stouffer, minP, and maxP methods of pooling p-values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
metaGroup8 first performs a column check by calling colcheck(). If the columns are valid then it will perform the best statistical test to investigate if there is any group difference for each biomarker within each data frame by calling beststatest(). Lastly the p-values for each biomarker across the different data frames are pooled and returned to the user.
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

## generate two dataframes
set.seed(123)
p <- 100
## dataframe 1
data1 <- data.frame(group=sample(1:3,200,replace=TRUE), matrix(rnorm(p*200),ncol=p))
## dataframe 2
data2 <- data.frame(group=sample(1:2,150,replace=TRUE), matrix(rnorm(p*150),ncol=p))

## get the pooled p-values
metaGroup8(data1, data2, method="stouffer")

## The function is currently defined as
function (a, b, c = NULL, d = NULL, e = NULL, method = c("fisher", 
    "stouffer", "minP", "maxP")) 
{
    checkresult = colcheck(a, b, c, d, e)
    if (checkresult == 1) {
        all.p.value = beststatest(a)
        all.p.value = cbind(all.p.value, beststatest(b))
        all.p.value = cbind(all.p.value, beststatest(c))
        all.p.value = cbind(all.p.value, beststatest(d))
        all.p.value = cbind(all.p.value, beststatest(e))
        p.value = apply(all.p.value, 1, poolp, method = method)
        return(list(biomarkers = names(a[-1]), pooled.p.value = p.value))
    }
    else {
        print("Error: Different length of colume")
        return(NULL)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
