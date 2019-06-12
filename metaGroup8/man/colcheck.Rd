\name{colcheck}
\alias{colcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check for valid columns
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Input data and columns are checked to see if they are valid. Return 1 if valid else return 0.
}
\usage{
colcheck(a, b, c = NULL, d = NULL, e = NULL)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Checks if the number of columns in the data frames are all equal to one another.
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

## check if the columns are valid
colcheck(data1, data2)


## The function is currently defined as
function (a, b, c = NULL, d = NULL, e = NULL) 
{
    if (!is.null(e) && !is.null(d) && !is.null(c)) {
        if (ncol(a) == ncol(b) && ncol(b) == ncol(c) && ncol(b) == 
            ncol(d) && ncol(b) == ncol(e)) {
            return(1)
        }
        else {
            return(0)
        }
    }
    if (is.null(e) && !is.null(d) && !is.null(c)) {
        if (ncol(a) == ncol(b) && ncol(b) == ncol(c) && ncol(b) == 
            ncol(d)) {
            return(1)
        }
        else {
            return(0)
        }
    }
    if (is.null(e) && is.null(d) && !is.null(c)) {
        if (ncol(a) == ncol(b) && ncol(b) == ncol(c)) {
            return(1)
        }
        else {
            return(0)
        }
    }
    if (is.null(e) && is.null(d) && is.null(c)) {
        if (ncol(a) == ncol(b)) {
            return(1)
        }
        else {
            return(0)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
