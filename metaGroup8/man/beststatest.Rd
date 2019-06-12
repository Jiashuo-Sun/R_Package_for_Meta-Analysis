\name{beststatest}
\alias{beststatest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Select the Best Statistical Test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
beststatest will check for normality of the dataframe by group, perform the appopriate statistical test and return a vector of p-values, one for each biomarker in the dataframe.
}
\usage{
beststatest(onedataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{onedataframe}{
%%     ~~Describe \code{onedataframe} here~~
the dataframe which contains group membership as the first column and biomarker measurements as the remaining columns
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If the dataframe is normal and there are > 2 groups it will perform one way ANOVA to determine if there is group difference. If the biomarker is not normally distributed, it will use the Kruskal Wallis test (this is the non-parametric version of one way ANOVA). If there are = 2 groups it will perform two sample t-test to determine if there is group difference (also checking for variance). If the biomarker is not normally distributed, use the Wilcoxon rank sum test. Returns a vector of p-values, one for each biomarker in the dataframe.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Vector of p-values
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
## Create a dataframe
set.seed(123)
p <- 100
data1 <- data.frame(group=sample(1:3,200,replace=TRUE),
matrix(rnorm(p*200),ncol=p))

## pass in data1 dataframe arg into beststatest and assign to p.values
p.values = beststatest(data1)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (onedataframe) 
{
    my.pvalue1 <- function(x) {
        pv = NULL
        z = as.numeric(length(unique(x[, 1])))
        if (z != 2) {
            print("Error: The dataframe has more than 2 group")
        }
        if (z == 2) {
            for (i in 2:dim(x)[2]) {
                nor = shapiro.test(x[, i])$p.value
                if (nor >= 0.05) {
                  a1 = x[, i][x[, 1] == 1]
                  a2 = x[, i][x[, 1] == 2]
                  a = var.test(a1, a2)$p.value
                  if (a >= 0.05) {
                    pvalue = t.test(a1, a2, var.equal = TRUE)$p.value
                  }
                  if (a < 0.05) {
                    pvalue = t.test(a1, a2, var.equal = FALSE)$p.value
                  }
                  pv = c(pv, pvalue)
                }
                if (nor < 0.05) {
                  a1 = x[, i][x[, 1] == 1]
                  a2 = x[, i][x[, 1] == 2]
                  pvalue = wilcox.test(a1, a2)$p.value
                  pv = c(pv, pvalue)
                }
            }
            return(pv)
        }
    }
    my.pvalue2 <- function(x) {
        a = as.numeric(length(unique(x[, 1])))
        pv = NULL
        for (i in 2:dim(x)[2]) {
            A = NULL
            ALL = NULL
            group = NULL
            nor = shapiro.test(x[, i])$p.value
            if (nor >= 0.05) {
                for (j in 1:a) {
                  A[j] = as.data.frame(x[, i][x[, 1] == j])
                  ALL = c(ALL, as.array(A[j])[[1]])
                  group = c(group, rep(j, length(as.array(A[j])[[1]])))
                }
                ydata = data.frame(ALL = ALL, group = factor(group))
                fit = lm(ALL ~ group, data = ydata)
                pvalue = anova(fit)$Pr[1]
                pv = c(pv, pvalue)
            }
            if (nor < 0.05) {
                pvalue = kruskal.test(x[, i] ~ as.factor(x[, 
                  1]))$p.value
                pv = c(pv, pvalue)
            }
        }
        return(pv)
    }
    x = onedataframe
    if (is.null(x)) {
        return(NULL)
    }
    a = as.numeric(length(unique(x[, 1])))
    if (a == 2) {
        return(my.pvalue1(x))
    }
    if (a > 2) {
        return(my.pvalue2(x))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
