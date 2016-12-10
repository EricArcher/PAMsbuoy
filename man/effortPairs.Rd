\name{effortPairs}
\alias{effortPairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
effortPairs(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
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

## The function is currently defined as
function (df) 
{
    eff.df <- do.call(rbind, lapply(seq(1, nrow(df), by = 2), 
        function(i) {
            start <- df$datetime[i]
            end <- df$datetime[i + 1]
            data.frame(file = df$file[i], station = df$station[i], 
                start = start, end = end, interval = difftime(end, 
                  start, units = "secs"))
        }))
    eff.df <- do.call(rbind, by(eff.df, eff.df$station, function(x) {
        data.frame(start = min(x$start), end = max(x$end), duration = sum(x$interval))
    }))
    cbind(station = rownames(eff.df), eff.df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
