\name{loadEffort}
\alias{loadEffort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadEffort(folder)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
%%     ~~Describe \code{folder} here~~
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
function (folder) 
{
    fnames <- dir(folder, full.names = TRUE)
    df <- do.call(rbind, lapply(fnames, readStationFile))
    df <- df[df$UpdateOf != 0, c("UTC", "UTCMilliseconds", "Comment", 
        "station", "file", "datetime")]
    df$Comment <- sub("[[:space:]]+$", "\\1", df$Comment)
    df <- df[order(df$station, df$datetime), ]
    paired.effort <- sapply(1:nrow(df), function(i) {
        if (df$Comment[i] == "START EFFORT") 
            return(df$Comment[i + 1] == "END EFFORT")
        if (i > 1 & df$Comment[i] == "END EFFORT") 
            return(df$Comment[i - 1] == "START EFFORT")
        FALSE
    })
    df[paired.effort, ]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
