\name{clipBuoyLatLon}
\alias{clipBuoyLatLon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clipBuoyLatLon(df, lat.range = NULL, lon.range = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{lat.range}{
%%     ~~Describe \code{lat.range} here~~
}
  \item{lon.range}{
%%     ~~Describe \code{lon.range} here~~
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
function (df, lat.range = NULL, lon.range = NULL) 
{
    if (is.null(lat.range)) 
        lat.range <- range(df$BuoyLatitude)
    if (is.null(lon.range)) 
        lon.range <- range(df$BuoyLongitude)
    df <- df[!(is.na(df$BuoyLatitude) | is.na(df$BuoyLongitude)), 
        ]
    lat.good <- isBetween(df$BuoyLatitude, lat.range, include.ends = TRUE)
    lon.good <- isBetween(df$BuoyLongitude, lon.range, include.ends = TRUE)
    df[lat.good & lon.good, ]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
