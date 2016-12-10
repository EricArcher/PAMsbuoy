\name{readStationFile}
\alias{readStationFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readStationFile(f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
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
function (f) 
{
    df <- read.csv(f, na.strings = c("", " ", "NA"), stringsAsFactors = FALSE)
    if (nrow(df) == 0) 
        return(NULL)
    dt <- paste0(df$UTC, ".", df$UTCMilliseconds)
    df$file <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(f))
    df$station <- gsub("_P[[:alnum:]]*", "", df$file)
    df$datetime <- strptime(dt, format = "\%m/\%d/\%Y \%H:\%M:\%OS", 
        tz = "GMT")
    df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
