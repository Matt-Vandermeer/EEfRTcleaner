\name{EEfRTcleanerLoop}
\alias{EEfRTcleanerLoop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Loop through directory of CSV files to clean EEfRT behavioural data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EEfRTcleanerLoop(input, output, EEfRT_Type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Path of directory containing EEfRT raw behavioural data .csv files. Do not include a final slash on the path

e.g., "Z:/CPD Lab Studies/ACED/03_Data_and_Syntax/05_ERP_Data/02_Cognitive_EFFrT/01_Behavioural_Data/01_Data"
}
  \item{output}{
Path of directory you'd like to send cleaned EEfRT data .csv files to.

e.g., "Z:/CPD Lab Studies/ACED/03_Data_and_Syntax/05_ERP_Data/02_Cognitive_EFFrT/01_Behavioural_Data/01_Data/"
  }
  \item{EEfRT_Type}{
  Type of EEfRT behavioural data you are processing
  "physical" = Physical EEfRT behavioural data
  "cognitive" = Cognitive EEfRT behavioural data
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
Matthew Richard John Vandermeer, PhD, CPsych (supervised practice)
  \email{dr.matt.vandermeer@protonmail.com}
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
