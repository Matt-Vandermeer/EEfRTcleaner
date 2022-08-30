\name{Clean_Cognitive_EEfRT}
\alias{Clean_Cognitive_EEfRT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Clean Cognitive EEfRT Data
}
\description{
Function that takes an ACED Cognitive EEfRT behavioural data set and reconstructs it
into two clean .csv data files.
}
\usage{
Clean_Cognitive_EEfRT(file_path, output_directory)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_path}{
  Full path of ACED cognitive EEfRT behavioural data .csv file to be processed.

  e.g., "Z:\CPD Lab Studies\ACED\03_Data_and_Syntax\05_ERP_Data\02_Cognitive_EFFrT\01_Behavioural_Data\01_Data\ACED9999_CognitiveEEfRT.csv"
}
  \item{output_directory}{
  Full path of the directory where you want to save the processed .csv files to.

  e.g., e.g., "Z:\CPD Lab Studies\ACED\03_Data_and_Syntax\05_ERP_Data\02_Cognitive_EFFrT\01_Behavioural_Data\01_Data\01_Baseline\"
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
