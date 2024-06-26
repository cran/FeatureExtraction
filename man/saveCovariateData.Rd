% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CovariateData.R
\name{saveCovariateData}
\alias{saveCovariateData}
\title{Save the covariate data to folder}
\usage{
saveCovariateData(covariateData, file)
}
\arguments{
\item{covariateData}{An object of type \code{covariateData} as generated using
\code{getDbCovariateData}.}

\item{file}{The name of the folder where the data will be written. The folder should not
yet exist.}
}
\value{
No return value, called for side effects.
}
\description{
\code{saveCovariateData} saves an object of type covariateData to folder.
}
\details{
The data will be written to a set of files in the folder specified by the user.
}
\examples{
\donttest{
covariateData <- FeatureExtraction::createEmptyCovariateData(
  cohortIds = 1,
  aggregated = FALSE,
  temporal = FALSE
)
# For this example we'll use a temporary file location:
fileName <- tempfile()
saveCovariateData(covariateData = covariateData, file = fileName)
# Cleaning up the file used in this example:
unlink(fileName)
}

}
