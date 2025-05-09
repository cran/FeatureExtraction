% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetDefaultCovariates.R
\name{getDbDefaultCovariateData}
\alias{getDbDefaultCovariateData}
\title{Get default covariate information from the database}
\usage{
getDbDefaultCovariateData(
  connection,
  oracleTempSchema = NULL,
  cdmDatabaseSchema,
  cohortTable = "#cohort_person",
  cohortId = -1,
  cohortIds = c(-1),
  cdmVersion = "5",
  rowIdField = "subject_id",
  covariateSettings,
  targetDatabaseSchema,
  targetCovariateTable,
  targetCovariateRefTable,
  targetAnalysisRefTable,
  aggregated = FALSE,
  minCharacterizationMean = 0,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
)
}
\arguments{
\item{connection}{A connection to the server containing the schema as created using the
\code{connect} function in the \code{DatabaseConnector} package.}

\item{oracleTempSchema}{DEPRECATED: use \code{tempEmulationSchema} instead.}

\item{cdmDatabaseSchema}{The name of the database schema that contains the OMOP CDM instance.
Requires read permissions to this database. On SQL Server, this should
specifiy both the database and the schema, so for example
'cdm_instance.dbo'.}

\item{cohortTable}{Name of the table holding the cohort for which we want to construct
covariates. If it is a temp table, the name should have a hash prefix,
e.g. '#temp_table'. If it is a non-temp table, it should include the
database schema, e.g. 'cdm_database.cohort'.}

\item{cohortId}{DEPRECATED:For which cohort ID should covariates be constructed? If set to -1,
covariates will be constructed for all cohorts in the specified cohort
table.}

\item{cohortIds}{For which cohort ID(s) should covariates be constructed? If set to c(-1),
covariates will be constructed for all cohorts in the specified cohort
table.}

\item{cdmVersion}{The version of the Common Data Model used. Currently only 
\code{cdmVersion = "5"} is supported.}

\item{rowIdField}{The name of the field in the cohort temp table that is to be used as the
row_id field in the output table. This can be especially usefull if there
is more than one period per person.}

\item{covariateSettings}{Either an object of type \code{covariateSettings} as created using one
of the createCovariate functions, or a list of such objects.}

\item{targetDatabaseSchema}{(Optional) The name of the database schema where the resulting covariates
should be stored.}

\item{targetCovariateTable}{(Optional) The name of the table where the resulting covariates will
be stored. If not provided, results will be fetched to R. The table can be
a permanent table in the \code{targetDatabaseSchema} or a temp table. If
it is a temp table, do not specify \code{targetDatabaseSchema}.}

\item{targetCovariateRefTable}{(Optional) The name of the table where the covariate reference will be stored.}

\item{targetAnalysisRefTable}{(Optional) The name of the table where the analysis reference will be stored.}

\item{aggregated}{Should aggregate statistics be computed instead of covariates per
cohort entry?}

\item{minCharacterizationMean}{The minimum mean value for binary characterization output. Values below this will be cut off from output. This
will help reduce the file size of the characterization output, but will remove information
on covariates that have very low values. The default is 0.}

\item{tempEmulationSchema}{Some database platforms like Oracle and Impala do not truly support
temp tables. To emulate temp tables, provide a schema with write
privileges where temp tables can be created.}
}
\value{
Returns an object of type \code{CovariateData}, which is an Andromeda object containing information on the baseline covariates.
Information about multiple outcomes can be captured at once for efficiency reasons. This object is
a list with the following components: \describe{ \item{covariates}{An ffdf object listing the
baseline covariates per person in the cohorts. This is done using a sparse representation:
covariates with a value of 0 are omitted to save space. The covariates object will have three
columns: rowId, covariateId, and covariateValue. The rowId is usually equal to the person_id,
unless specified otherwise in the rowIdField argument.} \item{covariateRef}{A table
describing the covariates that have been extracted.}  }. The CovariateData object will also have a \code{metaData} attribute, a list of objects with
information on how the covariateData object was constructed.
}
\description{
Constructs a large default set of covariates for one or more cohorts using data in the CDM schema.
Includes covariates for all drugs, drug classes, condition, condition classes, procedures,
observations, etc.
}
\details{
This function uses the data in the CDM to construct a large set of covariates for the provided
cohort. The cohort is assumed to be in an existing temp table with these fields: 'subject_id',
'cohort_definition_id', 'cohort_start_date'. Optionally, an extra field can be added containing the
unique identifier that will be used as rowID in the output. Typically, users don't call this
function directly but rather use the \code{\link{getDbCovariateData}} function instead.
}
\examples{
\donttest{
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "cohort"
)
connection <- DatabaseConnector::connect(connectionDetails)

results <- getDbDefaultCovariateData(
  connection = connection,
  cdmDatabaseSchema = "main",
  cohortTable = "cohort",
  covariateSettings = createDefaultCovariateSettings(),
  targetDatabaseSchema = "main",
  targetCovariateTable = "ut_cov"
)
}
}
