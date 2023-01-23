#' Key to data listed in marine.lifehist.data
#'
#' Data key to marine.lifehist.data
#'
#' @format  `marine.lifehist.datakey`
#' A data frame with 312 rows and 9 columns. Each row corresponds to an item in
#' \code{\link{marine.lifehist.data}}
#' \describe{
#'   \item{dataset}{numbered dataset id}
#'   \item{species}{species name: camel-case common name with no spaces. For other names see \code{\link{marine.lifehist.speciesdata}}}
#'   \item{sex}{sex of whales in dataset}
#'   \item{reference}{reference id for data origin. see \code{\link{bibliography}} for full reference list}
#'   \item{data source}{whaling, mass-stranding, bycatch etc.}
#'   \item{from} {table, barplot, scatterplot etc.}
#'   \item{data} {Data type, currently: age-structure, corpora or pregnancy}
#' }
"marine.lifehist.datakey"


#'Life history data from marine mammals
#'
#' List of life history data extracted from the literature for marine mammals.
#' Details of data types, species, and references are in \code{\link{marine.lifehist.datakey}}
#'
#' @format  `marine.lifehist.data`
#' A list of length 312. Where each list item is a marine life history dataset. Each
#' list item is a data frame with size and format depending on data type.
#'
#' For \strong{age-structure data}. A data frame with two columns, where each row represents a single animal:
#' \describe{
#' \item{age}{age of animal in years}
#' \item{dataset}{dataset id, see \code{\link{marine.lifehist.datakey}} }
#' }
#'
#' For \strong{corpora data}. A data frame with three columns, where each row represents a single animal:
#' \describe{
#' \item{age}{age of animal in years}
#' \item{corpora}{number of corpora reported from the ovary. this includes all types of corpora reported}
#' \item{dataset}{dataset id, \code{\link{marine.lifehist.datakey}} }
#' }
#'
#' For \strong{pregnancy data}. A data frame with three columns, where age row represents an age in the sample:
#' \describe{
#' \item{age}{age in years}
#' \item{pregnancy.rate}{proportion of females of a given age pregnant in sample}
#' \item{dataset}{dataset id, \code{\link{marine.lifehist.datakey}} }
#' }
"marine.lifehist.data"

#' Extra information on datasets included in marine.lifehist.data
#'
#' Some further information on datasets in \code{\link{marine.lifehist.data}} necessarry
#' for some types of analysis
#'
#' @format `marine.lifehist.datasetextras`
#' A list of length 2. Each item is a data frame describing different species level data.
#' List items are ordered and named:
#'
#' \enumerate{
#' \item{\emph{datasetsextras_bias.details}} data frame indicating known sources of bias (metric) and their (value) needed for survival calculations
#' \item{\emph{datasetsextras_populations.key}} data frame showing which datasets are taken from studies of the same population (pop.identifier)
#' }
#'
"marine.lifehist.datasetextras"



#' Species level data for Marine Mammals
#'
#' List of different species level data for marine mammals.
#'
#' @format `marine.lifehist.speciesdata`
#' A list of length 2. Each item is a data frame describing different species level data.
#' List items are ordered and named:
#'
#' \enumerate{
#' \item{\emph{species_names.key}} data frame with species names and alternatives, as per International Whaling Commission Sept 2021
#' \item{\emph{species_age.maturity}} data frame with species ages at maturity for each sex of each species. NAs indicate no data found
#' \item{\emph{species_raw.sizes}} data frame raw size data extracted from the literature. measuremtn = length/mass/girth. Type = type of measure reported. n = sample size (Where reported), and if estimated = 1 authros refer to the values reported as estimates.
#' \item{\emph{species_length}} data frame with processed length data for each species-sex. Methods in Ellis et al 2023. All derived from species_raw.sizes.
#' \item{\emph{species_mass}} data frame with processed mass data for each species-sex. Methods as in for length outlined in Ellis et al 2023. All derived from species_raw.sizes.
#' }
#'
"marine.lifehist.speciesdata"


#' Bibliography
#'
#' Reference list for marine mammal life history data
#'
#' \describe{
#' \item{reference} {reference id}
#' \item{reference.text} {reference text}
#' }
"bibliography"
