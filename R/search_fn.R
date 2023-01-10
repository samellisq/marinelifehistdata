#' Filter subsets of marine.lifehist.data
#'
#' Function to extract subsets of \code{\link{marine.lifehist.data}} easily.
#'
#' @param data.type "age-structure" (default), "corpora" or "pregnancy"
#' @param species vector of species names, as common name in camel-case with no spacies.
#' If \code{NULL} (default) then all species are returned
#' @param sex "F" or "M", character, indiciating sexes of datasets return if
#' \code{NULL} (default) both sexes are returned.
#' @param return.key if \code{FALSE} (default) then only datasets are returned. Else
#' if \code{TRUE} then a filtereded data key is also returned.
#'
#' @return
#' if \code{return.key = FALSE} then a list of datasets. A subset of \code{\link{marine.lifehist.data}}
#' matching the input conditions. List element names reflect dataset number
#'
#' if \code{return.key = TRUE} then a two element list. Element 1 \code{$key} is a filtered
#' version of  code{\link{marine.lifehist.datakey}} describing the datasets matrching the input conditions.
#' Element 2 \code{data} is a list of datasets.  A subset of \code{\link{marine.lifehist.data}}
#' matching the input conditions. List element names reflect dataset number
#' #'
#'
#' @details
#' All data are present in \code{\link{marine.lifehist.data}}and \code{\link{marine.lifehist.datakey}}
#' this function simply provides a simple way to access subsets of the data. See
#' \code{?marine.lifehist.data} & \code{?marine.lifehist.datakey} for full description
#' of column headings and contents.
#'
#' @examples
#' get_lifehist_data()
#' get_lifehist_data(species = "KillerWhale", sex = "F")
#' get_lifehist_data(species = c("KillerWhale", "ShortFinnedPilotWhale"))
#'
#' @export
get_lifehist_data = function(data.type = "age-structure", species = NULL, sex = NULL, return.key = FALSE){
  if(!is.null(species)){
    for(i in 1:length(species)){
      if(!(species[i] %in% marine.lifehist.datakey$species)){
        warning(paste(species[i], "is not present in the data. Check naming, spelling and formating (camel-case, no spaces).
                      For a full list of species with data (and naming) see marine.lifehist.datakey$species"))
      }
    }
  }

  if(is.null(species)){
    species = marine.lifehist.datakey$species
  }

  if(is.null(sex)){
    sex = c("F", "M")
  }

  species.i = species
  sex.i = sex

  key.df = dplyr::filter(marine.lifehist.datakey, data %in% data.type, species %in% species.i, sex %in% sex.i)
  print(key.df)

  data.list = marine.lifehist.data[key.df$dataset]

  if(!return.key){
    return(data.list)
  } else {
    return(list(key = key.df, datasets = data.list))
  }


}


