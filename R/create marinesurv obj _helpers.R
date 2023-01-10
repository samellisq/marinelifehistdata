### from translating data extras fns.R  on 2022/11/14

########Sampling Biases

#' Helper Function to sort sampling bias data
get_biasmat = function(datasetskey.df, extras.df, nages = 100){
  biasextras.df = dplyr::filter(extras.df, metric == "biasage" | metric == "biasdirections")

  biasmat.list = list()
  for(i in 1:max(datasetskey.df$dataset.num)){
    DATASET = datasetskey.df$dataset[i]

    if(DATASET %in% biasextras.df$dataset){

      age.mat = datasetskey.df$age.mat[i]

      # biasdirect = filter(biasextras.df, dataset == DATASET & metric == "biasdirection")$value

      biasages.raw = dplyr::filter(biasextras.df, dataset == DATASET & metric == "biasage")$value
      biasages.raw = stringr::str_to_lower(biasages.raw)

      # if(length(biasdirect) <1 | length(biasages.raw) <1){
      #   warning(paste("Missing info for dataset", DATASET, sep = " "))
      # }

      if(stringr::str_detect(biasages.raw, "to")){
        biasages = stringr::str_split(biasages.raw, "to", simplify = TRUE)
        window.min = as.numeric(biasages[,1])
        window.max = as.numeric(biasages[,2])
      }

      if(stringr::str_detect(biasages.raw, ">")) {
        biasages = stringr::str_remove_all(biasages.raw, ">")
        window.min = as.numeric(biasages)
        window.max = (nages-1) + age.mat
      }

      if(stringr::str_detect(biasages.raw, "<")) {
        biasages = as.numeric(stringr::str_remove_all(biasages.raw, "<"))
        window.min = age.mat
        window.max = as.numeric(biasages)
      }

      if(stringr::str_length(biasages.raw) == 1){
        window.min = as.numeric(biasages.raw)
        window.max = as.numeric(biasages.raw)
      }

      window.min = window.min - age.mat
      window.max = window.max - age.mat

      s.vector = ifelse(0:(nages-1) >= window.min & 0:(nages-1) <= window.max, 1 ,0)

    } else {
      s.vector = rep.int(0,nages)
    }

    biasmat.list[[i]] = s.vector
  }

  biasmat = do.call(cbind, biasmat.list)
  return(biasmat)
}

#' Helper Function to sort sampling bias data
get_bias.inclusion.vector = function(biasmat){
  include = colSums(biasmat) # no point in including when not actually used
  include = ifelse(include ==0, 0, 1)
  return(include)

}

#' Helper Function to sort sampling bias data
get_bias.direction.vector = function(datasetskey.df, extras.df){
  biasextras.df = dplyr::filter(extras.df, metric == "biasdirection")
  bias.direction = rep(0, max(datasetskey.df$dataset.num))

  for(i in 1:length(bias.direction)){
    if(datasetskey.df$dataset[i] %in% biasextras.df$dataset){
      bias.direction[i] = as.numeric(dplyr::filter(biasextras.df, dataset == datasetskey.df$dataset[i] & metric == "biasdirection")$value)

      if(!(bias.direction[i] == 1 |bias.direction[i] == -1 )){
        warning(paste("somthing odd is going on with the direction for dataset ", datasetskey.df$dataset[i], sep = ""))
      }

    }
  }
  return(bias.direction)
}


############## Population Growth
#' Helper Function to sort sampling bias data
get_popchange.vector = function(datasetskey.df, rawdatakey.df, extras.df){
  data.sources =
    rawdatakey.df %>%
    dplyr::filter(dataset %in% datasetskey.df$dataset) %>%
    dplyr::select(dataset, datasource = `data source`)

  whaling.terms = unique(data.sources$datasource[stringr::str_detect(data.sources$datasource, "whaling")])

  assume.decrease = c("bycatch", whaling.terms)

  assumed.change = ifelse(data.sources$datasource %in% assume.decrease, -1, 0)

  additional.info = dplyr::filter(extras.df, metric == "popchange")
  if(any(!(additional.info$value == -1 |additional.info$value == 0 |additional.info$value == 1) )){
    warning("something has gone wrong with popchange in dataset_extras")
  }

  popchange.vector = assumed.change
  for(i in 1:length(popchange.vector)){
    if(datasetskey.df$dataset[i] %in% additional.info){
      popchange.vector[i] = dplyr::filter(additional.info, dataset == datasetskey.df$dataset[i])$value
    }
  }

  #to go back to by dataset just comment this out and return popchange.vector
  n.pops = max(datasetskey.df$pop.num)
  popchange.vector.bypop = numeric(n.pops)
  linked.df = data.frame(datasetskey.df, popchange.vector)
  for(i in 1:n.pops){
    pop.df = dplyr::filter(linked.df, pop.num == i)
    change = unique(pop.df$popchange.vector)
    if(length(change)>1){print(pop.df)}
    popchange.vector.bypop[i] = ifelse(length(change) ==1,
                                       change,
                                       0 # because if there is any disagreement if is safest to assume nothing
    )
  }

  return(popchange.vector.bypop)
}


