#' Function converting  marine.lifehist.data to input for a marine survival model
#'
#'@export
create_marinesurvival_modinput = function(datasets.list, min.sample.size = 0){

  datasets.key = dplyr::filter(marine.lifehist.datakey, dataset %in% names(datasets.list))

  ### Remove datasets with no samples> age at amturity
  datasets.key =
    dplyr::left_join(datasets.key,
                     marine.lifehist.speciesdata$species_age.maturity %>%
                       dplyr::select(species, sex, age.mat)
    )
  new.datasets.list = list()
  name.keep = rep("TRUE", length(datasets.list))
  for(i in 1:length(datasets.list)){
    agemati = dplyr::filter(datasets.key, dataset == names(datasets.list[i]))$age.mat
    dataseti = datasets.list[[i]]
    filtered.dataset = dplyr::filter(dataseti, age> agemati)
    if(nrow(filtered.dataset) > 0){
      new.datasets.list[[i]] = filtered.dataset
    } else {
      new.datasets.list[[i]] = NULL
      name.keep[i] = FALSE
      warning(paste("Dataset ", datasets.list[[i]]$dataset[1], " dropped as contains no adult samples", sep =""))
    }
  }
  names(new.datasets.list) = names(datasets.list)[name.keep== TRUE]
  datasets.list = purrr::compact(new.datasets.list)
  datasets.key = dplyr::filter(datasets.key, dataset %in% names(datasets.list))



  ## NUMBER DATASETS AND SPECIES
  # datasets.key$dataset.num = seq(1,nrow(datasets.key), 1)
  datasets.key$species.sex = paste(datasets.key$species, datasets.key$sex, sep = ".")
  species.key =
    datasets.key %>%
    dplyr::arrange(species.sex) %>%
    dplyr::select(species, species.sex, sex) %>%
    dplyr::distinct()
  species.key$species.num = seq(1, nrow(species.key), 1)
  datasets.key = dplyr::left_join(datasets.key, species.key)



  ## CREATE AND APPLY POPULATION KEY
  if(nrow(dplyr::filter(datasets.key, !(dataset %in% marine.lifehist.datasetextras$datasetsextras_populations.key$dataset))) > 0){

    population.key=
      datasets.key %>%
      dplyr::filter(!(dataset %in% marine.lifehist.datasetextras$datasetsextras_populations.key$dataset)) %>%
      dplyr::left_join(dplyr::select(marine.lifehist.datakey, dataset, reference)) %>%
      dplyr::group_by( reference, species) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pop.num = seq(1:dplyr::n())) %>%
      dplyr::right_join(
        datasets.key %>%
          dplyr::filter(!(dataset %in% marine.lifehist.datasetextras$datasetsextras_populations.key$dataset)) %>%
          dplyr::select(dataset) %>%
          dplyr::left_join(dplyr::select(marine.lifehist.datakey, species, dataset, reference)),
        multiple = "all"
      )%>%
      dplyr::arrange(dataset) %>%
      dplyr::select(dataset, pop.num)

  } else(population.key = data.frame(dataset = numeric(), pop.num = numeric()))
  if(nrow(dplyr::filter(marine.lifehist.datasetextras$datasetsextras_populations.key,dataset %in% datasets.key$dataset))>0){

    population.key =
      dplyr::bind_rows(population.key,
                       marine.lifehist.datasetextras$datasetsextras_populations.key %>%
                         dplyr::filter(dataset %in% datasets.key$dataset) %>%
                         dplyr::mutate(pop.identifier.recount = pop.identifier - min(pop.identifier, na.rm = TRUE) +1) %>%
                         dplyr::mutate(pop.num = pop.identifier.recount + max(population.key$pop.num)) %>%
                         dplyr::select(dataset, pop.num)
      ) %>%
      dplyr::arrange(dataset)
  }
  population.key$pop.num = ifelse(is.infinite(population.key$pop.num), 1, population.key$pop.num)
  datasets.key = dplyr::left_join(datasets.key, population.key)


  ##Rearrange age sample for input (output: by age, and samples long)
  # datasets.key =
  #   dplyr::left_join(datasets.key,
  #                    marine.lifehist.speciesdata$species_age.maturity %>%
  #                      dplyr::select(species, sex, age.mat)
  #   )



  ## REARRAGE AGE SAMPLES TO FORM INPUTS
  samples.list = list()
  byage.list = list()
  R = nrow(datasets.key)
  datasets.key$include = "Y"
  datasets.key$dataset.num = numeric(R)
  dataset.counter = 1
  for(i in 1:R){
    all.datasets = dplyr::bind_rows(datasets.list)
    dataset.raw = dplyr::filter(all.datasets, dataset == datasets.key$dataset[i])
    dataset.raw = dplyr::left_join(dataset.raw, datasets.key, by = "dataset")
    samples.i= data.frame(
      age = dataset.raw$age,
      age.adj = dataset.raw$age - datasets.key$age.mat[i],
      # dataset.num = dataset.raw$dataset.num,
      species.num = dataset.raw$species.num
    )
    samples.i = dplyr::filter(samples.i, age.adj >=0) # because some datasets  include juveniles

    if(nrow(samples.i) < min.sample.size){
      warning(paste("Dataset ", datasets.key$dataset[i] , " contains fewer than adult samples than the min sample size and has been removed", sep = ""))
      datasets.key$include[i] = "N"
    } else { # as long as it has enough samples

      datasets.key$dataset.num[i] = dataset.counter
      samples.i$dataset.num = dataset.counter
      dataset.counter = dataset.counter+1

      samples.list[[i]] = samples.i


      byage =
        data.frame(
          age.adj = seq(0, 99,1),
          sex = ifelse(datasets.key$sex[i] == "F", 1,2)
        )
      byage$n.dead = numeric(nrow(byage))
      for(j in 1:nrow(byage)){byage$n.dead[j] = sum(byage$age.adj[j] == samples.list[[i]]$age.adj)}
      # byage$s.vector = rep.int(0, nrow(byage))
      # byage$s.vector= ifelse(byage$age.adj >25, 0, 0) # will need to input s data here
      byage$species.num = samples.list[[i]]$species.num[1]
      byage$dataset.num = samples.list[[i]]$dataset.num[1]

      byage.list[[i]] = byage
    }

  }



  samples.df = dplyr::bind_rows(samples.list)
  byage = dplyr::bind_rows(byage.list)

  datasets.key = datasets.key %>% dplyr::filter(include == "Y") %>% dplyr::select(-include)

  #some annoying renumbering if any datasets have been removed
  if(length(unique(datasets.key$pop.num))!= max(datasets.key$pop.num)){
    pop.renumber.df = data.frame(
      pop.num = sort(unique(datasets.key$pop.num)),
      pop.renum = seq(1, length(unique(datasets.key$pop.num)), 1)
    )

    datasets.key =
      dplyr::left_join(datasets.key, pop.renumber.df) %>%
      dplyr::mutate(pop.num = pop.renum) %>%
      select(-pop.renum)

    samples.df =
      dplyr::left_join(samples.df,
                       datasets.key %>%
                         select(dataset.num, pop.num)
      )

    byage =
      dplyr::left_join(byage, datasets.key %>%
                         select(dataset.num, pop.num)
      )

  }
  if(length(datasets.key$species.num) != max(datasets.key$species.num)){
    species.renum.df = data.frame(
      species.num = sort(unique(datasets.key$species.num)),
      species.renum = seq(1, length(unique(datasets.key$species.num)), 1)
    )

    datasets.key =
      dplyr::left_join(datasets.key, species.renum.df) %>%
      dplyr::mutate(species.num = species.renum) %>%
      select(-species.renum)

    samples.df =
      dplyr::left_join(samples.df, species.renum.df) %>%
      dplyr::mutate(species.num = species.renum) %>%
      select(-species.renum)

    byage =
      dplyr::left_join(byage, species.renum.df) %>%
      dplyr::mutate(species.num = species.renum) %>%
      select(-species.renum)

  }


  ### GET MAX AGES
  maxages = numeric(max(byage$dataset.num))
  for(i in 1:max(byage$dataset.num)){
    (maxages[i] = max(samples.df$age.adj[samples.df$dataset.num == i]))
  }

  ### SORT BIAS INPUTS
  # 1. Age estimation
  ageest.inclusion.vector = rep.int(1, max(byage$dataset.num))

  #2. Sampling bias
  biasmat = get_biasmat(datasets.key, marine.lifehist.datasetextras$datasetsextras_bias.details)
  bias.direction.vector = get_bias.direction.vector(datasets.key, marine.lifehist.datasetextras$datasetsextras_bias.details)
  bias.inclusion.vector = get_bias.inclusion.vector(biasmat)

  #3. Population change
  popchange.direction = get_popchange.vector(datasets.key, marine.lifehist.datakey, marine.lifehist.datasetextras$datasetsextras_bias.details)
  popchange.inclusion.vector = rep.int(1, max(byage$dataset.num))

  ## PRIORS
  priors = marinesurvival::get_gomp_prior_shapes()


  ## PUT IT ALL TOGETHER
  mod.list = list(
    Nages = nrow(byage)/max(byage$dataset.num), # Number of ages in the data (e.g. 0-10, Nages = 11)
    Nsamples = nrow(samples.df), # Number of samples
    age = seq(0, max(byage$age.adj), 1), # All ages
    sample_ages = samples.df$age.adj, # Ages of sampled animals

    Nspecies = max(byage$species.num), # how many species in the data
    Ndatasets = max(byage$dataset.num),
    Npopulations = max(datasets.key$pop.num),
    species_vector = as.array(dplyr::distinct(dplyr::select(byage, dataset.num, species.num))$species.num), # vector indicating which species each dataset belongs to
    population_vector = as.array(datasets.key$pop.num),
    dataset_vector = samples.df$dataset.num, # dataset id of each sample. For our purposes they are all the
    species_sex_vector = as.array(dplyr::distinct(dplyr::select(byage, sex, species.num))$sex ),
    species_maxages = as.array(maxages), # Maximum possible age. Needed for some technical reasons.
    prior_a_s1 = priors$shape1_a, # Just the priors from before
    prior_a_s2 = priors$shape2_a,
    prior_b_s1 = priors$shape1_b,
    prior_b_s2 = priors$shape2_b,


    # all the following need to be 'as.array' to make it work. But it doesn;t actually change the data. just its 'type'. A necessarry quirk.
    include_age_est_error = as.array(ageest.inclusion.vector), # 0 for do not include 1 for include
    age_error_sd = ifelse(samples.df$age == 0, 0.01, samples.df$age*0.05), # error based on real rather than adjusted age

    include_samplebias_error = as.array(bias.inclusion.vector), # 0 for do not include 1 for include
    BiasMat = biasmat, # A vector showing if a particualr age is subject toa biased sample (1) or not (0).
    direction_samplebias = as.array(bias.direction.vector), # +1/-1/0. 0 for UNK/Both

    include_popchange_error = as.array(popchange.inclusion.vector), # 0 for do not include 1 for include
    direction_popchange = as.array(popchange.direction) # +1/-1/0. 0 for UNK/Both
  )
  return(mod.list)
}

