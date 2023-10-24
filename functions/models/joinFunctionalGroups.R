# combine species modelled as functional groups (FGs)
# by changing their scientific name to their FG name

joinFunctionalGroups <- function(speciesData, focalSpecies) {
  # remove duplicate species in same FG 
  focalSpecies <- unique(focalSpecies[,c("species", "taxonomicGroup", "functionalGroup")])
  # identify any species in multiple groups
  instances <- table(focalSpecies$species)
  multiFG <- names(instances[instances > 1])
  # check if list of datasets
  if(inherits(speciesData, "sf") && !inherits(speciesData, "list")){
    unlist <- TRUE
    speciesData <- list(data = speciesData)
  } else {
    unlist <- FALSE
  }
  # for each dataset
  speciesData <- lapply(speciesData, function(ds) {
    # convert species name to character
    ds$simpleScientificName <- as.character(ds$simpleScientificName)
    ds$species <- ds$simpleScientificName# make copy of original species names
    # identify records of species part of one functional group
    # (typically generalists, or if only distinct specialists)
    singleFG <- ds$simpleScientificName %in% names(instances[instances == 1]) &
      focalSpecies$functionalGroup[  # functional group not blank 
        match(ds$simpleScientificName, focalSpecies$species)] != ""  
    # rename species part of one functional group 
    ds$simpleScientificName[singleFG] <-
      focalSpecies$functionalGroup[match(ds$simpleScientificName[singleFG], 
                                         focalSpecies$species)]
    # return ds if no species in taxa have multiple FGs
    if (length(multiFG) == 0) {
      return(ds)
    }
    # otherwise, identify any species in dataset used in multiple FGs
    # (typically specialists nested in model for generalists, if present, 
    # or species modelled in group and individually)
    multiFG.ds <- multiFG[multiFG %in% unique(ds$simpleScientificName)]
    # return ds if no species in dataset are used in multiple FGs
    if (length(multiFG.ds) == 0) {
      return(ds)
    }
    # otherwise, loop through each multi FG species to duplicate records and change species name
    ds.multiFG <- do.call(rbind, lapply(multiFG.ds, function(sp) {
      # identify rows to be replicated
      species_rows <- ds[ds$simpleScientificName == sp, ] 
      # define FG replacement names, using species for blank groups 
      fg <- focalSpecies[focalSpecies$species == sp, ]
      fg <- ifelse(fg$functionalGroup == "", fg$species, fg$functionalGroup)
      
      # Replicate rows for species with multiple FGs
      replicated_rows <- do.call(rbind, replicate(length(fg), species_rows, simplify = FALSE))
      replicated_rows$simpleScientificName <- rep(fg, each = nrow(species_rows))  # assign name
      replicated_rows 
    }))
    # combine with remaining data (i.e., species modelled separately or in one FG)
    return(rbind(ds.multiFG, ds[!(ds$simpleScientificName %in% multiFG.ds), ]))
  })
  # unlist if data was sf
  if(unlist){
    speciesData <- speciesData[[1]]
  }

  return(speciesData)
}

