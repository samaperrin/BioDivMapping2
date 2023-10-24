
#' @importFrom terra nlyr
#' @importFrom sf st_sf
#' @importFrom intSDM startWorkflow

#### MODEL DATA PREPARATION ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

modelPreparation <- function(focalSpecies, speciesData, regionGeometry, modelFolderName, environmentalDataList = NULL) {
  focalTaxa <- unique(focalSpecies$taxonomicGroup)
  
  workflowList <- list()
  
  # Begin running different species groups
  for (i in seq_along(focalTaxa)) {
    
    # Define species group to create
    focalGroup <- focalTaxa[i]
    focalGroupSpecies <- focalSpecies$species[focalSpecies$taxonomicGroup %in% focalGroup]
    
    focalSpeciesData <- speciesData
    
    # We need to remove all unnecessary species datasets from the species data
    focalSpeciesDataRefined <- lapply(focalSpeciesData, FUN = function(x) {
      focalDataset <- x[x$simpleScientificName %in% focalGroupSpecies,]
      if (nrow(focalDataset) == 0) {
        focalDataset <- NA
      }
      focalDataset
    })
    focalSpeciesDataRefined <- focalSpeciesDataRefined[!is.na(focalSpeciesDataRefined)]
    
    # Combine species by functionalGroup if requested (else leave as separate)
    # identify species with data
    uniqueTaxaSpecies <- unique(unlist(lapply(focalSpeciesDataRefined, function(ds){
        as.character(ds$simpleScientificName)
      })))
    
    # functional groups in species with data for focal taxonomic group
    # focalFunctionalGroups <- focalSpecies$functionalGroup[focalSpecies$species %in% uniqueTaxaSpecies &
    #                                                       focalSpecies$taxonomicGroup %in% focalGroup]
    focalSpeciesWithData <- focalSpecies[focalSpecies$species %in% uniqueTaxaSpecies &  # species with data
                                         focalSpecies$taxonomicGroup %in% focalGroup,]  # and of focal taxa (in case same species in different taxa)
    # if any species are to be modelled as functional groups
    if(any(focalSpeciesWithData$functionalGroup != "")){
      # update data
      focalSpeciesDataRefined <- joinFunctionalGroups(speciesData = focalSpeciesDataRefined, 
                           focalSpecies = focalSpeciesWithData)
                           
      focalGroupSpecies <- unique(unlist(lapply(focalSpeciesDataRefined, function(ds){
          as.character(ds$simpleScientificName)
        })))
    }

    # Initialise workflow, creating folder for model result storage
    workflow <- startWorkflow(
      Projection = '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
      Species = focalGroupSpecies,
      saveOptions = list(projectDirectory = modelFolderName, projectName =  focalGroup), Save = TRUE
    )
    workflow$addArea(Object = st_sf(regionGeometry))
    
    # Add datasets - note that for the moment this excludes the NTNU field notes and ANO,
    # the model will currently not run with these involved
    for (l in seq_along(focalSpeciesDataRefined)) {
      dataset <- focalSpeciesDataRefined[[l]]
      
      if (nrow(dataset) < 5) next
      
      dataType <- unique(dataset$dataType)
      datasetName <- gsub(" ", "", gsub("[[:punct:]]", "", names(focalSpeciesDataRefined)[l]))
      
      workflow$addStructured(dataStructured = dataset,
                             datasetType = dataType,
                             datasetName = datasetName,
                             responseName = 'individualCount',
                             speciesName = 'simpleScientificName')
      cat(sprintf("    - %s species added. \n", length(unique(dataset$simpleScientificName))))
    }
    
    # Add environmental characteristics
    env <- if(is.null(environmentalDataList)) 0 else seq(nlyr(environmentalDataList))
    for (e in env) {
      cat(sprintf("Adding covariate '%s' to the model.\n", names(environmentalDataList)[e]))
      workflow$addCovariates(Object = environmentalDataList[[e]])
    }
    
    workflowList[[focalGroup]] <- workflow
    
  }
  return(workflowList)
}
