# Biodiversity mapping

This pipeline is dedicated to the production of biodiversity mapping and associated metrics for Norway. It imports, processes, and models species 
data from open-source data repositories, including GBIF and ANO. Currently the pipeline is in a draft format, producing species maps across four 
different taxa in the county of Trøndelag in central Norway. The current end product, the Open Data Biodiversity Mapper (ODBM) can be viewed 
[**at this link**](https://swp-data-projects.shinyapps.io/hotspotMaps2/).

## Pipeline structure

When the pipeline is running at an acceptable scope or level a more detailed instruction guide will be provided, however for the moment the description below serves as a brief overview of the pipeline structure. A visualisation of the workflow can be found [**at this Miro board**](https://miro.com/app/board/uXjVMCkk6YI=/).

To run the pipeline the following files need to be supplied in the data/external folder. Once you have cloned to repository to your
local environment you can simply edit the current versions of these files that already reside there.

- focalSpecies.csv - A list of species. Follow the format of the example provided, making sure you use the accepted scientific name as 
listed in GBIF (if you're unsure of the accepted scientific name, [**GBIF has a tool for this purpose**](https://www.gbif.org/tools/species-lookup))
- metadataSummary.csv - A list of datasets to be used, along with their GBIF dataset codes and the data type, plus a TRUE/FALSE column 
stating whether they should be used or not. Check the [**FAQ section of the ODBM**](https://swp-data-projects.shinyapps.io/hotspotMaps2/) for
information on finding GBIF dataset codes.

You'll also need to edit the focalCovariates.csv folder, however this is simply a matter of deciding which
covariates you'd like to use by editing the TRUE/FALSE column.

The following 6 scripts can all be run through the masterScript.R script at the head of the repository, and there are detailed comments
throughout each script to help the reader. However a brief technical description of each script is contained below.

### [speciesImport.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/import/speciesImport.R)

This is the starting point for the entire pipeline. There are a few inputs that should be defined at the top of the script before running (
if not defined they will simply use Trøndelag count in central Norway as a default region). These are necessary imports for the defineRegion.R util 
script, which defines the spatial region we are looking at during the pipeline run. 

- *level* - This defines the spatial level for which the pipeline will run. Options are 'municipality', 'county', 'country' or 'points'.
- *region* - Defines the region. If 'municipality' or 'county', use Norwegian standard numerical codes to define the region 
([**found here**](https://kartverket.no/til-lands/kommunereform/tekniske-endringer-ved-sammenslaing-og-grensejustering/komendr2020)).

Thereafter the script imports data from GBIF using the spatial region defined above. It also imports necessary data from the ANO database. The
output is a list format, with each list item being data from a specific dataset. A metadata summary file is also produced, which gives metadata on 
each dataset used. The script then delivers the relevant species, geometry and occurrence data to the visualisation location which will later
be used to create the necessary maps.

Finally, the script downloads photos of each individual species from iNaturalist, as well as an image URL and the user name of the individual
who took the photo. This is later used in the ODBM to provide extra species info.

**Outputs**: Full species data list (sf class), regional geometry file (sf class), png files of each species.

### [environmentalImport.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/import/environmentalImport.R)

The environmentalImport.R script is then used to import environmental data across the same region. The focalCovariates.csv file defines which 
covariates we are using in the model. The environmental data has been provided by Dr. Ron Togunov. Data is imported as a raster class at 1 x 1km
resolution Further information on the data sources used can be found in the 
[**external/data/environmentalCovariates**](https://github.com/gjearevoll/BioDivMapping/tree/main/data/external/environmentalCovariates) description.

**Outputs**: List of environmental datasets (raster class).

### [speciesProcessing.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/processing/speciesDataProcessing.R)

Since some of the databases are listed as presence only but are in fact presence absence, there is additional processing required. This 
script takes those datasets and checks which species were surveyed for, by grabbing the endpoint URLs from GBIF's metadata and downloading
the datasets directly from the source. If those species do not show up at a location in an event, they are considered as
absences at that location.

Additionally, since this script provides the final dataset which will be used in the modelling, there is an option in this script to upload the 
dataset to the project's local dataset, Wallace. This is set to FALSE as a default for now.

**Outputs**: List of processed datasets (sf).

### [speciesModelRuns.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/models/speciesModelRuns.R)

The speciesModelRuns.R data scripts are then used to coalesce the data into a format so that it can be inputted into Philip Mosert's
[**intSDM package**](https://github.com/PhilipMostert/intSDM). The utils script modelPreparation.R also plays a large role here, merging the species
and environmental data into an R6 Environment object. Model calculation is by far the most computationally intensive aspect of the pipeline,
and will vary in its comuptation time depending on the size of the region surveyed, the number of species occurrences, the 

**Important**: The properties of the INLA Mesh need to be adjusted whenever the region is changed. You can read more about an INLA Mesh and how to 
construct a good one in our [**FAQ section of ODBM**](https://swp-data-projects.shinyapps.io/hotspotMaps2/). As presented in the 
master script, you can also construct a mesh with a bit of trial and error using the utils/meshTest.R script.

**Outputs**: Each species run through the model gets its own folder with a map and set of predictions for the entire region.

### [biodiversityMetricEstimation.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/processing/biodiversityMetricEstimation.R)

The biodiversityMetricEstimation.R script is then used to aggregate the results of the integrated species models to a biodiversity 
metric. This, along with the results from individual species models, is saved in the data folder, as well as directly into the
visualisation folder.

### [visualisation/hotspotMaps](https://github.com/gjearevoll/BioDivMapping/tree/main/visualisation/hotspotMaps)

The visualisation/hotspotMap folder contains a shiny app which shows species occurrence data, modelled species intensity data, and
biodiversity metrics. The output can be previewed in the 
[**Taxa biodiversity tab of the ODBM**](https://swp-data-projects.shinyapps.io/hotspotMaps2/).

## Folder structure

- data
  + external - This is a temporary repository for any data we currently have stored locally, which will in the future be accessed externally (it. GBIF data, environmental data downloaded from external servers)
  + temp - Another temporary data folder, this is where all data will be stored that would normally be uploaded to our server and then accessed again later in the pipeline
  + run_xxxx-xx-xx - Model output data for each species group. This folder is created automatically when models are initiated.
- pipeline - this folder contains all scripts which need to be run anually from the command line
  + imports - contains the scripts necessary for importing species and environmental data
  + models - contains scripts relevant to running integrated species distribution models
  + processing - contains scripts that process data for input or output
- utils - contains scripts which perform action that do not require manual inputs. Will be turned into functions in the future.
- visualisations - contains the shiny app (hotspotMaps) which visualises biodiversity, species intensities and species occurrences
