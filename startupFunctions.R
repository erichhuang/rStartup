## startupFunctions.R

## Erich S. Huang
## erich@post.harvard.edu

## STARTUP FUNCTIONS

# A function to use a Synapse query to get the names and IDs of objects within a Synapse folder
folderContents <- function(parentId){
  require(synapseClient)
  queryString <- sprintf("SELECT id, name FROM entity WHERE entity.parentId == '%s'", parentId)
  queryObj <- synapseQuery(queryString)
}

# A function to conveniently push objects from the R workspace up to Synapse
synPut <- function(object, parentId = NULL){
  require(synapseClient)
  if(is.null(parentId)){
    stop('You must provide the Synapse ID of a parent Synapse Folder or Project\n')
  } else {
    entName <- readline('Please enter the name for your new Entity: \n')
    cat(sprintf('Creating %s\n', entName))
    synEnt <- Data(list(name = entName, parentId = parentId))
    synEnt <- createEntity(synEnt)
    synEnt <- addObject(synEnt, object)
    cat(sprintf('Storing %s\n', entName))
    synEnt <- storeEntity(synEnt)
    cat(sprintf('Completed creation and storage of %s\n', entName))
    return(synEnt)
  }
}

# A function to do the same as above but from a list of objects
synPutList <- function(listObject, parentId = NULL){
  require(synapseClient)
  if(is.null(parentId)){
    stop('You must provide the Synapse ID of a parent Synapse Folder or Project\n')
  } else {
    cat('Note that the Entity names will take the name of the objects\n')
    objectNames <- names(listObject)
    if(is.null(objectNames)){
      stop('Each list slot must have a name')
    } else {
      for(i in 1:length(listObject)){
        iObject <- listObject[[i]]
        iName <- objectNames[i]
        iEnt <- Data(list(name = iName), parentId = parentId)
        iEnt <- createEntity(iEnt)
        iEnt <- addObject(iEnt, iObject, name = iName)
        cat(sprintf('Storing %s\n', iName))
        iEnt <- storeEntity(iEnt)
      }
    }
  }
}

# A function to ease defining multiple 'generatedBy' relationships
generatedByList <- function(entityList, activity){
  require(synapseClient)
  activityID <- activity@properties@properties@stringAnnotations$id
  for(i in 1:length(entityList)){
    cat('Getting updated Activity information\n')
    activity <- getActivity(activityID)
    cat(sprintf('Defining provenance for File %s\n', i))
    iEnt <- entityList[[i]]
    generatedBy(iEnt) <- activity
    iEnt <- updateEntity(iEnt)
  }
}

# # A function to read TCGA PanCancer files into memory
# readToWorkspace <- function(synapseEntity){
#   
#   # Apparently, with these entities the cacheDir depth is one 
#   # directory too deep, hence
#   cachePath <- synapseEntity$cacheDir
#   pathElements <- strsplit(cachePath, '/')
#   pathLength <- length(pathElements[[1]]) - 1
#   newPathElements <- pathElements[[1]][1:pathLength]
#   filePath <- paste(newPathElements, collapse = '/')
#   fullFilePath <- list.files(filePath, full.names = T)
#   excludeVec <- rep(NA, 2)
#   excludeVec[1] <- grep('files.json', fullFilePath)
#   excludeVec[2] <- grep('_unpacked', fullFilePath)
#   fullFilePath <- fullFilePath[-excludeVec]
#   
#   # read the first column for rownames
#   rowNames <- read.table(pipe(paste('cut -f1 ', fullFilePath, sep = '')))
#   colNames <- read.table(pipe(paste('head -1 ', fullFilePath, sep = '')))
#   strippedTable <- read.table(fullFilePath)
# }

