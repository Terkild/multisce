#' Save individual reducedDim
#'
#' @param reducedDimName  Name of reducedDim to be saved
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#'
#' @export
reduceddim_save <- function(reducedDimName, sce, path, folder="reducedDim"){
  current_dimred <- reducedDim(sce, reducedDimName)

  multisce_individual_save(current_dimred, path=file.path(path, folder), filename=reducedDimName)
}

#' Load individual reducedDim
#'
#' @param reducedDimName  Name of reducedDim to be loaded
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#'
#' @export
reduceddim_load <- function(reducedDimName, path, folder="reducedDim"){
  current_dimred <- multisce_individual_load(path=file.path(path, folder), filename=reducedDimName)

  return(current_dimred)
}

#' List available reducedDims
#'
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#'
#' @export
reduceddim_list <- function(path, folder="reducedDim", extension=".rds"){

  reduceddim_names <- gsub(paste0(extension,"$"),"", list.files(path=file.path(path, folder), pattern=extension, full.names=FALSE))
  return(reduceddim_names)
}
