#' Save individual reducedDim
#'
#' @param reducedDimName  Name of reducedDim to be saved
#' @param reduceddim reducedDim object
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#'
#' @export
reduceddim_save <- function(reducedDimName, reduceddim, path, folder="reducedDim"){

  multisce_individual_save(reduceddim, path=file.path(path, folder), filename=reducedDimName)
}

#' Load individual reducedDim
#'
#' @param reducedDimName  Name of reducedDim to be loaded
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#'
#' @export
reduceddim_load <- function(reducedDimName, path, folder="reducedDim"){
  reduceddim <- multisce_individual_load(path=file.path(path, folder), filename=reducedDimName)

  return(reduceddim)
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
