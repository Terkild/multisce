#' Save individual reducedDim
#'
#' @param reducedDimName  Name of reducedDim to be saved
#' @param reduceddim reducedDim object
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#' @param barcodes_file Name of file to save barcodes (default: barcodes.tsv)
#' @param barcodes_overwrite Should barcodes file be overwritten if existing - may break colname relationship to objects not currently loaded in the multisce
#'
#' @export
reduceddim_save <- function(reducedDimName, reduceddim, path, folder="reducedDim", barcodes_file="barcodes.tsv", barcodes_overwrite=FALSE){

  if(barcodes_exits(path=path, filename=barcodes_file) & barcodes_overwrite == FALSE){

    # Check of SCE barcodes matches barcodes file
    if(barcodes_load(path=path, filename=barcodes_file) != rownames(reduceddim)){
      stop(paste("Barcodes of ReducedDim (",reducedDimName,") does not match saved barcodes file - set barcode_overwrite=TRUE to ignore this (Obs. this may break associations with other linked objects)"))
    }
  }

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
