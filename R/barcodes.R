#' Save barcode data
#'
#' Saves barcodes from SingleCellExperiment as separate file
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param filename Name of resulting tsv file
#'
#' @return DataFrame containing only the columns with the indicated prefix
#'
#' @export
barcodes_save <- function(sce, path=multisce_path(sce), filename="barcodes.tsv", overwrite=TRUE){
  file_bc <- file.path(path, filename)

  if(!barcodes_exists(file_bc) | overwrite==TRUE){

    write.table(colnames(sce), file=file_bc, col.names=FALSE, row.names=FALSE)

    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Load barcode data
#'
#' Load barcodes from tsv file
#'
#' @param path Path to multisce folder for the object
#' @param filename Name of resulting tsv file
#' @param missing_ignore Booling indicating if missing barcode file should be ignored (if so, returns empty vector instead of error)
#'
#' @return Vector of barcodes from tsv file
#'
#' @export
barcodes_load <- function(path, filename="barcodes.tsv", missing_ignore=FALSE){
  file_bc <- file.path(path, filename)

  if(barcodes_exists(path=path, filename=filename) == TRUE){

    barcodes <- read.table(file=file_bc, header=FALSE, sep="\t")[[1]]

    return(barcodes)
  } else {
    if(missing_ignore == TRUE){
      return(c())
    } else {
      stop("Barcodes file does not exist")
    }
  }
}

#' Check if barcodes file exists
#'
#' Checks if barcodes file exists
#'
#' @param path Path to multisce folder for the object
#' @param filename Name of resulting tsv file
#'
#' @return Boolean of whether barcode file exists
#'
#' @export
barcodes_exists <- function(path, filename="barcodes.tsv"){
  file_bc <- file.path(path, filename)

  return(file.exists(file_bc))
}


#' Check barcode data
#'
#' Checks if barcodes in SingleCellExperiment object matches the saved barcodes file
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param bc_filename Name of barcodes tsv file
#'
#' @return Boolean of whether barcodes in tsv file matches colnames in SingleCellExperiment object
#'
#' @export
barcodes_check <- function(sce, path=multisce_path(sce), bc_filename="barcodes.tsv"){
  bc <- barcodes_load(path=path, filename=bc_filename)

  if(identical(colnames(sce), bc) == TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
