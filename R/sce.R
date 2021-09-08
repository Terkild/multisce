#' Save SingleCellExperiment
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param filename Name of sce
#' @param folder Subfolder within path where sce are located
sce_save <- function(sce, path, filename, folder="sce"){

  multisce_individual_save(sce, path=file.path(path, folder), filename=filename)
}

#' Load SingleCellExperiment
#'
#' @param path Path to multisce folder for the object
#' @param filename Name of sce
#' @param folder Subfolder within path where sce are located
sce_load <- function(path, filename, folder="sce"){

  multisce_individual_load(path=file.path(path, folder), filename=filename)
}

#' List available SingleCellExperiments
#'
#' @param path Path to multisce folder for the object
#' @param folder Subfolder within path where sce are located
#' @param extension File extension used for individual sce files
#'
#' @export
sce_list <- function(path, folder="sce", extension=".rds"){

  sce_names <- gsub(paste0(extension,"$"),"", list.files(path=file.path(path, folder), pattern=extension, full.names=FALSE))
  return(sce_names)
}

#' Save individual altExp
#'
#' @param altExpName  Name of altExp to be saved
#' @param altexp SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param rownames_strip_prefix Should rowname prefix (altExp name) be stripped before saving?
#' @param rownames_prefix_sep Separator used for pasting rownames prefix
#'
#' @importFrom SingleCellExperiment altExp
#' @export
altexp_save <- function(altExpName, altexp, path, rownames_strip_prefix=TRUE, rownames_prefix_sep="_"){

  ## Remove rowname prefix if required
  if(rownames_strip_prefix == TRUE) rownames(altexp) <- gsub(paste0("^",altExpName,rownames_prefix_sep),"", rownames(altexp))

  sce_save(altexp, path=path, filename=altExpName)
}

#' Load individual altExp
#'
#' @param altExpName  Name of altExp to be loaded
#' @param path Path to multisce folder for the object
#' @param rownames_add_prefix Should rowname prefix (altExp name) be added?
#' @param rownames_prefix_sep Separator used for pasting rownames prefix
#'
#' @export
altexp_load <- function(altExpName, path, rownames_add_prefix=TRUE, rownames_prefix_sep="_"){
  current_sce <- sce_load(path=path, filename=altExpName)

  ## Remove rowname prefix if required
  if(rownames_add_prefix == TRUE) rownames(current_sce) <- paste(altExpName, rownames(current_sce), sep=rownames_prefix_sep)

  return(current_sce)
}
