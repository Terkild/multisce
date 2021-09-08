#' Save individual metadata entries
#'
#' @param entry_name  Name of metadata entry to be saved
#' @param entry metadata entry (list element)
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for reducedDim objects
#'
#' @export
metadata_entry_save <- function(entry_name, entry, path, folder="metadata"){
  multisce_individual_save(entry, path=file.path(path, folder), filename=entry_name)
}

#' Load individual metadata entries
#'
#' @param entry_name  Name of metadata entry to be loaded
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for metadata objects
#'
#' @return Metadata entry (list element)
#'
#' @export
metadata_entry_load <- function(entry_name, path, folder="metadata"){
  entry <- multisce_individual_load(path=file.path(path, folder), filename=reducedDimName)

  return(entry)
}

#' List available metadata entries
#'
#' @param path Path to multisce folder for the object
#' @param folder Subfolder to use for metadata objects
#'
#' @export
metadata_list <- function(path, folder="metadata", extension=".rds"){

  metadata_names <- gsub(paste0(extension,"$"),"", list.files(path=file.path(path, folder), pattern=extension, full.names=FALSE))
  return(metadata_names)
}

#' Save metadata
#'
#' @param sce SingleCellElement
#' @param path Path to multisce folder (uses multisce_path from metadata by default)
#' @param metadata_include Metadata entries to save ("all" includes all entries)
#' @param metadata_exclude Metadata entries to exclude from being saved
#'
#' @return List of metadata entries excluded from being saved
#'
#' @importFrom furrr future_iwalk
#' @export
metadata_save <- function(sce, path=multisce_path(sce), metadata_include="all", metadata_exclude=c("multisce_path")){
  metadata <- metadata(sce)

  if(length(metadata) >0){
    metadata_names <- names(metadata)
    if(metadata_include != "all") metadata_include <- intersect(metadata_names, metadata_include)
    metadata_include <- setdiff(metadata_include, metadata_exclude)

    if(length(metadata_include) > 0) furrr::future_iwalk(metadata[metadata_include], ~ metadata_entry_save(entry_name=.y, entry=.x, path=path))

    if(length(metadata_exclude) > 0) return(metadata[metadata_exclude])
  }
}

#' Load metadata
#'
#' @param path Path to multisce folder (uses multisce_path from metadata by default)
#' @param metadata_include Metadata entries to load ("all" includes all entries)
#' @param metadata_exclude Metadata entries to exclude from being load
#'
#' @importFrom furrr future_map
#' @export
metadata_load <- function(path, metadata_include="all", metadata_exclude=c("multisce_path")){

  if(length(metadata_include) > 0){
    if(metadata_include == "all") metadata_include <- metadata_list(path)
    metadata_include <- setdiff(metadata_include, metadata_exclude) %>% setNames(., .)

    if(length(metadata_include) > 0){
      metadata <- furrr::future_map(metadata_include, metadata_entry_load, path=path)

      return(metadata)
    }
  }
}
