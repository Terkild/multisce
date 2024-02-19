#' Save SCE and conneced altExp into multisce folder
#'
#' Saves the main SCE and its connected alternative experiments (altExp) into
#' separate RDS files in the multisce folder
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param main_name  Name of main experiment (defaults to mainExpName of sce)
#' @param barcodes Boolean if barcodes should be saved as individual file
#' @param barcodes_file Name of file to save barcodes (default: barcodes.tsv)
#' @param barcodes_overwrite Should barcodes file be overwritten if existing - may break colname relationship to objects not currently loaded in the multisce
#' @param altexp_include  If "all", all altExps are saved (unless specified in altexp_exclude). Otherwise, a vector of altExp names to be saved
#' @param altexp_exclude  Vector of altExp names to exclude from being saved
#' @param rownames_strip_prefix Should rowname prefix (altExp name) be stripped before saving?
#' @param rownames_prefix_sep Separator used for pasting rownames prefix
#' @param reduceddim_include  If "all", all reducedDims are saved (unless specified in reduceddim_exclude). Otherwise, a vector of reducedDim names to be saved
#' @param reduceddim_exclude  Vector of reducedDim names to exclude from being saved
#' @param metadata_include  If "all", all metadata entries are saved (unless specified in metadata_exclude). Otherwise, a vector of metadata entry names to be saved
#' @param metadata_exclude  Vector of metadata entry names to exclude from being saved
#' @param main_prefix Names with this prefix are omitted when saving the colData and reducedDims separately but included in the colData of the saved main SCE
#'
#' @importFrom SingleCellExperiment altExps altExpNames colData mainExpName
#' @importFrom purrr walk
#' @importFrom S4Vectors metadata
#' @export
multisce_save <- function(sce, path, main_name=SingleCellExperiment::mainExpName(sce), barcodes=TRUE, barcodes_file="barcodes.tsv", barcodes_overwrite=FALSE, altexp_include="all", altexp_exclude=c(), reduceddim_include="all", reduceddim_exclude=c(), rownames_strip_prefix=TRUE, rownames_prefix_sep="_", main_prefix=paste0(main_name,"__"), metadata_include="all", metadata_exclude=c("multisce_path")){

  if(barcodes_exists(path=path, filename=barcodes_file) && barcodes_overwrite == FALSE){

    # Check of SCE barcodes matches barcodes file
    if(!barcodes_check(sce, path=path, bc_filename=barcodes_file)){
      stop("Barcodes of SCE does not match saved barcodes file - set barcode_overwrite=TRUE to ignore this (Obs. this may break associations with other linked objects)")
    }

    barcodes_vector <- barcodes_load(path=path, filename=barcodes_file)
  } else {
    barcodes_vector <- colnames(sce)
  }

  # Write barcodes file
  if(barcodes == TRUE){
    barcodes_save(sce, path=path, filename=barcodes_file, overwrite=barcodes_overwrite)
  }

  ### altExp ###
  ## Save altExps individually
  altexp_names <- altExpNames(sce)
  if(!"all" %in% altexp_include) altexp_names <- intersect(altexp_names, altexp_include)
  altexp_names <- setdiff(altexp_names, altexp_exclude)

  walk(altexp_names, ~ altexp_save(.x, altexp=altExp(sce, .x), path=path, rownames_strip_prefix=rownames_strip_prefix, rownames_prefix_sep=rownames_prefix_sep, barcodes_file=barcodes_file, barcodes_overwrite=barcodes_overwrite))

  ## Remove altExps from main object
  altExps(sce) <- NULL

  ### reducedDims ###
  reduceddim_names <- reducedDimNames(sce)
  if(!"all" %in% reduceddim_include) reduceddim_names <- intersect(reduceddim_names, reduceddim_include)
  reduceddim_names <- setdiff(reduceddim_names, reduceddim_exclude)

  walk(reduceddim_names, ~ reduceddim_save(.x, reduceddim=reducedDim(sce, .x), path=path), barcodes_file=barcodes_file, barcodes_overwrite=barcodes_overwrite)

  ## Remove reducedDims from main object
  reducedDims(sce) <- NULL

  ### colData ###
  ## Save colData
  colData(sce) <- coldata_save(sce, path=path, coldata_column_prefix=main_prefix, barcodes_file=barcodes_file, barcodes_overwrite=barcodes_overwrite)

  ### metadata ###
  if(length(metadata(sce)) > 0) metadata_save(sce, path=path, metadata_include=metadata_include, metadata_exclude=metadata_exclude)

  ### Main SCE ###
  if(!is.null(main_name)) sce_save(sce, path=path, filename=main_name, barcodes_file=barcodes_file, barcodes_overwrite=barcodes_overwrite)
}

#' Load SCE and conneced altExp into multisce folder
#'
#' Loads the main SCE, colData and selected alternative experiments (altExp) from
#' separate RDS files in the multisce folder
#'
#' @param path Path to multisce folder for the object
#' @param main_name  Name of main experiment (defaults to mainExpName of sce). If NULL, an empty sce scaffold is used (fastest) - useful for plotting colData only.
#' @param main_prefix Prefix for colData columns only associated with the main experiment (not loaded/saved with the remaining colData)
#' @param coldata_include Should coldata be loaded (default TRUE)
#' @param altexp_include  If "all", all altExps are loaded (unless specified in altexp_exclude). Otherwise, a vector of altExp names to be loaded
#' @param altexp_exclude  Vector of altExp names to exclude from being loaded (most relevant if altexp_include = "all")
#' @param altexp_rownames_add_prefix Should rowname prefix (altExp name) be added after loading?
#' @param altexp_rownames_prefix_sep Separator used for pasting rownames prefix
#' @param reduceddim_include  If "all", all reducedDims are loaded (unless specified in reduceddim_exclude). Otherwise, a vector of reducedDim names to be loaded
#' @param reduceddim_exclude  Vector of reducedDim names to exclude from being loaded (most relevant if reduceddim_include = "all")
#' @param metadata_include  If "all", all metadata entries are loaded (unless specified in metadata_exclude). Otherwise, a vector of metadata entry names to be loaded
#' @param metadata_exclude  Vector of metadata entry names to exclude from being loaded
#'
#' @import SingleCellExperiment
#' @importFrom furrr future_map
#' @importFrom S4Vectors metadata
#' @export
multisce_load <- function(path, main_name="RNA", main_prefix=paste0(main_name,"__"), coldata_include=TRUE, altexp_include=c(), altexp_exclude=c(), altexp_rownames_add_prefix=TRUE, altexp_rownames_prefix_sep="_", reduceddim_include="all", reduceddim_exclude=c(), metadata_include=c(), metadata_exclude=c()){
  if(is.null(main_name)){
    if(coldata_include == TRUE){
      sce <- SingleCellExperiment::SingleCellExperiment()
    } else {
      stop("main_name cannot be NULL if coldata_include==FALSE")
    }
  } else {
    sce <- sce_load(path, filename=main_name)
  }

  ## To assure compatibility with saving functions, add main_name to sce
  mainExpName(sce) <- main_name

  sce <- multisce_add(sce, path=path,
                      main_name=main_name, main_prefix=main_prefix,
                      coldata_include=coldata_include,
                      altexp_include=altexp_include, altexp_exclude=altexp_exclude, altexp_clear=TRUE,
                      altexp_rownames_add_prefix=altexp_rownames_add_prefix, altexp_rownames_prefix_sep=altexp_rownames_prefix_sep,
                      reduceddim_include=reduceddim_include, reduceddim_exclude=reduceddim_exclude,
                      metadata_include=metadata_include, metadata_exclude=metadata_exclude)

  return(sce)
}

#' Add elements to multisce object
#'
#' Adds colData, selected alternative experiments (altExp) or dimension reductions (reduceddim) from
#' separate RDS files in the multisce folder
#'
#' @param sce SingleCellExperiment
#' @param path Path to multisce folder for the object
#' @param main_name  Name of main experiment (defaults to mainExpName of sce)
#' @param main_prefix Prefix for colData columns only associated with the main experiment (not loaded/saved with the remaining colData)
#' @param coldata_include Should coldata be loaded (default TRUE)
#' @param altexp_include  If "all", all altExps are loaded (unless specified in altexp_exclude). Otherwise, a vector of altExp names to be loaded
#' @param altexp_exclude  Vector of altExp names to exclude from being loaded (most relevant if altexp_include = "all")
#' @param altexp_clear Remove existing altExps before adding
#' @param altexp_rownames_add_prefix Should rowname prefix (altExp name) be added after loading?
#' @param altexp_rownames_prefix_sep Separator used for pasting rownames prefix
#' @param reduceddim_include  If "all", all reducedDims are loaded (unless specified in reduceddim_exclude). Otherwise, a vector of reducedDim names to be loaded
#' @param reduceddim_exclude  Vector of reducedDim names to exclude from being loaded (most relevant if reduceddim_include = "all")
#' @param reduceddim_clear Remove existing reducedDims before adding
#' @param metadata_include  If "all", all metadata entries are loaded (unless specified in metadata_exclude). Otherwise, a vector of metadata entry names to be loaded
#' @param metadata_exclude  Vector of metadata entry names to exclude from being loaded
#' @param metadata_clear Remove existing metadata before adding
#'
#' @import SingleCellExperiment
#' @importFrom Matrix Matrix
#' @importFrom magrittr "%>%"
#' @export
multisce_add <- function(sce, path=multisce_path(sce), main_name=mainExpName(sce), main_prefix=paste0(main_name,"__"), coldata_include=FALSE, altexp_include=c(), altexp_exclude=c(), altexp_clear=FALSE, altexp_rownames_add_prefix=TRUE, altexp_rownames_prefix_sep="_", reduceddim_include=c(), reduceddim_exclude=c(), reduceddim_clear=FALSE, metadata_include=c(), metadata_exclude=c(), metadata_clear=FALSE){

  ### colData ###
  if(coldata_include == TRUE){
    coldata <- coldata_load(sce=sce, path=path, coldata_column_prefix=main_prefix)

    # If dummy data should be loaded, make sure it matches the dimensions of the coldata
    if(is.null(main_name)){
      mtx_dummy <- matrix(data=0, nrow=1, ncol=nrow(coldata), dimnames=list("DUMMY", rownames(coldata)))
      sce <- SingleCellExperiment::SingleCellExperiment(list(counts=Matrix::Matrix(mtx_dummy)))
      mainExpName(sce) <- main_name
    }

    colData(sce) <- coldata
  }

  ### metadata ###
  add_metadata <- metadata_load(path=path, metadata_include=metadata_include, metadata_exclude=metadata_exclude)
  current_metadata <- metadata(sce)

  if(metadata_clear==TRUE){
    current_metadata <- list()
  }

  metadata(sce) <- append(current_metadata[setdiff(names(current_metadata), names(add_metadata))], add_metadata)

  ## Load path into object
  multisce_path(sce) <- path

  ### altExps ###
  if(length(altexp_include) > 0){
    if("all" %in% altexp_include) altexp_include <- setdiff(sce_list(path), c(main_name, altExpNames(sce)))
    altexp_include <- setdiff(altexp_include, altexp_exclude) %>% setNames(., .)

    # If existing altexps should not be included, remove them before adding

    if(length(altexp_include) > 0) altexp_add <- lapply(altexp_include %>% setNames(.,.),
                                                                   altexp_load, path=path,
                                                                   rownames_add_prefix=altexp_rownames_add_prefix, rownames_prefix_sep=altexp_rownames_prefix_sep)

    if(altexp_clear==TRUE | length(altExpNames(sce))<1){
      altExps(sce) <- altexp_add
    } else {
      altExps(sce) %<>% append(altexp_add)
    }

  }

  ### reducedDims ###
  if(length(reduceddim_include) > 0){
    if("all" %in% reduceddim_include) reduceddim_include <- setdiff(reduceddim_list(path), reducedDimNames(sce))
    reduceddim_include <- setdiff(reduceddim_include, reduceddim_exclude) %>% setNames(., .)

    if(length(reduceddim_include) > 0){
      reducedDim_add <- lapply(reduceddim_include, reduceddim_load, path=path)
    }

    if(reduceddim_clear==TRUE | length(reducedDimNames(sce))<1){
      reducedDims(sce) <- reducedDim_add
    } else if(length(reduceddim_include) > 0) {
      reducedDims(sce) %<>% append(reducedDim_add)
    }
  }

  return(sce)
}

#' Get current multisce path
#'
#' @param sce SingleCellExperiment object for main experiment.
#' @return Path to multisce folder
#'
#' @importFrom S4Vectors metadata
#' @export
multisce_path <- function(sce){
  return(metadata(sce)$multisce_path)
}

#' Set current multisce path
#'
#' @param sce SingleCellExperiment object for main experiment.
#' @param value  Path to multisce folder
#' @return SingleCellExperiment object with multisce_path set
#'
#' @importFrom S4Vectors metadata
#' @export
"multisce_path<-" <- function(sce, ..., value){
  metadata(sce, ...)$multisce_path <- value

  return(sce)
}
