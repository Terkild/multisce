#' Save SCE and conneced altExp into multisce folder
#'
#' Saves the main SCE and its connected alternative experiments (altExp) into
#' separate RDS files in the multisce folder
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param main_name  Name of main experiment (defaults to mainExpName of sce)
#' @param altexp_include  If "all", all altExps are saved (unless specified in altexp_exclude). Otherwise, a vector of altExp names to be saved
#' @param altexp_exclude  Vector of altExp names to exclude from being saved
#' @param reduceddim_include  If "all", all reducedDims are saved (unless specified in reduceddim_exclude). Otherwise, a vector of reducedDim names to be saved
#' @param reduceddim_exclude  Vector of reducedDim names to exclude from being saved
#' @param rownames_strip_prefix Should rowname prefix (altExp name) be stripped before saving?
#' @param rownames_prefix_sep Separator used for pasting rownames prefix
#' @param main_prefix Names with this prefix are omitted when saving the colData and reducedDims separately but included in the colData of the saved main SCE
#'
#' @importFrom SingleCellExperiment altExps altExpNames colData mainExpName
#' @importFrom furrr future_walk
#' @export
multisce_save <- function(sce, path, main_name=SingleCellExperiment::mainExpName(sce), altexp_include="all", altexp_exclude=c(), reduceddim_include="all", reduceddim_exclude=c(), rownames_strip_prefix=TRUE, rownames_prefix_sep="_", main_prefix=paste0(main_name,"__")){

  ### altExp ###
  ## Save altExps individually
  altexp_names <- altExpNames(sce)
  if(altexp_include != "all") altexp_names <- intersect(altexp_names, altexp_include)
  altexp_names <- setdiff(altexp_names, altexp_exclude)

  furrr::future_walk(altexp_names, altexp_save, sce=sce, path=path, rownames_strip_prefix=rownames_strip_prefix, rownames_prefix_sep=rownames_prefix_sep)

  ## Remove altExps from main object
  altExps(sce) <- NULL

  ### reducedDims ###
  reduceddim_names <- reducedDimNames(sce)
  if(reduceddim_include != "all") reduceddim_names <- intersect(reduceddim_names, reduceddim_include)
  reduceddim_names <- setdiff(reduceddim_names, reduceddim_exclude)

  furrr::future_walk(reduceddim_names, reduceddim_save, sce=sce, path=path)

  ## Remove reducedDims from main object
  reducedDims(sce) <- NULL

  ### colData ###
  ## Save colData
  colData(sce) <- coldata_save(sce, path=path, coldata_column_prefix=main_prefix)

  ### Main SCE ###
  multisce_individual_save(sce, path=path, filename=main_name)
}

#' Load SCE and conneced altExp into multisce folder
#'
#' Loads the main SCE, colData and selected alternative experiments (altExp) from
#' separate RDS files in the multisce folder
#'
#' @param path Path to multisce folder for the object
#' @param main_name  Name of main experiment (defaults to mainExpName of sce)
#' @param main_prefix Prefix for colData columns only associated with the main experiment (not loaded/saved with the remaining colData)
#' @param coldata_include Should coldata be loaded (default TRUE)
#' @param altexp_include  If "all", all altExps are loaded (unless specified in altexp_exclude). Otherwise, a vector of altExp names to be loaded
#' @param altexp_exclude  Vector of altExp names to exclude from being loaded (most relevant if altexp_include = "all")
#' @param altexp_rownames_add_prefix Should rowname prefix (altExp name) be added after loading?
#' @param altexp_rownames_prefix_sep Separator used for pasting rownames prefix
#' @param reduceddim_include  If "all", all reducedDims are loaded (unless specified in reduceddim_exclude). Otherwise, a vector of reducedDim names to be loaded
#' @param reduceddim_exclude  Vector of reducedDim names to exclude from being loaded (most relevant if reduceddim_include = "all")
#'
#' @import SingleCellExperiment
#' @importFrom furrr future_map
#' @export
multisce_load <- function(path, main_name="RNA", main_prefix=paste0(main_name,"__"), coldata_include=TRUE, altexp_include=c(), altexp_exclude=c(), altexp_rownames_add_prefix=TRUE, altexp_rownames_prefix_sep="_", reduceddim_include="all", reduceddim_exclude=c()){
  sce <- sce_load(path, filename=main_name)

  ## To assure compatibility with saving functions, add main_name to sce
  mainExpName(sce) <- main_name

  ## Load path into object
  metadata(sce)$multisce_path <- path

  ### colData ###
  if(coldata_include == TRUE) colData(sce) <- coldata_load(sce=sce, path=path, coldata_column_prefix=main_prefix)

  ### altExps ###
  if(length(altexp_include) > 0){
    if(altexp_include == "all") altexp_include <- setdiff(sce_list(path), main_name)
    altexp_include <- setdiff(altexp_include, altexp_exclude) %>% setNames(., .)

    altExps(sce) <- furrr::future_map(altexp_include, altexp_load, path=path, rownames_add_prefix=altexp_rownames_add_prefix, rownames_prefix_sep=altexp_rownames_prefix_sep)
  }

  ### reducedDims ###
  if(length(reduceddim_include) > 0){
    if(reduceddim_include == "all") reduceddim_include <- reduceddim_list(path)
    reduceddim_include <- setdiff(reduceddim_include, reduceddim_exclude) %>% setNames(., .)

    reducedDims(sce) <- furrr::future_map(reduceddim_include, reduceddim_load, path=path)
  }

  return(sce)
}
