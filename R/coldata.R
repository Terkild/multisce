#' Save column data
#'
#' Saves colData from SingleCellExperiment as separate file
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param filename Name of resulting rds file
#' @param coldata_column_prefix Column names with this prefix are omitted when saving but included in the returned DataFrame
#' @param barcodes_file Name of file to save barcodes (default: barcodes.tsv)
#' @param barcodes_overwrite Should barcodes file be overwritten if existing - may break colname relationship to objects not currently loaded in the multisce
#'
#' @return DataFrame containing only the columns with the indicated prefix
#'
#' @importFrom SingleCellExperiment colData mainExpName
#' @export
coldata_save <- function(sce, path=multisce_path(sce), filename="coldata", coldata_column_prefix=paste0(mainExpName(sce),"_"), barcodes_file="barcodes.tsv", barcodes_overwrite=FALSE){
  df <- colData(sce)
  columns_skip <- grepl(paste0("^", coldata_column_prefix), colnames(df))

  if(barcodes_exists(path=path, filename=barcodes_file) && barcodes_overwrite == FALSE){

    # Check of SCE barcodes matches barcodes file
    if(barcodes_check(sce, path=path, bc_filename=barcodes_file) == FALSE){
      stop(paste("Barcodes of colData does not match saved barcodes file - set barcode_overwrite=TRUE to ignore this (Obs. this may break associations with other linked objects)"))
    }
  }

  multisce_individual_save(object=df[, columns_skip == FALSE], path=path, filename=filename)

  return(df[, columns_skip])
}

#' Load column data
#'
#' Loads colData DataFrame from separate file
#'
#' @param sce SingleCellExperiment object
#' @param path Path to multisce folder for the object
#' @param filename Name of resulting rds file
#' @param coldata_column_prefix Column names with this prefix are omitted when saving but included in the returned DataFrame
#'
#' @return DataFrame containing only the columns with the indicated prefix
#'
#' @importFrom SingleCellExperiment colData mainExpName
#' @export
coldata_load <- function(sce=NULL, path, filename="coldata", coldata_column_prefix=paste0(mainExpName(sce),"_")){
  df <- multisce_individual_load(path=path, filename=filename)
  if(!is.null(sce)){
    df_existing <- colData(sce)
    if(ncol(df_existing)>0){
      colnames(df_existing) <- paste0(coldata_column_prefix, colnames(df_existing))
      df <- cbind(df_existing, df)
    }
  }

  return(df)
}
