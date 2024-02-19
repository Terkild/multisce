#' Filter all multisce objects by coldata
#'
#' Filters all associated objects in an multisce object so their barcodes corresponds to the barcodes in the coldata
#'
#' @param path Path to multisce folder for the object
#' @param filename Name of coldata rds file
#' @param barcode_match Regexp expression to use on barcodes before matching
#' @param debug Debug mode (will print relevant info during filtering)
#'
#' @export
filter_by_coldata <- function(path, filename="coldata", barcode_match="^(.*)$", debug=FALSE){
  coldata <- coldata_load(path=path, filename=filename)

  if(debug == TRUE) print(paste("Coldata example:", head(rownames(coldata), 1)))

  colnames_match <- gsub(barcode_match, "\\1", rownames(coldata))

  if(debug == TRUE) print(paste("Coldata for matching example:", head(colnames_match, 1)))

  reduceddim_names <- reduceddim_list(path=path)

  walk(reduceddim_names, function(reduceddim_name){
    reduceddim <- reduceddim_load(path=path, reducedDimName=reduceddim_name)

    rownames(reduceddim) <- gsub(barcode_match, "\\1", rownames(reduceddim))

    if(debug == TRUE) print(paste(reduceddim_name, "colnames example:", head(rownames(reduceddim), 1)))

    message(paste("Filtering ",reduceddim_name, "to", path))
    names_diff <- setdiff(colnames_match, rownames(reduceddim))

    if(length(names_diff) > 0){
      if(debug == TRUE) print(names_diff)
      stop(paste0("ReducedDim object '",reduceddim_name,"' does not contain all barcodes included in coldata"))
    }

    reduceddim <- reduceddim[colnames_match, ]
    rownames(reduceddim) <- rownames(coldata)

    reduceddim_save(reduceddim, path=path, reducedDimName=reduceddim_name)
  })

  sce_names <- sce_list(path=path)

  walk(sce_names, function(sce_name){
    sce <- sce_load(path=path, filename=sce_name)

    colnames(sce) <- gsub(barcode_match, "\\1", colnames(sce))

    if(debug == TRUE) print(paste(sce_name, "colnames example:", head(colnames(sce), 1)))

    message(paste("Filtering ",sce_name, "to", path))
    names_diff <- setdiff(colnames_match, colnames(sce))
    if(length(names_diff) > 0){
      if(debug == TRUE) print(names_diff)
      stop(paste0("SCE object '",sce_name,"' does not contain all barcodes included in coldata"))
    }

    sce <- sce[, colnames_match]
    colnames(sce) <- rownames(coldata)

    sce_save(sce, path=path, filename=sce_name)
  })

}
