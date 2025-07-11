#' Save file
#'
#' For saving each of the separate parts of the multisce in the save format (currently RDS)
#'
#' @param object  Object to be saved
#' @param path  Path to multisce folder
#' @param filename  Filename without extension
#' @param extension Filename extension
#' @importFrom data.table fwrite
#' @importFrom HDF5Array saveHDF5SummarizedExperiment
#'
multisce_individual_save <- function(object, path, filename, extension=".rds"){

  file_path <- file.path(path, paste0(filename, extension))

  if(!dir.exists(path)){
    message(paste("Creating directory: ",path))
    dir.create(path, recursive=TRUE)
  }

  message(paste("Saving",filename,"to",file_path))

  if(extension == ".h5"){

    saveHDF5SummarizedExperiment(object, dir=file.path(path, filename))

  } else if(extension == ".rds"){

    saveRDS(object, file=file_path)

  } else if(extension %in% c(".tsv.gz", ".tsv")) {

    data.table::fwrite(object, file=file_path, sep="\t")

  } else {

    stop(paste0("Extension ", extension, " is not supported"))

  }
}
#' Load file
#'
#' For loading the separate parts of the multisce in the saved format (currently RDS)
#'
#' @param path  Path to multisce folder
#' @param filename  Filename without extension
#' @param extensions Filename extensions to loop through to find the file (.h5 [HDF5SummarizedExperiment], .rds, .tsv and .tsv.gz is currently supported)
#' @importFrom data.table fread
#' @importFrom HDF5Array loadHDF5SummarizedExperiment
#'
multisce_individual_load <- function(path, filename, extensions=c(".h5", ".rds", ".tsv.gz", ".tsv")){

  for(i in seq_along(extensions)){
    extension <- extensions[i]
    file_path <- file.path(path, paste0(filename, extension))
    folder_path <- file.path(path, filename)

    if(file.exists(file_path) | dir.exists(folder_path)){
      message(paste("Loading",filename,"from",file_path))

      if(extension == ".h5") return(loadHDF5SummarizedExperiment(dir=folder_path))

      if(extension == ".rds") return(readRDS(file=file_path))

      if(extension %in% c(".tsv.gz", ".tsv")) return(data.table::fread(file=file_path))
    }
  }
  stop(paste0("File '", filename, "' could not be found in ", path, ". Make sure it is in a supported format (",paste(extensions, collapse=", "),")."))

  return(NULL)
}
