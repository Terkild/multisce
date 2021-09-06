#' Save file
#'
#' For saving each of the separate parts of the multisce in the save format (currently RDS)
#'
#' @param object  Object to be saved
#' @param path  Path to multisce folder
#' @param filename  Filename without extension
#' @param extension Filename extension
#'
multisce_individual_save <- function(object, path, filename, extension=".rds"){

  file_path <- file.path(path, paste0(filename, extension))

  if(!dir.exists(path)){
    message(paste("Creating directory: ",path))
    dir.create(path, recursive=TRUE)
  }

  message(paste("Saving",filename,"to",file_path))
  saveRDS(object, file=file_path)
}
#' Load file
#'
#' For loading the separate parts of the multisce in the saved format (currently RDS)
#'
#' @param path  Path to multisce folder
#' @param filename  Filename without extension
#' @param extension Filename extension
#'
multisce_individual_load <- function(path, filename, extension=".rds"){

  file_path <- file.path(path, paste0(filename, extension))

  message(paste("Loading",filename,"from",file_path))
  return(readRDS(file=file_path))
}
