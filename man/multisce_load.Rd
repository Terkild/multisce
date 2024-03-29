% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multisce.R
\name{multisce_load}
\alias{multisce_load}
\title{Load SCE and conneced altExp into multisce folder}
\usage{
multisce_load(
  path,
  main_name = "RNA",
  main_prefix = paste0(main_name, "__"),
  coldata_include = TRUE,
  altexp_include = c(),
  altexp_exclude = c(),
  altexp_rownames_add_prefix = TRUE,
  altexp_rownames_prefix_sep = "_",
  reduceddim_include = "all",
  reduceddim_exclude = c(),
  metadata_include = c(),
  metadata_exclude = c()
)
}
\arguments{
\item{path}{Path to multisce folder for the object}

\item{main_name}{Name of main experiment (defaults to mainExpName of sce). If NULL, an empty sce scaffold is used (fastest) - useful for plotting colData only.}

\item{main_prefix}{Prefix for colData columns only associated with the main experiment (not loaded/saved with the remaining colData)}

\item{coldata_include}{Should coldata be loaded (default TRUE)}

\item{altexp_include}{If "all", all altExps are loaded (unless specified in altexp_exclude). Otherwise, a vector of altExp names to be loaded}

\item{altexp_exclude}{Vector of altExp names to exclude from being loaded (most relevant if altexp_include = "all")}

\item{altexp_rownames_add_prefix}{Should rowname prefix (altExp name) be added after loading?}

\item{altexp_rownames_prefix_sep}{Separator used for pasting rownames prefix}

\item{reduceddim_include}{If "all", all reducedDims are loaded (unless specified in reduceddim_exclude). Otherwise, a vector of reducedDim names to be loaded}

\item{reduceddim_exclude}{Vector of reducedDim names to exclude from being loaded (most relevant if reduceddim_include = "all")}

\item{metadata_include}{If "all", all metadata entries are loaded (unless specified in metadata_exclude). Otherwise, a vector of metadata entry names to be loaded}

\item{metadata_exclude}{Vector of metadata entry names to exclude from being loaded}
}
\description{
Loads the main SCE, colData and selected alternative experiments (altExp) from
separate RDS files in the multisce folder
}
