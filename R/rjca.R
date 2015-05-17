##' Drive the category counter
##'
##' Drive the category counter
##'
##' @param dictionary a content analysis dictionary
##' @param file a list of file and folder names
##' @return a category counter
jca_cat <- function(dictionary, files, ...) {

  control <- list(...)
  control$locale <- ifelse(!is.null(control$locale),
                           control$locale, get_locale())
  control$encoding <- ifelse(!is.null(control$encoding),
                             control$encoding, get_encoding())
  control$old.matching <- ifelse(!is.null(control$old.matching),
                                 control$old.matching, FALSE)
  control$progress <- ifelse(!is.null(control$progress),
                                 control$progress, TRUE)
  control$output <- ifelse(!is.null(control$output),
                           control$output, tempdir())

  cc <- .jnew("org/conjugateprior/ca/app/CategoryCounter")
  .jcall(cc, "V", "setDictionary", path.expand(dictionary))
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setUsingOldMatchStrategy", control$old.matching)
  .jcall(cc, "V", "setSilent", !control$progress)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFolder", control$output)

  .jcall(cc, "V", "processFiles")
  message("Folder is ", control$output)
  read.csv(file.path(control$output, "data.csv"), header=TRUE, row.names=1)
}

##' Determine default locale
##'
##' Determines default locale by parsing the LC_COLLATE locale value
##'
##' @return ISO string representation of a locale
get_locale <- function(){
  ll <- Sys.getlocale(category="LC_COLLATE")
  unlist(strsplit(x=ll, split=".", fixed=TRUE))[1]
}

##' Determine default encoding
##'
##' Determines default file encoding by parsing the LC_COLLATE locale value
##'
##' @return string representation of a file encoding
get_encoding <- function(){
  ll <- Sys.getlocale(category="LC_COLLATE")
  unlist(strsplit(x=ll, split=".", fixed=TRUE))[2]
}

