##' Drive the category counter
##'
##' Drive the category counter.  Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the folder that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary folder}
##'  \item{\code{old.matching}: }{whether to use the old style pattern matching
##'   (behaves like JFreq but also risks double counting when multiple patterns match
##'   a string). Defaults to FALSE}
##'  \item{\code{progress}: }{Whether to show the progress of the java code as it goes
##'  through the \code{files}}
##' }
##' This function also dumps the location of the temporary folder where the
##' results landed to
##' standard error in case you want the original csv data file and yoshikoder dictionary
##' translation.
##'
##' The output is a data.frame with documents as row names, dictionary categories as
##' column names and counts of how many words in each document matched patterns in that
##' category.  Dictionaries are hierarchically structured so category names are sorted
##' depth first and
##'
##' @param dictionary a content analysis dictionary
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a data.frame containing the content analysis results
##' @export
##' @import rJava
jca_cat <- function(dictionary, files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   old.matching=FALSE, progress=TRUE,
                   output=tempfile(pattern="jca_cat"))
  control <- insert_defaults(list(...), defaults)

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

  ## swap > for _ in the column names
  hh <- read.csv(file.path(control$output, "data.csv"),
                 header=TRUE, row.names=1, check.names=FALSE,
                 encoding="UTF-8")
  names(hh) <- gsub(">", "_", names(hh))
  hh
}

##' Drive the document describer
##'
##' Drive the document describer.  Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the file that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary file}
##'  \item{\code{progress}: }{Whether to show the progress of the java code as it goes
##'  through the \code{files}}
##' }
##'
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a data.frame containing brief document descriptions
##' @export
##' @import rJava
jca_desc <- function(files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   progress=TRUE, output=tempfile(pattern="jca_desc",
                                                  fileext=".csv"))
  control <- insert_defaults(list(...), defaults)

  cc <- .jnew("org/conjugateprior/ca/app/Description")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setSilent", !control$progress)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFile", .jnew("java/io/File", control$output))

  .jcall(cc, "V", "processFiles")
  message("File is ", control$output)
  read.csv(control$output, header=TRUE, row.names=1, encoding="UTF-8")
}

##' Determine default locale
##'
##' Determines default locale by parsing the LC_COLLATE locale value
##'
##' @return ISO string representation of a locale
##' @export
get_locale <- function(){
  ll <- Sys.getlocale(category="LC_COLLATE")
  unlist(strsplit(x=ll, split=".", fixed=TRUE))[1]
}

##' Determine default encoding
##'
##' Determines default file encoding by parsing the LC_COLLATE locale value
##'
##' @return string representation of a file encoding
##' @export
get_encoding <- function(){
  ll <- Sys.getlocale(category="LC_COLLATE")
  unlist(strsplit(x=ll, split=".", fixed=TRUE))[2]
}

##' Drive the word counter
##'
##' Drive the word counter and return a sparse matrix containing word counts.
##' Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the folder that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary folder}
##'  \item{\code{progress}: }{Whether to show the progress of the java code as it goes
##'  through the \code{files}}
##'  \item{\code{no.currency}: }{Whether to remove currency. Defaults to FALSE}
##'  \item{\code{no.numbers}: }{Whether to remove numbers. Defaults to FALSE}
##'  \item{\code{stopwords}: }{File of stop words not to count. By default
##'  every word is counted}
##'  \item{\code{stemmer}: }{A language specific stemmer. By default nothing is stemmed.}
##' }
##'
##' Available stemmer languages are \code{danish}, \code{dutch}, \code{english},
##' \code{finnish}, \code{french}, \code{german}, \code{hungarian}, \code{italian},
##' \code{norwegian}, \code{portuguese}, \code{romanian}, \code{russian},
##' \code{spanish}, \code{swedish}, \code{turkish}.  These are snowball project stemmers
##' so I can't vouch for them.
##'
##' This function also dumps the location of the temporary folder where the
##' results landed to
##' standard error in case you want the original csv data file and yoshikoder dictionary
##' translation.
##'
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a sparse Matrix containing the word counts
##' @export
##' @import rJava
jca_word <- function(files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   no.currency=FALSE, no.numbers=FALSE,
                   stopwords=FALSE, stemmer=FALSE,
                   progress=TRUE, output=tempfile(pattern="jca_word"))
  control <- insert_defaults(list(...), defaults)

  stemlangs <- unlist(strsplit(paste("danish dutch english finnish french german",
                               "hungarian italian norwegian portuguese",
                               "romanian russian spanish swedish turkish"), split=" "))
  if ((control$stemmer != FALSE) && !(tolower(control$stemmer) %in% stemlangs))
    stop("stemmer must be one of:", paste(stemlangs))

  cc <- .jnew("org/conjugateprior/ca/app/WordCounter")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setSilent", !control$progress)
  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFolder", control$output)
  .jcall(cc, "V", "setFormat", "mtx")

  vf <- .jcall(cc, "Lorg/conjugateprior/ca/reports/VocabularyFilterer;", "getFilterer")
  if (control$no.numbers != FALSE)
    .jcall(vf, "V", "addNoNumberFilter")
  if (control$no.currency != FALSE)
    .jcall(vf, "V", "addNoCurrencyFilter")
  if (control$stopwords != FALSE)
    .jcall(vf, "V", "addStopwordFilter", control$stopwords)
  if (control$stemmer != FALSE)
    .jcall(vf, "V", "addStemmingFilter", control$stemmer)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "processFiles")

  message("Folder is ", control$output)
  read_mtx(control$output)
}

#' Read the a word frequency matrix in Matrix Market format
#'
#' Returns an sparse Matrix object from a folder generated by \code{jca_word}.
#' The folder is expected to contain 'data.mtx', a word count matrix
#' with documents as rows in Matrix Market format, a file of row names
#' 'documents.csv', and column names 'words.csv'.  Uses the Matrix packages \code{readMM}
#' underneath.
#'
#' @param folder folder containing the data
#' @export
#' @import Matrix
read_mtx <- function(folder){
  ff <- readMM(file.path(folder, "data.mtx"))
  rown <- read.csv(file.path(folder, "docs.csv"), encoding="UTF-8", header=FALSE)$V1
  coln <- read.csv(file.path(folder, "words.csv"), encoding="UTF-8", header=FALSE)$V1
  dimnames(ff) <- list(documents=rown, words=coln)
  ff
}

##' Drive the concordancer
##'
##' Drive the concordancer and return the concordance as a data.frame.
##'
##' Possible extra arguments are
##' \itemize{
##'  \item{\code{locale}: }{locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the folder that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary folder}
##'  \item{\code{progress}: }{whether to show the progress of the java code as it goes
##'  through the \code{files}}
##'  \item{\code{dictionary}: }{a content analysis dictionary in ykd or vbpro format}
##'  \item{\code{category}: }{optional name of a category within \code{dictionary}}
##'  \item{\code{pattern}: }{a word or phrase, possible containing wildcard character '*'}
##'  \item{\code{window}: }{how many words either side to show}
##' }
##'
##' Either \code{dictionary} or \code{pattern} must be provided.  If \code{dictionary} is
##' provided then \code{category} may also be provided.  If it is not then the concordance
##' is of all the patterns anywhere in the dictionary.
##'
##' This function also dumps the location of the temporary folder where the
##' results landed to standard error in case you want the original html data file
##' from which the data.frame was extracted.
##'
##' Note: Concordances can get big.  This function does not try to be efficient.
##'
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a data.frame containing
##' @export
##' @import rJava XML
jca_conc <- function(files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   progress=TRUE, output=tempfile(pattern="jca_conc"),
                   pattern=FALSE, dictionary=FALSE,
                   category=FALSE, window=5)

  control <- insert_defaults(list(...), defaults)
  if (is.null(control$pattern) && is.null(control$dictionary))
    stop("Either a pattern or a dictionary (plus optionally a category) must be set")

  cc <- .jnew("org/conjugateprior/ca/app/Concordancer")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setSilent", !control$progress)
  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFolder", control$output)
  .jcall(cc, "V", "setFormat", "html")
  .jcall(cc, "V", "setWindow", as.integer(control$window))

  if (control$dictionary != FALSE)
    .jcall(cc, "V", "setDictionary", path.expand(control$dictionary))
  if (control$category != FALSE)
    .jcall(cc, "V", "setCategory", control$category)
  if (control$pattern != FALSE)
    .jcall(cc, "V", "addPattern", control$pattern)

  .jcall(cc, "V", "processFiles")
  message("Folder is ", control$output)

  df <- readHTMLTable(file.path(control$output, "concordance.html"),
                                header=TRUE, colClasses='character')
  tidy_conc(df$conctable)
}

#' Turn jca_conc output into something that looks like a concordance
#'
#' Constructs a data frame with document names in the first column and
#' string-aligned concordance lines in the second column.  Blank document
#' names are filled in as necessary.  Used to format
#' output in \code{jca_conc}.
#'
#' @param df A three column data.frame with incomplete document names
#' @return a pretty data.frame that looks kind of like a concordance
tidy_conc <- function(df){
  dd <- df[,1]
  for (i in 2:length(dd))
    if (dd[i] == "")
      dd[i] <- dd[i-1]

    conc <- as.factor(apply(cbind(format(as.character(df[,2]), justify="right"),
                                  format(as.character(df[,3]), justify="left")),
                            1, function(x){ paste(x[1], x[2])} ))
  data.frame(document=dd, concordance=conc)
}

#' Fill in default values for a list
#'
#' Adds entries to a list where necessary.
#'
#' @param lst List to be augmented
#' @param defs Default entry values for \code{lst} if it doesn't already have them
#' @return an augmented list
#'
insert_defaults <- function(lst, defs){
  for (n in names(defs))
    if (is.null(lst[[n]]))
      lst[[n]] <- defs[[n]]
  lst
}

