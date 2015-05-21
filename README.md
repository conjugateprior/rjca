# rjca

`rjca` is an R package to drive the `jca` java tools that are bundled with it.  These tools count words, apply content analysis dictionaries to documents, construct concordances, and a few other things.

## Installation

The package relies on Java 1.8 or higher.  Instructions for installing this are in the `installation.Rmd` in the `vignettes` directory.

To install the package from its home on Github

    install.packages('devtools') ## if you don't already have it
    devtools::install('conjugateprior/rjca')

## Usage

See the lovely vignette for a worked example.

## Problems

If you load the package and immediately get an error about 'library paths' and suchlike, then the Java setup was not successfull.  Have another look at the installation vignette.  And if you still can't figure it out raise an issue on the Github site.

