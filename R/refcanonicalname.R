#' Get standardised name of reference electrode
#'
#' Given a reference electrode label, this function returns its canonical name
#' (as defined by this package).
#' This function tries to match against as many variations as possible for each
#' reference electrode.
#' The entire point of this function is to decrease the mental load on the user
#' by not requiring them to remember a particular label or name for each reference
#' electrode, instead almost any sufficiently distinct label or string will still
#' be correctly identified.
#'
#' @param refname string or a vector of strings
#'
#' @return vector with corresponding "canonical" name or empty string (if none found)
#' @export
RefCanonicalName <- function(refname) {
   # scale names
   electrode.system <- list()
   electrode.system[["SHE"]] <-
      c("SHE",
        "Standard hydrogen",
        "Standard hydrogen electrode")
   electrode.system[["AgCl/Ag"]] <-
      c("AgCl/Ag",
        "Ag/AgCl",
        "AgCl",
        "Silver-Silver chloride",
        "Silver chloride",
        "SSC") # Sometimes used abbr. for Saturated Silver Chloride
   electrode.system[["Hg2Cl2/Hg"]] <-
      c("Hg2Cl2/Hg",
        "Hg/Hg2Cl2",
        "Hg2Cl2",
        "Calomel-Mercury",
        "Mercury-Calomel",
        "Calomel",
        "SCE")
   electrode.system[["AVS"]] <-
      c("AVS",
        "Vacuum",
        "Vacuum scale",
        "Absolute",
        "Absolute scale",
        "Absolute vacuum scale")
   electrode.system[["Li"]] <-
      c("Li",
        "Li/Li+",
        "Li+/Li",
        "Lithium")
   electrode.system[["Na"]] <-
      c("Na",
        "Na+/Na",
        "Na/Na+",
        "Sodium")
   electrode.system[["Mg"]] <-
      c("Mg",
        "Mg2+/Mg",
        "Mg/Mg2+",
        "Magnesium")

   # if no argument or empty string supplied as arg, return the entire list as df
   # to give the user a nice overview of all available options
   if (missing(refname) || refname == "") {
      max.row.length <- 0
      for (i in 1:length(electrode.system)) {
         # find the longest row and save its length
         this.row.length <- length(electrode.system[[i]])
         if (this.row.length > max.row.length) max.row.length <- this.row.length
      }
      # initialise an empty df with dimensions that fit electrode.system
      overview.names <-
         data.frame(
            structure(dimnames =
                         list(
                            # rownames
                            seq(1, length(electrode.system)),
                            # colnames
                            c("canonical", paste0("option", seq(1, max.row.length - 1)))),
                      matrix("",
                             nrow = length(electrode.system),
                             ncol = max.row.length,
                             byrow = TRUE)),
            stringsAsFactors = FALSE)
      # now populate the df
      for (i in 1:length(electrode.system)) {
         this.row.length <- length(electrode.system[[i]])
         overview.names[i,1:this.row.length] <- electrode.system[[i]]
      }
      message(paste0("You did not specify any reference electrode name.\n",
                     "Here are the options supported by this function (case-insensitive):"))
      print(knitr::kable(overview.names))
   }

   # defining refname in this manner makes sure to get all possible combinations
   # but there might be a number of duplicates, but those we can
   # get rid of in the next step
   electrode <-
      data.frame(refname =
                    # here we create lower-case version of electrode.system,
                    # a version with symbols (-/) subbed with spaces,
                    # and a lower-case with symbols subbed with spaces
                    c(unname(unlist(electrode.system)),
                      tolower(unname(unlist(electrode.system))),
                      gsub("[-/]", " ", unname(unlist(electrode.system))),
                      gsub("[-/]", " ", tolower(unname(unlist(electrode.system))))),
                 refcanon =
                    rep(sub("[0-9]$", "", names(unlist(electrode.system))),
                        4), # this number needs to equal number of elements in c() above!
                 stringsAsFactors = FALSE)
   # detect and remove duplicates
   electrode <-
      electrode[!duplicated(electrode$refname),]
   # reset row numbering in dataframe just for good measure
   row.names(electrode) <- 1:dim(electrode)[1]

   # pre-allocate the return vector
   refcanon <- rep("", length(refname))
   # now all we have to do is check each user-submitted refname against
   # electrode$refname and return the value on the same row but next column
   for (i in 1:length(refname)) {
      refcanon[i] <-
         electrode$refcanon[which(electrode$refname == refname[i])]
   }

   return(refcanon)
}
