#' Potentials as SHE
#'
#' This function just outputs a tidy dataframe with potential vs SHE for
#' different scales, electrolytes, concentrations, and temperatures.
#' Using data from literature.
#'
#' @return tidy dataframe with the following columns
#'    \tabular{ll}{
#'    \code{electrode}     \tab reference electrode \cr
#'    \code{electrolyte}   \tab electrolyte \cr
#'    \code{conc.num}      \tab concentration of electrolyte, mol/L \cr
#'    \code{conc.string}   \tab concentration of electrolyte, as string, may also note temperature at which conc \cr
#'    \code{temp}          \tab temperature / degrees Celsius \cr
#'    \code{SHE}           \tab potential vs SHE / volt \cr
#'    \code{sid}           \tab set id, just for housekeeping inside this function \cr
#'    \code{reference}     \tab BibTeX reference \cr
#'    \code{dEdT}          \tab temperature coefficient / volt/kelvin \cr
#'    }
#' @export
potentials.as.SHE <- function() {
   # scale name should be one of canonical (see RefCanonicalName)

   # follow the convention of "each row one observation" (at different temperatures)
   # all potentials vs SHE

   potentials <-
      as.data.frame(
         matrix(data =
                   # electrode # electrolyte # conc/M # conc label # temp # pot vs SHE # set id # ref
                   c("AgCl/Ag",   "NaCl(aq)", "5.9", "saturated",   "25", "0.2630",  "9",  "CRC 97th ed., 97-05-22",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "10", "0.215",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "15", "0.212",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "20", "0.208",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "25", "0.205",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "30", "0.201",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "35", "0.197",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "40", "0.193",   "1",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "10", "0.214",   "2",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "15", "0.209",   "2",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "20", "0.204",   "2",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "25", "0.199",   "2",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "30", "0.194",   "2",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "35", "0.189",   "2",  "Sawyer1995",
                     "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "40", "0.184",   "2",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "10", "0.336",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "15", "0.336",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "20", "0.336",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "25", "0.336",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "30", "0.335",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "35", "0.334",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "40", "0.334",   "3",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "10", "0.287",   "4",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "20", "0.284",   "4",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "25", "0.283",   "4",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "30", "0.282",   "4",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "40", "0.278",   "4",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "10", "0.256",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "15", "0.254",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "20", "0.252",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "25", "0.250",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "30", "0.248",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "35", "0.246",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "40", "0.244",   "5",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "10", "0.254",   "6",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "15", "0.251",   "6",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "20", "0.248",   "6",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "25", "0.244",   "6",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "30", "0.241",   "6",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "35", "0.238",   "6",  "Sawyer1995",
                     "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "40", "0.234",   "6",  "Sawyer1995",
                     "AVS",       "",         "",    "",            "25", "-4.44",   "7",  "Trasatti1986",
                     "SHE",       "",         "",    "",       "-273.15", "0.00",    "8",  "Inzelt2013",
                     "SHE",       "",         "",    "",             "0", "0.00",    "8",  "Inzelt2013",
                     "SHE",       "",         "",    "",            "25", "0.00",    "8",  "Inzelt2013",
                     # arbitrary max T=580C (temp at which sodalime glass loses rigidity)
                     "SHE",       "",         "",    "",           "580", "0.00",    "8",  "Inzelt2013",
                     "Li",        "",         "1.0", "1.0M at 25C", "25", "-3.0401", "10", "CRC 97th ed., 97-05-22",
                     "Na",        "",         "1.0", "1.0M at 25C", "25", "-2.71",   "11", "CRC 97th ed., 97-05-22",
                     "Mg",        "",         "1.0", "1.0M at 25C", "25", "-2.372",  "12", "CRC 97th ed., 97-05-22"),
                ncol = 8,
                byrow = TRUE), stringsAsFactors = FALSE)

   colnames(potentials) <-
      c("electrode",
        "electrolyte",
        "conc.num",
        "conc.string",
        "temp",
        "SHE",
        "sid",
        "reference")

   # convert these columns to type numeric
   potentials[, c("conc.num", "temp", "SHE")] <-
      as.numeric(as.character(unlist(potentials[, c("conc.num", "temp", "SHE")])))

   # make room for a dE/dT column
   potentials$dEdT <- as.numeric(NA)

   # calculate temperature coefficient (dE/dT) for each scale, concentration, and electrolyte (ie. set id)
   for (s in 1:length(unique(potentials$sid))) {
      # sid column eas added to data just to make this calculation here easier
      subspot <- potentials[which(potentials$sid == unique(potentials$sid)[s]), ]
      # a linear fit will give us temperature coefficient as slope
      lm.subspot <- stats::lm(SHE ~ temp, data = subspot)
      potentials[which(potentials$sid == unique(potentials$sid)[s]), "dEdT"] <-
         lm.subspot$coefficients[2]
   }

   return(potentials)
}




#' Convert from electrochemical or physical scale to SHE
#'
#' Convert an arbitrary number of potentials against any known electrochemical
#' scale (or the electronic vacuum scale) to potential vs SHE.
#'
#' @param potential potential in volt
#' @param scale name of the original scale
#' @param electrolyte optional, specify electrolyte solution, e.g., "KCl(aq)". Must match value in \code{as.SHE.data$electrolyte}.
#' @param concentration of electrolyte in mol/L, or as the string "saturated"
#' @param temperature of system in degrees Celsius
#' @param as.SHE.data dataframe with dataset
#'
#' @return potential in SHE scale
#' @export
as.SHE <- function(potential,
                   scale,
                   electrolyte = "",
                   concentration = "saturated",
                   temperature = 25,
                   as.SHE.data = refelectrodes::potentials.as.SHE()) {

   # if the supplied temperature does not exist in the data, this function will attempt to interpolate
   # note that concentration has to match, no interpolation is attempted for conc

   # potential and scale vectors supplied by user could have arbitrary length
   # just make sure potential and scale args have the same length (or length(scale) == 1)
   if (length(potential) == 0 | length(scale) == 0) {
      stop("Potential or scale arguments cannot be empty!")
   } else if (length(potential) != length(scale)) {
      # stop, unless length(scale) == 1 where we will assume it should be recycled
      if (length(scale) == 1) {
         message("Arg <scale> has unit length. We'll recycle it to match length of <potential>.")
         scale <- rep(scale, length(potential))
      } else {
         stop("Length of <potential> and <scale> must be equal OR <scale> may be unit length.")
      }
   }

   arglength <- length(potential)
   # make the args concentration, temperature and electrolyte this same length,
   # unless the user supplied them (only necessary for length > 1)
   if (arglength > 1) {
      # handle two cases:
      # 1. user did not touch concentration, temperature and electrolyte args.
      #    Assume they forgot and reset their length and print a message
      # 2. user did change concentration or temperature or electrolyte, but still failed to
      #    ensure length equal to arglength. In this case, abort.
      # note: we can get the default value set in the function call using formals()
      if (identical(concentration, formals(as.SHE)$concentration) &
          identical(temperature, formals(as.SHE)$temperature) &
          identical(electrolyte, formals(as.SHE)$electrolyte)) {
         # case 1
         # message("NOTE: default concentration and temperature values used for all potentials and scales.")
         message(paste0("The default concentration (", formals(as.SHE)$concentration, ") and temperature (", formals(as.SHE)$temperature, "C) will be assumed for all your potential/scale values."))
         concentration <- rep(concentration, arglength)
         temperature <- rep(temperature, arglength)
         electrolyte <- rep(electrolyte, arglength)
      } else {
         # case 2
         stop("Concentration, temperature and electrolyte arguments must have the same number of elements as potential and scale!")
      }
   }

   ## we can now safely assume that length(<args>) == arglength
   # place args into a single dataframe
   # this way, we can correlate columns to each other by row
   dfargs <-
      data.frame(potential = potential,
                 scale = refelectrodes::RefCanonicalName(scale),
                 electrolyte = electrolyte,
                 concentration = concentration,
                 temperature = temperature,
                 stringsAsFactors = FALSE)
   # add column to keep track of vacuum scale
   # dfargs$vacuum <- as.logical(FALSE)
   # add column to hold calc potential vs SHE
   dfargs$SHE <-  as.numeric(NA)

   ## From here on, ONLY access the arguments via this dataframe
   ## That is, use dfargs$electrolyte, NOT electrolyte

   # SHE scale special considerations
   # 1. concentration is constant for SHE
   if (any(dfargs$scale == refelectrodes::RefCanonicalName("SHE"))) {
      dfargs$concentration[which(dfargs$scale == refelectrodes::RefCanonicalName("SHE"))] <- ""
      dfargs$electrolyte[which(dfargs$scale == refelectrodes::RefCanonicalName("SHE"))] <- ""
   }

   # AVS scale special considerations
   # 1. concentration is meaningless for AVS
   if (any(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))) {
      # concentration is meaningless for AVS (no electrolyte) so for those rows, we'll reset it
      dfargs$concentration[which(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))] <- ""
      dfargs$electrolyte[which(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))] <- ""
      # dfargs$vacuum[which(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))] <- TRUE
   }

   # now just work our way through dfargs, line-by-line to determine potential as SHE
   # all necessary conditions should be recorded right here in dfargs
   for (p in 1:dim(dfargs)[1]) {
      ## WE ARE NOW WORKING ROW-BY-ROW THROUGH THE SUPPLIED ARGUMENTS IN dfargs
      # Step-wise matching:
      # + first, we subset against electrode scale. If dataset only has one row, done. Else,
      # + we subset against either conc.string or conc.num. Stop if zero rows in dataset (error), otherwise proceed.
      # Our "dataset" is the literature data supplied via the argument as.SHE.data
      this.data.scale <- subset(as.SHE.data, electrode == dfargs$scale[p])
      # subset.scale <- subset(as.SHE.data, electrode == dfargs$scale[p])
      if (dim(this.data.scale)[1] > 1) {
         # continue matching, now against conc.string or conc.num
         if (is.character(dfargs$concentration[p])) {
            this.data.concentration <-
               # subset.concentration <-
               subset(this.data.scale, conc.string == dfargs$concentration[p])
         } else {
            this.data.concentration <-
               # subset.concentration <-
               subset(this.data.scale, conc.num == dfargs$concentration[p])
         }
         # stop if the resulting dataframe after matching contains no rows
         if (dim(this.data.concentration)[1] == 0) {
            stop(paste0("Failed to find any matching entries in dataset for ",
                        paste(dfargs[p, ], collapse = " ", sep = "")))
         }
         # Note: it's ok at this point if the resulting dataset contains more than one row as
         #       more matching will be done below
         # If we haven't had reason to stop(), we should be good
         # just housekeeping: rename the variable so we don't have to edit code below
         this.SHE.data <- this.data.concentration
         # subset.SHE.data <- subset.concentration
      } else {
         # just housekeeping again
         this.SHE.data <- this.data.scale
         # subset.SHE.data <- subset.scale
      }


      ## Electrolyte
      # == We would like to transparently handle the following scenario:
      # || if the user did not specify electrolyte solution (which we can check by using formals())
      # || but the dataset (after subsetting against scale and concentration above) still contains
      # || more than one electrolyte
      # >> Approach: we'll specify a "fallback" electrolyte, KCl (usually that's what the user wants)
      # >> and inform/warn about it

      # KCl is a good assumption, as we always have KCl
      # for the cases where an electrode system has more than one electrolyte
      fallback.electrolyte <- "KCl(aq)"
      if (length(unique(this.SHE.data$electrolyte)) > 1) {
         if (formals(as.SHE)$electrolyte == "") {
            warning(paste0("More than one electrolyte ",
                           "available for E(", dfargs$scale[p], ") in dataset. ",
                           "I'll assume you want ", fallback.electrolyte, "."))
            this.SHE.data <-
               subset(this.SHE.data, electrolyte == fallback.electrolyte)
         } else {
            # else the user did change the electrolyte arg, use the user's value
            this.SHE.data <-
               subset(this.SHE.data, electrolyte == dfargs$electrolyte[p])
            # but stop if the resulting dataframe contains no rows
            if (dim(this.SHE.data)[1] == 0) stop("Your choice of electrolyte does not match any data!")
         }
      } else {
         # dataset contains only one unique electrolyte
         # again, check if electrolyte in arg matches the one in dataset
         # if it does, great, if it does not, print a message and use it anyway
         if (unique(this.SHE.data$electrolyte) == dfargs$electrolyte[p]) {
            this.SHE.data <-
               subset(this.SHE.data, electrolyte == dfargs$electrolyte[p])
         } else {
            # whatever electrolyte the user supplied does not match what's left in the datasubset
            # but at this point the user is probably better served by returning the electrolyte we have
            # along with an informative message (that's the only reason for the if-else below)
            electrolytes.in.subset <-
               unique(subset(as.SHE.data, electrode == dfargs$scale[p])$electrolyte)
            if (dfargs$electrolyte[p] == "") {
               message(
                  paste0('Electrolyte "" (empty string) not in dataset for E(',
                         dfargs$scale[p], '). ',
                         'These electrolytes are: ',
                         paste(electrolytes.in.subset, collapse = ', or '), '.',
                         "I'll assume you want ", fallback.electrolyte, ".")
               )
            } else {
               message(paste0("Electrolyte ", dfargs$electrolyte[p], " not in dataset for E(",
                              dfargs$scale[p], "). ",
                              "These electrolytes are: ",
                              paste(electrolytes.in.subset, collapse = ", or "), ".",
                              "I'll assume you want ", fallback.electrolyte, ".")
               )
            }
         }
      }

      # temperature
      # either happens to match a temperature in the dataset, or we interpolate
      # (under the assumption that potential varies linearly with temperature)
      if (!any(this.SHE.data$temp == dfargs$temperature[p])) {
         # sought temperature was not available in dataset, check that it falls inside
         # note: important to use less/more-than-or-equal in case data only contains one value
         if ((dfargs$temperature[p] <= max(this.SHE.data$temp)) && (dfargs$temperature[p] >= min(this.SHE.data$temp))) {
            # within dataset range, do linear interpolation
            lm.subset <- stats::lm(SHE ~ temp, data = this.SHE.data)
            # interpolated temperature, calculated based on linear regression
            # (more accurate than simple linear interpolation with approx())
            pot.interp <-
               lm.subset$coefficients[2] * dfargs$temperature[p] + lm.subset$coefficients[1]
            ### CALC POTENTIAL vs SHE
            dfargs$SHE[p] <-
               ifelse(dfargs$scale[p] == "AVS",
                      pot.interp - dfargs$potential[p],
                      pot.interp + dfargs$potential[p])
         }
      } else {
         # requested temperature does exist in dataset
         ### CALC POTENTIAL vs SHE
         dfargs$SHE[p] <-
            ifelse(dfargs$scale[p] == "AVS",
                   subset(this.SHE.data, temp == dfargs$temperature[p])$SHE - dfargs$potential[p],
                   subset(this.SHE.data, temp == dfargs$temperature[p])$SHE + dfargs$potential[p])
      }
   }

   return(dfargs$SHE)
}




#' Convert from SHE scale to another electrochemical or physical scale
#'
#' Convert an arbitrary number of potentials vs SHE to another electrochemical
#' scale (or the vacuum scale).
#' The available target scales are those listed by \code{\link{potentials.as.SHE}}.
#'
#' @param potential potential in volt
#' @param scale name of the target scale
#' @param electrolyte optional, specify electrolyte solution, e.g., "KCl(aq)". Must match one of the values in \code{\link{potentials.as.SHE}$electrolyte}
#' @param concentration of electrolyte in mol/L, or as the string "saturated"
#' @param temperature of system in degrees Celsius
#' @param as.SHE.data by default this parameter reads the full dataset \code{\link{potentials.as.SHE}}
#'
#' @return potential in the specified target scale
#' @export
from.SHE <- function(potential,
                     scale,
                     electrolyte = "",
                     concentration = "saturated",
                     temperature = 25,
                     as.SHE.data = refelectrodes::potentials.as.SHE()) {

   # if the supplied temperature does not exist in the data, this function will attempt to interpolate
   # note that concentration has to match, no interpolation is attempted for conc

   # potential and scale vectors supplied by user could have arbitrary length
   # just make sure potential and scale args have the same length (or length(scale) == 1)
   if (length(potential) == 0 | length(scale) == 0) {
      stop("Potential or scale arguments cannot be empty!")
   } else if (length(potential) != length(scale)) {
      # stop, unless length(scale) == 1 where we will assume it should be recycled
      if (length(scale) == 1) {
         message("Arg <scale> has unit length. We'll recycle it to match length of <potential>.")
         scale <- rep(scale, length(potential))
      } else {
         stop("Length of <potential> and <scale> must be equal OR <scale> may be unit length.")
      }
   }

   arglength <- length(potential)
   # make the args concentration, temperature and electrolyte this same length,
   # unless the user supplied them (only necessary for length > 1)
   if (arglength > 1) {
      # handle two cases:
      # 1. user did not touch concentration, temperature and electrolyte args.
      #    Assume they forgot and reset their length and print a message
      # 2. user did change concentration or temperature or electrolyte, but still failed to
      #    ensure length equal to arglength. In this case, abort.
      # note: we can get the default value set in the function call using formals()
      if (identical(concentration, formals(from.SHE)$concentration) &
          identical(temperature, formals(from.SHE)$temperature) &
          identical(electrolyte, formals(from.SHE)$electrolyte)) {
         # case 1
         message(paste0("The default concentration (", formals(from.SHE)$concentration, ") and temperature (", formals(from.SHE)$temperature, "C) will be assumed for all your potential/scale values."))
         concentration <- rep(concentration, arglength)
         temperature <- rep(temperature, arglength)
         electrolyte <- rep(electrolyte, arglength)
      } else {
         # case 2
         stop("Concentration, temperature and electrolyte arguments must have the same number of elements as potential and scale!")
      }
   }

   ## we can now safely assume that length(<args>) == arglength
   # place args into a single dataframe
   # this way, we can correlate columns to each other by row
   dfargs <-
      data.frame(potential = potential, # vs SHE
                 scale = refelectrodes::RefCanonicalName(scale), # target scale
                 electrolyte = electrolyte,
                 concentration = concentration,
                 temperature = temperature,
                 stringsAsFactors = FALSE)

   ## From here on, ONLY access the arguments via this dataframe
   ## That is, use dfargs$electrolyte, NOT electrolyte (and so on)

   # SHE scale special considerations
   # 1. concentration is constant for SHE
   if (any(dfargs$scale == refelectrodes::RefCanonicalName("SHE"))) {
      dfargs$concentration[which(dfargs$scale == refelectrodes::RefCanonicalName("SHE"))] <- ""
      dfargs$electrolyte[which(dfargs$scale == refelectrodes::RefCanonicalName("SHE"))] <- ""
   }

   # AVS scale special considerations
   # 1. concentration is meaningless for AVS
   if (any(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))) {
      # concentration is meaningless for AVS (no electrolyte) so for those rows, we'll reset it
      dfargs$concentration[which(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))] <- ""
      dfargs$electrolyte[which(dfargs$scale == refelectrodes::RefCanonicalName("AVS"))] <- ""
   }

   # now just work our way through dfargs, line-by-line to determine potential as SHE
   # all necessary conditions should be recorded right here in dfargs
   for (p in 1:dim(dfargs)[1]) {
      ## WE ARE NOW WORKING ROW-BY-ROW THROUGH THE SUPPLIED ARGUMENTS IN dfargs
      # Step-wise matching:
      # + first, we subset against electrode scale. If dataset only has one row, done. Else,
      # + we subset against either conc.string or conc.num. Stop if zero rows in dataset (error), otherwise proceed.
      # Our "dataset" is the literature data supplied via the argument as.SHE.data
      this.data.scale <- subset(as.SHE.data, electrode == dfargs$scale[p])
      # subset.scale <- subset(as.SHE.data, electrode == dfargs$scale[p])
      if (dim(this.data.scale)[1] > 1) {
         # continue matching, now against conc.string or conc.num
         if (is.character(dfargs$concentration[p])) {
            this.data.concentration <-
               subset(this.data.scale, conc.string == dfargs$concentration[p])
         } else {
            this.data.concentration <-
               subset(this.data.scale, conc.num == dfargs$concentration[p])
         }
         # stop if the resulting dataframe after matching contains no rows
         if (dim(this.data.concentration)[1] == 0) {
            stop(paste0("Failed to find any matching entries in dataset for ",
                        paste(dfargs[p, ], collapse = " ", sep = "")))
         }
         # Note: it's ok at this point if the resulting dataset contains more than one row as
         #       more matching will be done below
         # If we haven't had reason to stop(), we should be good
         # just housekeeping: rename the variable so we don't have to edit code below
         this.SHE.data <- this.data.concentration
      } else {
         # just housekeeping again
         this.SHE.data <- this.data.scale
      }


      ## Electrolyte
      # == We would like to transparently handle the following scenario:
      # || if the user did not specify electrolyte solution (which we can check by using formals())
      # || but the dataset (after subsetting against scale and concentration above) still contains
      # || more than one electrolyte
      # >> Approach: we'll specify a "fallback" electrolyte, KCl (usually that's what the user wants)
      # >> and inform/warn about it

      # KCl is a good assumption, as we always have KCl for the cases where
      # an electrode system has more than one electrolyte
      fallback.electrolyte <- "KCl(aq)"
      if (length(unique(this.SHE.data$electrolyte)) > 1) {
         if (formals(as.SHE)$electrolyte == "") {
            warning(paste0("More than one electrolyte ",
                           "available for E(", dfargs$scale[p], ") in dataset. ",
                           "I'll assume you want ", fallback.electrolyte, "."))
            this.SHE.data <-
               subset(this.SHE.data, electrolyte == fallback.electrolyte)
         } else {
            # else the user did change the electrolyte arg, use the user's value
            this.SHE.data <-
               subset(this.SHE.data, electrolyte == dfargs$electrolyte[p])
            # but stop if the resulting dataframe contains no rows
            if (dim(this.SHE.data)[1] == 0) stop("Your choice of electrolyte does not match any data!")
         }
      } else {
         # dataset contains only one unique electrolyte
         # again, check if electrolyte in arg matches the one in dataset
         # if it does, great, if it does not, print a message and use it anyway
         if (unique(this.SHE.data$electrolyte) == dfargs$electrolyte[p]) {
            this.SHE.data <-
               subset(this.SHE.data, electrolyte == dfargs$electrolyte[p])
         } else {
            # whatever electrolyte the user supplied does not match what's left in the datasubset
            # but at this point the user is probably better served by returning the electrolyte we have
            # along with an informative message (that's the only reason for the if-else below)
            electrolytes.in.subset <-
               unique(subset(as.SHE.data, electrode == dfargs$scale[p])$electrolyte)
            if (dfargs$electrolyte[p] == "") {
               message(
                  paste0('Electrolyte "" (empty string) not in dataset for E(',
                         dfargs$scale[p], '). ',
                         'These electrolytes are: ',
                         paste(electrolytes.in.subset, collapse = ', or '), '.',
                         "I'll assume you want ", fallback.electrolyte, ".")
               )
            } else {
               message(paste0("Electrolyte ", dfargs$electrolyte[p], " not in dataset for E(",
                              dfargs$scale[p], "). ",
                              "These electrolytes are: ",
                              paste(electrolytes.in.subset, collapse = ", or "), ".",
                              "I'll assume you want ", fallback.electrolyte, ".")
               )
            }
         }
      }

      # temperature
      # either happens to match a temperature in the dataset, or we interpolate
      # (under the assumption that potential varies linearly with temperature)
      if (!any(this.SHE.data$temp == dfargs$temperature[p])) {
         # sought temperature was not available in dataset, check that it falls inside
         # note: important to use less/more-than-or-equal in case data only contains one value
         if ((dfargs$temperature[p] <= max(this.SHE.data$temp)) && (dfargs$temperature[p] >= min(this.SHE.data$temp))) {
            # within dataset range, do linear interpolation
            lm.subset <- stats::lm(SHE ~ temp, data = this.SHE.data)
            # interpolated temperature, calculated based on linear regression
            # (more accurate than simple linear interpolation with approx())
            pot.interp <-
               lm.subset$coefficients[2] * dfargs$temperature[p] + lm.subset$coefficients[1]
            ### CALC POTENTIAL vs requested scale
            dfargs$potentialvsscale[p] <-
               ifelse(dfargs$scale[p] == "AVS",
                      pot.interp - dfargs$potential[p],
                      dfargs$potential[p] - pot.interp)
         }
      } else {
         # requested temperature does exist in dataset
         ### CALC POTENTIAL vs requested scale
         dfargs$potentialvsscale[p] <-
            ifelse(dfargs$scale[p] == "AVS",
                   subset(this.SHE.data, temp == dfargs$temperature[p])$SHE - dfargs$potential[p],
                   dfargs$potential[p] - subset(this.SHE.data, temp == dfargs$temperature[p])$SHE)
      }
   }

   return(dfargs$potentialvsscale)
}
