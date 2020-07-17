
# downloads FAERS/LAERS and extracts the demographics
# either run from a directory where you want the files to live, or setwd() first

source(file.path("..", "common", "util.R"))

if (!dir.exists("faers")) dir.create("faers")
if (!dir.exists("laers")) dir.create("laers")

faersReleases <- rbind(
  expand.grid(2014, paste0("q", 1:4)),
  expand.grid(2013, paste0("q", 1:4)),
  c(2012, "q4"))
colnames(faersReleases) <- c("year", "quarter")

printedOnce <- FALSE
for (i in seq_len(nrow(faersReleases))) {
  releaseName <- paste0("faers_ascii_", faersReleases$year[i], faersReleases$quarter[i])
  
  if (!dir.exists(file.path("faers", releaseName))) {
    if (!printedOnce) { cat("downloading FAERS\n"); printedOnce <- TRUE }
    download.file(paste0("https://fis.fda.gov/content/Exports/", releaseName, ".zip"),
                  file.path("faers", paste0(releaseName, ".zip")))
    unzip(file.path("faers", paste0(releaseName, ".zip")), exdir = file.path("faers", releaseName))
  }
}

laersReleases <- rbind(
  expand.grid(2012, paste0("q", 1:3)),
  expand.grid(2011:2004, paste0("q", 1:4)))
colnames(laersReleases) <- c("year", "quarter")
laersReleases <- laersReleases[order(apply(laersReleases, 1, paste0, collapse = ""), decreasing = TRUE),]

printedOnce <- FALSE
for (i in seq_len(nrow(laersReleases))) {
  releaseName <- paste0("aers_ascii_", laersReleases$year[i], laersReleases$quarter[i])
  
  if (!dir.exists(file.path("laers", releaseName))) {
    if (!printedOnce) { cat("downloading LAERS\n"); printedOnce <- TRUE }
    download.file(paste0("https://fis.fda.gov/content/Exports/", releaseName, ".zip"),
                  file.path("laers", paste0(releaseName, ".zip")))
    unzip(file.path("laers", paste0(releaseName, ".zip")), exdir = file.path("laers", releaseName))
  }
}

releases <- rbind(cbind(faersReleases, version = "F"),
                  cbind(laersReleases, version = "L"))
rownames(releases) <- NULL
releases$version <- as.factor(releases$version)

# define this to use dplyr to map all the ages into years
require(dplyr)
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

primaryid <- integer()
isr       <- integer()
numPerRelease <- numeric(nrow(releases))
sex       <- character()
age       <- numeric()
age_code  <- character()

warnedOnce <- FALSE
for (i in seq_len(nrow(releases))) {
  if (releases$version[i] == "F") {
    releasePrefix <- "f"
    pathPrefix    <- "f"
    versionName   <- "FAERS"
  } else {
    releasePrefix <- ""
    pathPrefix    <- "l"
    versionName   <- "LAERS"
  }
  
  releaseName <- paste0(releasePrefix, "aers_ascii_", releases$year[i], releases$quarter[i])
  releasePath <- file.path(paste0(pathPrefix, "aers"), releaseName, "ascii")
  demoFileName <- paste0("DEMO", substr(releases$year[i], 3, 4), toupper(releases$quarter[i]), ".txt")
  
  cat("processing ", versionName, " file ", demoFileName, "\n", sep = "")
  
  # sadly the format changes from year to year and some of the files have an empty column at the
  # end that isn't reflected in the header
  header <- readLines(file.path(releasePath, demoFileName), n = 2)
  colNames <- tolower(unname(sapply(strsplit(header[1], "\\$")[[1]], I)))
  numColumns <- length(gregexpr("\\$", header[2])[[1]]) + 1 # count the number of $s
  if (numColumns > length(colNames)) 
    colNames <- c(colNames, paste0("empty_", sprintf("%.2d", seq_len(numColumns - length(colNames)))))
  
  demographics <- read.table(file.path(releasePath, demoFileName), sep = "$", header = FALSE, quote = "", comment.char = "",
                             skip = 1, col.names = colNames)
  
  # some of the demo files have strange age codes
  if (class(demographics$age) == "factor") {
    oldWarn <- getOption("warn")
    options(warn = -1)
    nonIntegerLevels <- is.na(as.numeric(levels(demographics$age)))
    options(warn = oldWarn)
    warning(versionName, " file ", demoFileName, " has non-integer ages: '",
            paste0(evalx(levels(demographics$age)[nonIntegerLevels], x[x != ""]), collapse = "', '"), "'",
            immediate. = TRUE)
    demographics$age[demographics$age %in% levels(demographics$age)[nonIntegerLevels]] <- NA
    demographics$age <- droplevels(demographics$age)
    demographics$age <- as.numeric(levels(demographics$age))[demographics$age]
  }
  
  # convert all ages to year
  # I'm not a dplyr expert, there might be a cleaner way of handling this
  if (any(levels(demographics$age_cod %not_in% c("", "DEC", "DY", "HR", "MON", "WK", "YR"))))
    stop(versionName, " file ", demoFileName, " contains an unrecognized age code")
  
  demographics <- demographics %>% mutate_cond(age_cod == "DY", age = age / 365.25) %>%
                   mutate_cond(age_cod == "HR", age = age / 365.25 / 24) %>%
                   mutate_cond(age_cod == "MON", age = age / 12) %>%
                   mutate_cond(age_cod == "WK", age = age / (365.25 / 7)) %>%
                   mutate_cond(age_cod == "DEC", age = age * 10)
  
  if (any(!is.na(demographics$age) & demographics$age_cod == "") && !warnedOnce) {
    warning("one or more files have age values with an empty age code; assuming values are in years")
    warnedOnce <- TRUE
  }
  
  sexCol <- if ("sex" %in% colnames(demographics)) "sex" else "gndr_cod"
  evalx(demographics[[sexCol]], { x[x %not_in% c("F", "M")] <- NA; x <- droplevels(x)})
  
  if (releases$version[i] == "F") {
    primaryid <- c(primaryid, demographics$primaryid)
    isr       <- c(isr, rep(NA, nrow(demographics)))
  } else {
    primaryid <- c(primaryid, rep(NA, nrow(demographics)))
    isr       <- c(isr, demographics$isr)
  }
  sex       <- c(sex, as.character(demographics[[sexCol]]))
  age       <- c(age, demographics$age)
  age_code  <- c(age_code, as.character(demographics$age_cod))
  
  numPerRelease[i] <- nrow(demographics)
}

demographics <- data.frame(primaryid, isr, sex = as.factor(sex), age, age_cod_orig = as.factor(age_code))
releaseLabels <- apply(releases, 1, function(row)
  paste0(if (row['version'] == "F") "FAERS" else "LAERS", "_", row['year'], toupper(row['quarter'])))
# adding a release column to help with debugging, since there are some strange age values
# also keeping the original age code, since there are a number of errors
demographics$release <- factor(rep(seq_along(numPerRelease), numPerRelease),
                       labels = releaseLabels[numPerRelease > 0])
  
save(demographics, file = "demographics.RData")
