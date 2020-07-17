# uses AEOLUS to produce a single table that contains, for each outcome:
#  demographics, drugs, outcome
outcomes <- read.table(file.path("fdadata", "aeolus_v1", "standard_case_outcome.tsv"),
                       header = FALSE, sep = "\t", na.strings = "\\N",
                       col.names = c("primaryid", "isr", "pt", "outcome_concept_id", "snomed_outcome_concept_id"))
drugs <- read.table(file.path("fdadata", "aeolus_v1", "standard_case_drug.tsv"),
                    header = FALSE, sep = "\t", na.strings = "\\N",
                    col.names = c("primaryid", "isr", "drug_seq", "rol_cod", "standard_concept_id"))

load(file.path("demography_FAERS", "demographics.RData"))

sortedUnion <- function(...) {
  dotsList <- list(...)
  matchedCall <- match.call()
  result <- 
    if (length(dotsList) == 1L) sort(unique(dotsList[[1L]]))
    else {
      matchedCall[length(matchedCall)] <- NULL
      evalEnv <- parent.frame()
      sort(union(eval(matchedCall, evalEnv), sort(unique(tail(dotsList, 1L)[[1L]]))))
    }
  invisible(result)
}

# creating a unique id so we don't have to do everything twice, once for primaryid, once for isr
uniqueISRs       <- sortedUnion(outcomes$isr, drugs$isr, demographics$isr)
uniquePrimaryIds <- sortedUnion(outcomes$primaryid, drugs$primaryid, demographics$primaryid)

uidMap <- data.frame(isr       = c(uniqueISRs, rep_len(NA, length(uniquePrimaryIds))),
                     primaryid = c(rep_len(NA, length(uniqueISRs)), uniquePrimaryIds),
                     uniqueid  = seq_len(length(uniqueISRs) + length(uniquePrimaryIds)))

getUniqueId <- function(isr, primaryid)
  uidMap$uniqueid[ifelse(!is.na(isr), match(isr, uidMap$isr), match(primaryid, uidMap$primaryid))]


drugs <- within(drugs, uniqueid <- getUniqueId(isr, primaryid))

drugSets <- by(drugs, drugs$uniqueid,
               function(drugs.i) sort(unique(drugs.i$standard_concept_id)))
drugSetIds <- as.integer(names(drugSets))
drugSetStrings <- sapply(drugSets, function(concept_ids)
  paste0(concept_ids, collapse = "; "))
rm(drugSets)


outcomes <- within(outcomes, uniqueid <- getUniqueId(isr, primaryid))

outcomeSets <- by(outcomes, outcomes$uniqueid,
                  function(outcomes.i) sort(unique(outcomes.i$outcome_concept_id)))
outcomeSetIds <- as.integer(names(outcomeSets))
outcomeSetStrings <- sapply(outcomeSets, function(concept_ids)
  paste0(concept_ids, collapse = "; "))
rm(outcomeSets)

snowmedOutcomeSets <- by(outcomes, outcomes$uniqueid,
                         function(outcomes.i) sort(unique(outcomes.i$snomed_outcome_concept_id)))
snowmedOutcomeSetIds <- as.integer(names(snowmedOutcomeSets))
snowmedOutcomeSetStrings <- sapply(snowmedOutcomeSets, function(concept_ids) {
  concept_ids <- concept_ids[!is.na(concept_ids)]
  if (length(concept_ids) > 0L) paste0(concept_ids, collapse = "; ") else ""
})
rm(snowmedOutcomeSets)

demographics <- within(demographics, uniqueid <- getUniqueId(isr, primaryid))

faers <- demographics[,c("uniqueid", "primaryid", "isr", "sex", "age"),]

# for any outcomes or drugs that don't have matching demographic info,
# append them at end
orphanDrugs <- !(drugs$uniqueid %in% faers$uniqueid)
if (any(orphanDrugs)) {
  temp <- uidMap[match(unique(drugs$uniqueid[orphanDrugs]), uidMap$uniqueid),]
  temp$sex <- NA
  temp$age <- NA
  temp <- temp[,c("uniqueid", "primaryid", "isr", "sex", "age")]
  faers <- rbind(faers, temp)
  rm(temp)
}
rm(orphanDrugs)

orphanOutcomes <- !(outcomes$uniqueid %in% faers$uniqueid)
if (any(orphanOutcomes)) {
  temp <- uidMap[match(unique(outcomes$uniqueid[orphanOutcomes]), uidMap$uniqueid),]
  temp$sex <- NA
  temp$age <- NA
  temp <- temp[,c("uniqueid", "primaryid", "isr", "sex", "age")]
  faers <- rbind(faers, temp)
  rm(temp)
}
rm(orphanOutcomes)


faers$drugSet <- as.factor(drugSetStrings[match(faers$uniqueid, drugSetIds)])
faers$outcomeSet <- as.factor(outcomeSetStrings[match(faers$uniqueid, outcomeSetIds)])
faers$snowmedOutcomeSet <- as.factor(snowmedOutcomeSetStrings[match(faers$uniqueid, snowmedOutcomeSetIds)])

faers$uniqueid <- NULL

save(faers, file = file.path("fda", "faers.RData"))

