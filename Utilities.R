not.dropbox.dir <- "/Users/db2175/NonDData/Data/NCI/"


# check patient ids--do they overlap exactly in all sheets
# same for protocol ids
# some duplicate patient ids--how many unique patient/protocol combos

# CHANGE ALL NAMES TO CAPITALS, MAKE SURE ALL NAMES ARE THE SAME
# paste0(protocol, patient)    -- check all combinations[]

# "Proj31BaselineSymptoms.xlsx"     2753x7
# PROTOCOL--14 levels, character     "T99-0047"   no missing value
# PATIENT--468 levels, character     "900186091"  no missing value
# TOXICITY--801 levels, character    "BACK PAIN"  missing value: 67 have " " 
# CTCAE.CODE--318 levels, numeric    "10028411"   missing value: 29 have -2
#        some have codes not in ctcae_4_with_lay_terms, e.g., 
#        705, (DYSPNEA ON EXERTION, DYSPNEA, DYSPNEA EXERTION, 
#             DYSPNEA_(SHORTNESS_OF_BREATH)) 
#        1423 (FATIGUE)
# ONSET.DATE--586 levels, character  "01-Apr-02"  missing value: 795 have " "
#        UNK-UNK-02; UNK-UNK-00; UNK-UNK-01; UNK-UNK-03; UNK-Oct-02, etc...
# GRADE   1 mild,2 moderate,3 severe,4 life-threatening     missing value: 2 has -2
# RELATION       (Related to disease--Y[es], N[o], U[nknown]
#       " "   *   N     U    Y 
#       72    1  903  556 1221 

# "Proj31ClinicalOutcome.xlsx"      486x7
# PROTOCOL--14 levels, character
# PATIENT--468 levels, character
# Date.of.Best.Response    01-DEC-03     no UNK  missing value: 302 have " "
# BEST_RESPONSE    11 levels, character (PROGRESSIVE DISEASE, etc...)   
#           some coded missing values (TOO EARLY, NOT EVALUABLE)
#           3 have " ", 2 have *

#             Not applicable per protocol
#             too early to assess per protocol
#             not assessed
#             not evaluable
#             complete response
#             partial response
#             less than partial response
#             progressive disease 
#             stable disease
# Date.of.progression   264 levels, character    missing value: 198 have " "
#           no Unk
# ClinicalOutcomeOthersResolved has fewer Others in OFF.STUDY.REASON2, filled in based 
#   on conversation by Shing with NCI or Theradex
# OFF.STUDY.REASON
#   Complicating Disease/Intercurrent Illness
#   Death during Follow-up Period
#   Death on Study
#   Declined to participate
#   Disease Progression On Study
#   No Treatment - Per Protocol
#   Not Treated - Other Reasons
#   Other
#   Protocol Violation
#   Refused Further Treatment
#   Study Complete: No Protocol-Specified Follow-up
#   Switched to Alternative Treatment
#   Toxicity/Side Effects
# DATE_OFF_PROTOCOL     486 values, character no missing data
# Date.of.death   123 unique values, character      356 have " "

# "Proj31Dosing.xlsx     6039x13
# PROTOCOL    14 levels, character
# Patient.Id  468 LEVELS, character       
#         Patient COH-304 has 309 entries
# Course.Number   numeric, 1 through 42 
# Course.Start.Date        06-NOV-07    no missing values        (REMOVED FROM NEW VERSION OF DATA)
# Drug.Administration.Date    01-APR-08     no missing values
# Drug.Administration.Time   1135   845     
#           1901 rows have -2, 93 have 0, 1 has 55
# DRUG    40 levels
# Dose.Level   numeric, 64 levels
# Dose.Level.Unit   character, 22 levels
# Actual.Dose.Level   numeric, 668 levels
# Actual.Dose.Level.Unit   character, 7 levels
# Dose.modification    2821 have " "; 16 have 0; 7 have UNKNOWN   (REMOVED FROM NEW VERSION OF DATA)
#     No
#     Yes, planned
#     Yes, unplanned
#     Unknown


# ROUTE               1 has " ", 1 has 1, 4 have 1V, 1 has IP

# "Proj31MedicalHistory.xlsx   488x39
# PROTOCOL   14 levels
# PATIENT   468 levels
# EXAMINATION.DATE
# ...NECK   NECK_DETAILS ....etc...

# Proj31Patients.xlsx    486x13
# some patients have two rows    (001, 002, 003--same patient, different protocols)
# PROTOCOL    14 values
# PATIENT     468 values
# AGE         numeric, no missing
# GENDER      221 F, 265 M
# HEIGHT      one patient has -2
# HEIGHT.UNIT
# WEIGHT      one patient has -2
# WEIGHT.UNIT 
# PERFROMANCE_STATUS    0, 1, 2, 60, 70, 80, 90, 100    ?
# PRIMARY.SITE     no missing, 5 coded UNKNOWN, 1 UNKNOWN PRIMARY
# HISTOLOGY
# STAGE    120 have " ", 3 have UNK, have IV, 4 and STAGE 4, other duplicate codings
#          goes up to VI, includes METS, "T2 N2B"
# ONSTUDY      01-JUN-01  

# Proj31Toxicities.xlsx 13008x15
# Protocol        14 levels
# Patient         462 levels
# Course.Number   1-42
# Course.Start.Date   02-MAR-05
# AE.Description   FATIGUE etc...
# CTCAE.CODE     485 unique values  656 have -2     86 have 5765  (NEUTROPHILS/GRANULOCYTES); some with same description have code 10029363 
# Onset.Date     206 have " "    2399 unique values
# Resolved.Date   2206 unique values     4310 have " "
# Grade       2 have -2    others have 1 through 5
#    1 mild 2 moderate 3 severe 4 life-threatening 5 fatal
# Attribution (relation to study drug) 
#       1 unrelated 2 unlikely 3 possible 4 probable 5 definite
# DLT      Y (142), 841 have " ", 12025 have N
# Serious     1,2,3,4,5,7, 771 have -2
#   1 no 2 life-threatening 3 death 4 disability 5 hospitalization 6 caused congenital anomaly 
#   7 required intervention to prevent permanent impairment
# Action    36 have " ", 2 have "*", others have 1-5
#    1 none 2 dose reduced 3 regimen interrupted 4 therapy discontinued 5 interrupted then reduced
# Therapy      1 through 4, 4 least frequent
#    1 none 2 symptomatic 3 supportive 4 vigorous
# Outcome      153 have " ", 25 have "*", others have 1-4
#     1 recovered   2 still under treatment/observation 3 alive with sequelae 4 died

# load data
Load <- function(){
  baseline.symptoms <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31BaselineSymptoms.tsv"), stringsAsFactors=F)
  clinical.outcome <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31ClinicalOutcomeOthersResolved.tsv"), stringsAsFactors=F)
  dosing <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31Dosing.tsv"), stringsAsFactors=F)
  patients <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31Patients.tsv"), stringsAsFactors=F)
  toxicities <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31Toxicities.tsv"), stringsAsFactors=F)
  medical.history <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31MedicalHistory.tsv"), stringsAsFactors=F)
}

GetClinicalOutcome <- function(){
  clinical.outcome <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31ClinicalOutcomeOthersResolved.tsv"), 
                                 stringsAsFactors=F)
  names(clinical.outcome) <- c("PROTOCOL", "PATIENT", "DATE.OF.BEST.RESPONSE", "BEST_RESPONSE", 
                               "DATE.OF.PROGRESSION", "OLD.OFF.STUDY.REASON", "DATE_OFF_PROTOCOL", 
                               "DATE.OF.DEATH", "UID", "AGENT", "STUDY.ID", "RECODED.OFF.STUDY.REASON")
  clinical.outcome
}

tsv <- function(f){
  f <- gsub(".xlsx", ".tsv", f)
  # new files sent March 20 have .xls extension, earlier files have .xlsx extension
  f <- gsub(".xls", ".tsv", f)
}

GetPatients <- function(phase.1.only=T){
  f <- paste0(not.dropbox.dir, "/Data_tsv/Proj31Patients.tsv")
  #cat("reading", f, "\n")
  patients <- read.delim(f, stringsAsFactors=F)
}

Phase1PatientsUIDs <- function(){
  pat <- GetPatients(phase.1.only = T)
  unique(pat$UID)
}

GetPatientNumbers <- function(recompute=F, phase.1.only=T){
  f <- paste0(not.dropbox.dir, "/PatientNumbers.txt")
  if (recompute){
    library(plyr)
    patients <- GetPatients()
#     library(sas7bdat)
#     swog327 <- read.sas7bdat(paste(not.dropbox.dir, "/Phase2Data/tx0327.sas7bdat"))
#     swog339 <- read.sas7bdat(paste0(not.dropbox.dir, "/Phase2Data/tx0339.sas7bdat"))
#     pswog <- data.frame(PROTOCOL=c("S0327", "S0339"), 
#                         NUM.PATIENTS=c(length(unique(swog327$PATNO)), 
#                                        length(unique(swog339$PATNO))))
    p <- ddply(patients, "PROTOCOL", function(x){c(NUM.PATIENTS=nrow(x))})
    # p <- rbind(p, pswog)
    rownames(p) <- p$PROTOCOL
    write.table(p, f, sep="\t", quote=F)
  }
  #cat("reading", f, "\n")
  p.numbers <- read.table(f, header=T, sep="\t", stringsAsFactors=F)
  if (phase.1.only){
    p.numbers <- subset(p.numbers, PROTOCOL %in% Phase1Studies())
  }
  return(p.numbers)
}

GetPatientNumbersDosed <- function(recompute=F, phase.1.only=T){
  f <- paste0(not.dropbox.dir, "/PatientNumbersDosed.txt")
  if (recompute){
    p <- GetPatientNumbers()
    d <- GetRecodedDosing()
    library(plyr)
    yesdose <- ddply(subset(d, RECODED.DRUG=="BORTEZOMIB"), c("UID", "PROTOCOL"), function(x){any(!is.na(x$RECODED.DOSE.LEVEL))})
    withdose <- ddply(yesdose, "PROTOCOL", function(x){c("NUM.PATIENTS.DOSED"=sum(x$V1))})
    pswog <- subset(p, PROTOCOL %in% c("S0327", "S0339"))
    pswog$NUM.PATIENTS.DOSED <- pswog$NUM.PATIENTS
    dd <- merge(p, withdose)
    dd <- rbind(dd, pswog)
    write.table(dd, f, sep="\t", row.names=F, quote=F)
  }
  #cat("reading", f, "\n")
  pn <- read.table(f, header=T, sep="\t", stringsAsFactors=F)
  if (phase.1.only){
    pn <- subset(pn, PROTOCOL %in% Phase1Studies())
  }
  pn
}

# opens raw dosing file, makes consistent drug name column, DRUG2
# makes dose levels consistent in new column, DOSE.LEVEL.UNIT2
# makes actual dose levels consistent in new column, ACTUAL.DOSE.LEVEL.UNIT2
# removes five rows with NA (patients never dosed)
GetRecodedDosing <- function(phase.1.only=T){
  dosing <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31Dosing.tsv"), stringsAsFactors=F)
  drug.names <- read.table(paste0(not.dropbox.dir, "/drug_names.txt"), 
             col.names=c("OLD", "NEW"), 
             sep="\t", stringsAsFactors=F)
  dn2 <- data.frame(OLD=c("FFU", "PS314"), NEW=c("5-FU", "BORTEZOMIB"))
  drug.names <- rbind(drug.names, dn2)
  dict <- drug.names$NEW
  names(dict) <- drug.names$OLD
  dosing$DRUG2 <- dict[dosing$DRUG]
  # clean up units
  dict2 <-        c(NA, "AUC", "mg", "mg", "mg", "mg",  "mg/m2", "mg/m2", "ng")
  names(dict2) <- c("", "AUC", "mg", "Mg", "MG", "MG.", "mg/m2", "MG/M2", "mg")
  dict1 <-         c(NA, "mg/m2",    NA,  "mg/m2", "mg", "mg", "mg/m2", "mg/m2", "mg/m2", "mg/m2", "mg/m2", "mg/m2", "mg/m2",  "mg/m2", "mg/m2", "mg/m2",   "AUC", "auc", "mg/kg", "MG.m2", "MGm2",   "mg/m", "mgm2")
  names(dict1) <- c("",  ",g/M2", "1.4", "1mg/m2", "mg", "MG", "mg.m2", "mg/m2", "Mg/m2", "Mg/M2", "MG/m2", "MG/M2", "MG/MG2", "mg/ms", "mg/n2", "25 mg/m2","AUC", "AUC", "mg/kg", "mg/m2", "mg/m2", "mg/m2", "mg/m2")
  dosing$DOSE.LEVEL.UNIT2 <- dict1[dosing$DOSE.LEVEL.UNIT]
  dosing$ACTUAL.DOSE.LEVEL.UNIT2 <- dict2[dosing$ACTUAL.DOSE.LEVEL.UNIT]
  dosing$DRUG.ADMINISTRATION.DATE2 <- as.Date(dosing$DRUG.ADMINISTRATION.DATE, 
                                              format="%d-%b-%y")
  #library(gdata)
  #old.dosing <- read.xls("/Users/daniel/NonDData/Data/NCI/OldOriginalFiles/Proj31Dosing.xlsx", sheet=1, stringsAsFactors=F)
  # FIX ERRORS
  dosing <- dosing[!is.na(dosing$DOSE.LEVEL), ]
  dosing[dosing$PROTOCOL=="5326" & dosing$DOSE.LEVEL==0.74 & dosing$DRUG2=="BORTEZOMIB", "DOSE.LEVEL"] <- 0.75
  dosing[dosing$UID=="6520:908010924" & dosing$DOSE.LEVEL==0.07, "DOSE.LEVEL"] <- 0.7
  dosing[dosing$UID=="1857:950001090" & dosing$DRUG2=="BORTEZOMIB" & dosing$DRUG.ADMINISTRATION.DATE=="12-OCT-04", "DOSE.LEVEL"] <- 0.8
  # 6126:013
  new.colnames <- c("PROTOCOL", "PATIENT", "COURSE.NUMBER", "OLD.DRUG.ADMINISTRATION.DATE", 
                        "DRUG.ADMINISTRATION.TIME", "OLD.DRUG", "RECODED.DOSE.LEVEL", 
                        "OLD.DOSE.LEVEL.UNIT", "ACTUAL.DOSE.LEVEL", "OLD.ACTUAL.DOSE.LEVEL.UNIT", 
                    "ROUTE", "UID", "AGENT", "STUDY.ID", "RECODED.DRUG", "RECODED.DOSE.LEVEL.UNIT", 
                    "RECODED.ACTUAL.DOSE.LEVEL.UNIT", "RECODED.DRUG.ADMINISTRATION.DATE")
  colnames(dosing) <- new.colnames
  dosing
}

# this was used to clean up the toxicities after excel messed up all the UIDs
MakeRecodedToxicities <- function(){
  toxg1 <- read.delim("/Users/daniel/NonDData/Data/NCI/reviewed.recoded.toxicities.txt", 
                           stringsAsFactors=F)
  #tox <- subset(toxicities, !FINAL.DESCR=="" & !is.na(GRADE))
  otox <- GetToxicities()
  same.uids <- intersect(otox$UID, toxg1$UID)
  uids <- unique(otox$UID)
  #uids <- setdiff(unique(otox$UID), "T99-0047:9909-N016")
  names(uids)[uids %in% same.uids] <- same.uids
  amb <- subset(toxg1, !UID %in% same.uids)
  o.amb <- subset(otox, !UID %in% same.uids)
  amb$code <- with(amb, paste(AE.DESCRIPTION, CTCAE.CODE, GRADE, SERIOUS, ACTION, THERAPY, OUTCOME))
  o.amb$code <- with(o.amb, paste(AE.DESCRIPTION, CTCAE.CODE, GRADE, SERIOUS, ACTION, THERAPY, OUTCOME))
  # for each individual find intersection of uids of all codes
  for (uid in unique(amb$UID)){
    o.cc.uid <- unique(o.amb$UID)
    codes <- unique(subset(amb, UID==uid)$code)
    for (cc in codes){
      o.cc.uid <- intersect(o.cc.uid, subset(o.amb, code==cc)$UID)
    }
    names(uids)[uids == o.cc.uid] <- uid
  }
  toxg1$UID <- uids[toxg1$UID]
  toxg2 <- read.delim("/Users/daniel/NonDData/Data/NCI/uncategorizedfortheradex-resolved.txt", 
                            stringsAsFactors=F)
  library(gdata)
  options(stringsAsFactors=F)
  CTCAEv4 <- read.xls("/Users/daniel/Dropbox/Daniel/Biostatistics/NCI-DoseFinding/CTCAE/CTCAE_4.03_2010-06-14.xls", header=T, sheet=1, check.names=F)
  terms <- CTCAEv4[, "CTCAE v4.0 Term"]
  codes <- CTCAEv4[, "MedDRA v12.0 Code"]
  names(codes) <- toupper(terms)
  toxg2$CTCAE..NEW.CAT <- toupper(toxg2$CTCAE..NEW.CAT)
  u <- unique(toxg2$CTCAE..NEW.CAT)
  print(u[!u %in% names(codes)])
  # turn space into NA
  names(toxg2)[names(toxg2)=="CTCAE..NEW.CAT"] <- "FINAL.DESCR"
  uid.dict <- toxg1
  uid.dict$BADUID <- paste(uid.dict$PROTOCOL, uid.dict$PATIENT, sep=":")
  uid.dict <- uid.dict[, c("UID", "BADUID")]
  uid.dict <- unique(uid.dict)
  uid.dict.vector <- uid.dict$UID
  names(uid.dict.vector) <- uid.dict$BADUID
  toxg2$BADUID <- paste(toxg2$PROTOCOL, toxg2$PATIENT, sep=":")
  toxg2$UID <- uid.dict.vector[toxg2$BADUID]
  keep.columns <- c("COURSE.NUMBER", "COURSE.START.DATE", "AE.DESCRIPTION", 
                    "CTCAE.CODE", "ONSET.DATE", "RESOLVED.DATE", 
                    "GRADE", "ATTRIBUTION", "DLT", "SERIOUS", 
                    "ACTION", "THERAPY", "OUTCOME", "UID", "FINAL.DESCR")
  toxg1$FINAL.CTCAE <- codes[toxg1$FINAL.DESCR]
  toxg1 <- toxg1[!is.na(toxg1$FINAL.CTCAE), ]
  toxs1 <- toxg1[, keep.columns]
  toxs2 <- toxg2[, keep.columns]
  tox <- rbind(toxs1, toxs2)
  for (u in unique(tox$UID)){
    new.codes <- unique(subset(tox, UID==u)$AE.DESCRIPTION)
    old.codes <- unique(subset(otox, UID==u)$AE.DESCRIPTION)
    if (!length(new.codes) == length(intersect(new.codes, old.codes))){
      browser()
    }
  }
  patients <- read.delim("/Users/daniel/NonDData/Data/NCI/Data_tsv/Proj31Patients.tsv", stringsAsFactors=F)
  patients <- patients[, c("UID", "PROTOCOL", "PATIENT", "AGENT", "STUDY.ID")]
  tox <- merge(tox, patients)
  write.table(tox, "/Users/daniel/NonDData/Data/NCI/merged.recoded.toxicities.txt", sep="\t", row.names=F, quote=F)
}

Phase2Studies <- function(){
  c("S0327", "S0339")
}

Phase1Studies <- function(){
  dosing <- GetMaxCycles()
  setdiff(unique(dosing$PROTOCOL), Phase2Studies())
}

# returns dataframe with PROTOCOL, UID and MAX.CYCLE 
# (determined from dosing information)
# for Phase I and Phase II combined
GetMaxCycles <- function(recompute=F, phase.1.only=T){
  f <- paste0(not.dropbox.dir, "PatientMaxCycles.txt")
  if (recompute){
    library(plyr)
    dosing <- GetRecodedDosing()
    swog327 <- read.sas7bdat("/Users/daniel/NonDData/Data/NCI/Phase2Data/tx0327.sas7bdat")
    swog339 <- read.sas7bdat("/Users/daniel/NonDData/Data/NCI/Phase2Data/tx0339.sas7bdat")
    cols1 <- c("STUDYID", "CYCLENO", "PATNO")
    swog <- rbind(swog327[, cols1], swog339[, cols1])
    names(swog)[names(swog)=="STUDYID"] <- "PROTOCOL"
    names(swog)[names(swog)=="CYCLENO"] <- "COURSE.NUMBER"
    swog$UID <- paste0(swog$PROTOCOL, ":", swog$PATNO)
    cols <- c("PROTOCOL", "UID", "COURSE.NUMBER")
    dd <- rbind(dosing[, cols], swog[, cols])
    pat.max.cycle <- ddply(dd, c("PROTOCOL", "UID"), 
                           function(x){c(MAX.CYCLE=max(x$COURSE.NUMBER))})
    write.table(pat.max.cycle, f, sep="\t", row.names=T, quote=F)
  }
  #cat("reading", f, "\n")
  max.cycles <- read.table(f, header=T, sep="\t", stringsAsFactors=F)
  if (phase.1.only){
    max.cycles <- subset(max.cycles, UID %in% Phase1PatientsUIDs())
  }
  return(max.cycles)
}

GetMedianCycles <- function(recompute=F, phase.1.only=T){
  if (recompute){
    library(plyr)  
    patients <- read.delim("/Users/daniel/NonDData/Data/NCI/Data_tsv/Proj31Patients.tsv", stringsAsFactors=F)
    prot.patients <- ddply(patients, "PROTOCOL", function(x) c("Num.Patients"=nrow(x)))
    dosing <- read.delim("/Users/daniel/NonDData/Data/NCI/Data_tsv/Proj31Dosing.tsv", stringsAsFactors=F)
    pat.max.cycle <- ddply(dosing, c("PROTOCOL", "UID"), 
                           function(x){max(x$COURSE.NUMBER)})
    prot.med.cycle <- ddply(pat.max.cycle, "PROTOCOL", function(x)c("MED.CYCLE"=median(x$V1, na.rm=T)))
    swog327 <- read.sas7bdat("/Users/daniel/NonDData/Data/NCI/Phase2Data/tx0327.sas7bdat")
    swog339 <- read.sas7bdat("/Users/daniel/NonDData/Data/NCI/Phase2Data/tx0339.sas7bdat")
    swog327.med <- ddply(swog327, "PATNO", function(x){c(NUMCYCLES=max(x$CYCLENO))})
    swog339.med <- ddply(swog339, "PATNO", function(x){c(NUMCYCLES=max(x$CYCLENO))})
    pswog <- data.frame(PROTOCOL=c("S0327", "S0339"), 
                        MED.CYCLE=c(median(swog327.med$NUMCYCLES), 
                                       median(swog339.med$NUMCYCLES)))
    prot.med.cycle <- rbind(prot.med.cycle, pswog)
    med.cycle <- prot.med.cycle$MED.CYCLE
    names(med.cycle) <- prot.med.cycle$PROTOCOL
    write.table(med.cycle, "/Users/daniel/NonDData/Data/NCI/MedianNumCycles.txt", sep="\t", row.names=T, quote=F)
  }
  b <- read.table(paste0(not.dropbox.dir, "/MedianNumCycles.txt"), header=T, sep="\t", stringsAsFactors=F)
  d <- b$x
  names(d) <- rownames(b)
  if (phase.1.only){
    d <- d[names(d) %in% Phase1Studies()]
  }
  return(d)
}

GetHematologicCategories <- function(){
  tox <- read.delim("/Users/daniel/NonDData/Data/NCI/merged.recoded.toxicities.txt", stringsAsFactors=F)
  hematologic <- c("WHITE BLOOD CELL DECREASED", "PLATELET COUNT DECREASED",
                   "BLOOD AND LYMPHATIC SYSTEM DISORDERS - OTHER, SPECIFY",
                   "NEUTROPHIL COUNT DECREASED", "LYMPHOCYTE COUNT DECREASED", 
                   "ANEMIA", "FEBRILE NEUTROPENIA", 
                   "DISSEMINATED INTRAVASCULAR COAGULATION")
  hematologic <- c(hematologic, grep("HEMORRHAGE", unique(tox$FINAL.DESCR), value=T))
  return(hematologic)
}

GetSWOGtoxicities <- function(){
  library(sas7bdat)
  swog327tox <- read.sas7bdat("/Users/daniel/NonDData/Data/NCI/Phase2Data/ae0327.sas7bdat")
  swog339tox <- read.sas7bdat("/Users/daniel/NonDData/Data/NCI/Phase2Data/ae0339.sas7bdat")
  swog327tox$AGENT <- "BORTEZOMIB"
  swog339tox$AGENT <- "BORTEZOMIB+CARBOPLATIN+GEMCITABINE"
  swog327tox$PROTOCOL <- "S0327"
  swog339tox$PROTOCOL <- "S0339"
  swog <- rbind(swog327tox, swog339tox)
  swog$STUDY.ID <- paste0(swog$AGENT, "-", swog$PROTOCOL)
  swog$UID <- paste0(swog$PROTOCOL, ":", swog$PATNO)
  swog <- swog[, c("PATNO", "TOXDEG", "TOXCYC", "TOXLABEL", "TXATT", "UID", "PROTOCOL", "AGENT", "STUDY.ID")]
  names(swog) <- c("PATIENT", "GRADE", "COURSE.NUMBER", "FINAL.DESCR", "ATTRIBUTION", "UID", "PROTOCOL", "AGENT", "STUDY.ID")
  swog$ATTRIBUTION <- as.character(swog$ATTRIBUTION)
  swog$GRADE <- as.character(swog$GRADE)
  swog$FINAL.DESCR <- as.character(swog$FINAL.DESCR)
  hem.descr <- c("Hemoglobin", "Platelets", 
                 "Neutrophils", "Lymphopenia", 
                 "Febrile neutropenia", 
                 "Coagulation-other",
                 grep("emorrhage", unique(swog$FINAL.DESCR), value=T))
  swog$HEMATOLOGIC <- swog$FINAL.DESCR %in% hem.descr
  swog$ATTRIBUTION <- as.numeric(gsub("P", "", swog$ATTRIBUTION))
  cat("There are ", nrow(swog), "Phase 2 toxicities\n")
  swog <- subset(swog, !is.na(ATTRIBUTION) & !(GRADE==9) & !is.na(COURSE.NUMBER) & COURSE.NUMBER > 0)
  cat("There are", nrow(swog), "Phase 2 toxicities after removing toxicities with missing data\n")
  swog$ATTRIBUTION <- mapvalues(swog$ATTRIBUTION, from=c(1:5), 
                                to=c("unrelated", "unlikely", "possible", "probable", "definite"))
  swog$PHASE <- 2
  return(swog)
}

# includes PHASE variable indicating whether study is Phase 1 or Phase 2
# there are 4297 toxicities in the raw SWOG toxicity files, 3511 for study 339
# and 786 for study 327
# 15 are excluded for having NA for ATTRIBUTION (n=5), GRADE (n=1), 
# or COURSE.NUMBER (n=8), or having COURSE.NUMBER=0 (n=2)
# reads in 13008 toxicities for the Phase 1 studies, 
# and excludes 19 for having OUR.DLT.REL==NA (see GetRecodedToxicities())
# merges them together
# columns with NA for the Phase 2 studies are 
#    COURSE.START.DATE
#    ONSET.DATE
#    RESOLVED.DATE
#    DLT
GetMergedToxicities <- function(recompute=F, phase.1.only=T){
  if (recompute){
    swog <- GetSWOGtoxicities()
    tox <- GetRecodedToxicities()
    ttox <- subset(tox, !is.na(OUR.DLT.REL))
    library(plyr)
    ttox$PHASE <- 1
    ttox <- ttox[, !colnames(ttox) %in% c("AE.DESCRIPTION", 
                                          "CTCAE.CODE", 
                                          "SERIOUS", "THERAPY", 
                                          "OUTCOME", "OUR.DLT.REL")]
    ttox <- merge(ttox, swog, all=T)
    write.table(ttox, "/Users/daniel/NonDData/Data/NCI/merged.toxicities.txt", 
                sep="\t", row.names=F, quote=F)
  }
  else {
    ttox <- read.table(paste0(not.dropbox.dir, "merged.toxicities.txt"), header=T, stringsAsFactors=F, sep="\t")
  }
  if (phase.1.only){
    ttox <- subset(ttox, UID %in% Phase1PatientsUIDs())
  }
  return(ttox)
}


# not used
GetWorseningConsistentlyCoded <- function(recompute=F){
  if(recompute){
    ttox <- GetMergedToxicities()
#     con.tox.l <- dlply(ttox, c("UID", "FINAL.DESCR"),
#                        function(x){
#                          if (length(intersect(x[['ATTRIBUTION']], c("unrelated", "unlikely")))==0 |
#                                length(intersect(x[['ATTRIBUTION']], c("probable", "possible", "definite")))==0){
#                            return(x)
#                          }
#                        })
#     con.tox <- do.call('rbind', con.tox.l)
    # get highest grade event in each course
    con.tox.max <- ddply(ttox, c("UID", "FINAL.DESCR", "COURSE.NUMBER"), 
                         function(x){
                           return(x[which.max(x$GRADE), ])
                         })
    wors.summ <- ddply(con.tox.max, c("UID", "FINAL.DESCR"), 
                       function(x){
                         first.cyc <- subset(x, COURSE.NUMBER==1)
                         c(FIRST.CYC= ifelse(1 %in% x[['COURSE.NUMBER']], "TRUE", "FALSE"), 
                           FIRST.CYC.GRADE=ifelse(!is.null(first.cyc), first.cyc$GRADE, NA),
                           TR.REL=length(intersect(x[['ATTRIBUTION']], 
                                                   c("probable", "possible", "definite")))>0, 
                           MAX.GRADE=max(x[['GRADE']]))
                       })
    write.table(wors.summ, "/Users/daniel/NonDData/Data/NCI/worsening.consistently.coded.txt", 
                sep="\t", row.names=F, quote=F)
  } else {
    wors.summ <- read.table("/Users/daniel/NonDData/Data/NCI/worsening.consistently.coded.txt", 
                           stringsAsFactors=F, header=T, sep="\t")
  }
  wors.summ
}

# reads in 17271 toxicities from phase 1 and phase 2
# removes toxicities unrelated or unlikely to be related to treatment (reduces to 10743)
# keeps from each patient, cycle and toxicity type the highest grade
# and within the highest grade the highest attribution
# adds an OUR.DLT.REL variable
GetRecodedToxicitiesTrRelMaxInCycle <- function(recompute=F, phase.1.only=F){
  if (recompute){
    ttoxa <- GetMergedToxicities(T)
    # 142 DLTs before this step, 125 after remove unrelated and unlikely
    ttox <- subset(ttoxa, ATTRIBUTION %in% c("possible", "probable", "definite"))
    tox <- ddply(ttox, c("UID", "COURSE.NUMBER", "FINAL.DESCR"),
                 function(x){
                   maxg <- max(x$GRADE)
                   rets <- subset(x, GRADE==maxg)
                   if (any(rets[['ATTRIBUTION']]=="definite")){
                     return(head(subset(rets, ATTRIBUTION=="definite"), 1))
                   }
                   if (any(rets[['ATTRIBUTION']]=="probable")){
                     return(head(subset(rets, ATTRIBUTION=="probable"), 1))
                   }
                   if (any(rets[['ATTRIBUTION']]=="possible")){
                     return(head(subset(rets, ATTRIBUTION=="possible"), 1))
                   }
                 })
    tox$OUR.DLT.REL <- tox$ATTRIBUTION %in% c("possible", "probable", "definite") & 
      ((tox$GRADE >= 3 & !tox$HEMATOLOGIC) | (tox$GRADE >= 4))
    write.table(tox, "/Users/daniel/NonDData/Data/NCI/merged.recoded.tr.rel.toxicities.maxincycle.phase1and2.txt", 
                sep="\t", row.names=F, quote=F)
    browser()
  }
  else {
    tox <- read.table(paste0(not.dropbox.dir, "/merged.recoded.tr.rel.toxicities.maxincycle.phase1and2.txt"), header=T, stringsAsFactors=F, sep="\t")
  }
  if (phase.1.only){
    tox <- subset(tox, UID %in% Phase1PatientsUIDs())
  }
  tox
}

GetRecodedToxicitiesMaxInCycle <- function(recompute=F, phase.1.only=F){
  f <- paste0(not.dropbox.dir, 
              "/merged.recoded.toxicities.maxincycle.phase1and2.txt")
  if (recompute){
    ttoxa <- GetMergedToxicities(F)
    # 142 DLTs before this step, 125 after remove unrelated and unlikely
    tox <- ddply(ttoxa, c("UID", "COURSE.NUMBER", "FINAL.DESCR"),
                 function(x){
                   maxg <- max(x$GRADE)
                   rets <- subset(x, GRADE==maxg)
                   if (any(rets[['ATTRIBUTION']]=="definite")){
                     return(head(subset(rets, ATTRIBUTION=="definite"), 1))
                   }
                   if (any(rets[['ATTRIBUTION']]=="probable")){
                     return(head(subset(rets, ATTRIBUTION=="probable"), 1))
                   }
                   if (any(rets[['ATTRIBUTION']]=="possible")){
                     return(head(subset(rets, ATTRIBUTION=="possible"), 1))
                   }
                   if (any(rets[['ATTRIBUTION']]=="unlikely")){
                     return(head(subset(rets, ATTRIBUTION=="unlikely"), 1))
                   }
                   if (any(rets[['ATTRIBUTION']]=="unrelated")){
                     return(head(subset(rets, ATTRIBUTION=="unrelated"), 1))
                   }
                 })
    tox$OUR.DLT.REL <- tox$ATTRIBUTION %in% c("possible", "probable", "definite") & 
      ((tox$GRADE >= 3 & !tox$HEMATOLOGIC) | (tox$GRADE >= 4))
    write.table(tox, f, 
                sep="\t", row.names=F, quote=F)
  }
  else {
    #cat("Reading", f, "\n")
    tox <- read.table(f, header=T, stringsAsFactors=F, sep="\t")
  }
  if (phase.1.only){
    tox <- subset(tox, UID %in% Phase1PatientsUIDs())
  }
  tox
}

# returns recoded toxicities for Phase I studies (13,008)
# same as number of toxicities in Theradex toxicities file for Phase I
# includes HEMATOLOGIC variable and OUR.DLT.REL variable
# OUR.DLT.REL is NA if any of ATTRIBUTION, GRADE or FINAL.DESCR are NA (n=19)
#    16 have NA FINAL.DESCR
#    2 have NA GRADE
#    1 has NA attribution
#    none of these are protocol defined DLTs
#      subset(tox, is.na(ATTRIBUTION) | is.na(GRADE) | is.na(FINAL.DESCR))$DLT
GetRecodedToxicities <- function(){
  tox <- read.delim(paste0(not.dropbox.dir, "merged.recoded.toxicities.txt"), stringsAsFactors=F)
  hematologic <- c("WHITE BLOOD CELL DECREASED", "PLATELET COUNT DECREASED",
                   "BLOOD AND LYMPHATIC SYSTEM DISORDERS - OTHER, SPECIFY",
                   "NEUTROPHIL COUNT DECREASED", "LYMPHOCYTE COUNT DECREASED", 
                   "ANEMIA", "FEBRILE NEUTROPENIA", 
                   "DISSEMINATED INTRAVASCULAR COAGULATION")
  hematologic <- c(hematologic, grep("HEMORRHAGE", unique(tox$FINAL.DESCR), value=T))
  #cat("Hematologic categories: ", paste(hematologic, collapse=", "), "\n")
  related <- c("possible", "probable", 
               "definite")
  tox$HEMATOLOGIC <- tox$FINAL.DESCR %in% hematologic
  tox$FINAL.DESCR[tox$FINAL.DESCR==""] <- NA
  tox$OUR.DLT.REL <- tox$ATTRIBUTION %in% related & 
    ((tox$FINAL.DESCR %in% hematologic & tox$GRADE >= 4) | 
       (!tox$FINAL.DESCR %in% hematologic & tox$GRADE >= 3))
  tox$OUR.DLT.REL[is.na(tox$ATTRIBUTION) | is.na(tox$GRADE) | is.na(tox$FINAL.DESCR)] <- NA
  tox
}

GetToxicities <- function(){
  toxicities <- read.delim(paste0(not.dropbox.dir, "/Data_tsv/Proj31Toxicities.tsv"), stringsAsFactors=F)
  library(plyr)
  # Grade       2 have -2    others have 1 through 5
  #    1 mild 2 moderate 3 severe 4 life-threatening 5 fatal
  toxicities$GRADE[toxicities$GRADE==-2] <- NA
  #toxicities$GRADE <- revalue(as.character(toxicities$GRADE), 
  #          c("1"="mild", "2"="moderate", 
  #            "3"="severe", "4"="life-threatening", 
  #            "5"="fatal", "-2"=NA))
  #toxicities$GRADE <- factor(toxicities$GRADE, 
  #          levels = c(NA, "mild", "moderate", "severe", "life-threatening", "fatal"), exclude=NULL, ordered=T)
  # Attribution (relation to study drug) 
  #       1 unrelated 2 unlikely 3 possible 4 probable 5 definite
  toxicities$ATTRIBUTION <- revalue(as.character(toxicities$ATTRIBUTION),
            c("1"="unrelated", "2"="unlikely", 
              "3"="possible", "4"="probable", 
              "5"="definite"))
  toxicities$ATTRIBUTION <- factor(toxicities$ATTRIBUTION, 
            c("unrelated", "unlikely", "possible", "probable", "definite"), exclude=NULL)
  # Action    36 have " ", 2 have "*", others have 1-5
  #    1 none 2 dose reduced 3 regimen interrupted 4 therapy discontinued 5 interrupted then reduced
  new_names <- c("none", "dose reduced", "regimen interrupted", 
                 "therapy discontinued", "interrupted then reduced", NA)
  toxicities$ACTION <- mapvalues(as.character(toxicities$ACTION), 
            c("1","2","3","4","5"," ", "", "*"), 
            c(new_names, NA, NA))
  toxicities$ACTION <- factor(toxicities$ACTION, new_names, exclude=NULL)
  # DLT      Y (142), 841 have " ", 12025 have N
  toxicities$DLT <- mapvalues(as.character(toxicities$DLT), 
            c("Y", "N", " ", ""), 
            c(T, F, NA, NA))
  toxicities$DLT <- as.logical(toxicities$DLT)
  #toxicities$DLT <- factor(toxicities$DLT, c("No", "Yes", NA), exclude=NULL)
  return(toxicities)
}

ConvertToTSV <- function(){
  library(gdata)
  agent.protocol <- read.table("../Data/Protocol-Agents.txt", header=T)
  agents <- agent.protocol$AGENTS
  names(agents) <- agent.protocol$PROTOCOL
  for (f in list.files("../Data/Data_Excel/")){
    #cat(f, "\n")
    df <- read.xls(paste0("../Data/Data_Excel/", f), sheet=1, header=T)
    # many drugs have different names in the different studies
    if (f=="Proj31Dosing.xlsx"){
      cat("Converting drug name...\n")
      drug.dict <- read.table("../Data/drug_names.txt", 
                              col.names=c("OLD", "NEW"), 
                              sep="\t", stringsAsFactors=F)
      drug.d <- drug.dict$NEW
      names(drug.d) <- drug.dict$OLD
      df$DRUG <- drug.d[df$DRUG]
    }
    # Toxicities and Dosing Spreadsheet have lowercase names
    names(df) <- toupper(names(df))
    # Dosing uses Patient.Id instead of Patient
    names(df)[names(df)=="PATIENT.ID"] <- "PATIENT"
    # create a universal patient id since some protocols use the same patient id
    df$UID <- paste(df$PROTOCOL, df$PATIENT, sep=":")
    df$AGENT <- agents[df$PROTOCOL]
    df$STUDY.ID <- paste(df$AGENT, df$PROTOCOL, sep="-") 
    write.table(df, paste0("../Data/Data_tsv/", tsv(f)), sep="\t", quote=F, 
                row.names=F, col.names=T)
    #print(f)
    #print(str(df))
  }
}

# Data checking function, used when first got data
ComparePatients <- function(){
  # some patients in different protocols have the same id so we make a 
  # unique identifier
  UniquePatients <- function(df){
    unique(df$UID)
  }
  # 486 unique combinations in baseline symptoms, clinical outcome 
  # patients and medical history
  bs.patients <- UniquePatients(baseline.symptoms)
  co.patients <- UniquePatients(clinical.outcome)
  pa.patients <- UniquePatients(patients)
  mh.patients <- UniquePatients(medical.history)
  stopifnot(length(intersect(bs.patients, pa.patients))==length(bs.patients))
  stopifnot(length(intersect(bs.patients, co.patients))==length(bs.patients))
  stopifnot(length(intersect(bs.patients, mh.patients))==length(bs.patients))
  stopifnot(length(unique(bs.patients))==486)
  cat(length(unique(bs.patients)), "unique patients are represented in the baseline symptoms, clinical outcomes, patients and medical history spreadsheets.\n")
  do.patients <- UniquePatients(dosing) 
  cat(length(unique(do.patients)), "unique patients are represented in the dosing spreadsheet.\n")
  # all now in dosing (previously some were missing)
  stopifnot(length(setdiff(bs.patients, do.patients))==0)
  stopifnot(length(setdiff(do.patients, bs.patients))==0)
  stopifnot(length(unique(do.patients))==486)
  # only 479 in toxicities
  to.patients <- UniquePatients(toxicities)
  cat("Only", length(unique(to.patients)), "unique patients are represented in the toxicities spreadsheet.\n")
  stopifnot(length(setdiff(to.patients, bs.patients))==0)
  stopifnot(length(to.patients)==479)
  # how many patients in each study don't have dosing information
  library(plyr)
  dos.nump.per.pr <- ddply(dosing, "PROTOCOL", function(x) {c(NUM.DOSING=length(unique(x$PATIENT)))})
  pa.nump.per.pr <- ddply(patients, "PROTOCOL", function(x) {c(TOTAL.NUM=length(unique(x$PATIENT)))})
  do.miss <- merge(dos.nump.per.pr, pa.nump.per.pr)
  cat("This is a table of how many patients in each study have dosing information.\n")
  print(do.miss)
  no.dose <- subset(dosing, is.na(DOSE.LEVEL))
  cat("There are", nrow(no.dose), "patients without dosing information.\n")
  print(no.dose$UID)
  print(subset(clinical.outcome, UID %in% no.dose$UID)
        [, c("UID", "BEST_RESPONSE", "OFF.STUDY.REASON")])
  # look at course start date for missing patients for protocol 1857 (35 missing patients)
  # course start date is now missing, omit this check
#   csds <- function(patients, dosing, toxicities){
#     tox.missing.patients <- subset(toxicities, UID %in%setdiff(patients$UID, dosing$UID))
#     csds <- ddply(tox.missing.patients, "PATIENT", 
#                   function(x){data.frame(PROTOCOL=x$PROTOCOL[1], 
#                                 EARLIEST.COURSE.NUM.WITH.TOX=min(x$COURSE.NUMBER),
#                                 EARLIEST.COURSE.START.WITH.TOX=min(
#                     strptime(x$COURSE.START.DATE, format="%d-%b-%y")))})
#     csds <- arrange(csds, PROTOCOL)
#   }
#   
#   miss.csd <- csds(patients, dosing, toxicities)
#   cat("These patients were all treated--here are the dates on which 6 of them started treatment.\n")
#   browser()
#   print(head(miss.csd))
}

# Data checking function, used when first got data
CheckDates <- function(){
  co.data <- ddply(clinical.outcome, "UID", 
                   function(x){c(PROTOCOL=x$PROTOCOL[1], 
                                 DATEOFF=x$DATE_OFF_PROTOCOL[1],
                                 DATEBESTRESPONSE=x$DATE.OF.BEST.RESPONSE[1])})
  do.data <- ddply(dosing, "UID", 
                   function(x){data.frame(PROTOCOL=x$PROTOCOL[1], 
                                          FIRST.COURSE.START=min(
                                            strptime(x$COURSE.START.DATE, format="%d-%b-%y")), 
                                          LASTCSD=max(
                                            strptime(x$COURSE.START.DATE, format="%d-%b-%y")),
                                          NUMCSD=length(unique(x$COURSE.START.DATE)), 
                                          MAX.COURSE.NUM=max(x$COURSE.NUMBER), 
                                          NUM.COURSES.WITH.DATA=length(unique(x$COURSE.NUMBER)), 
                                          NUMDA=length(unique(x$DRUG.ADMINISTRATION.DATE)),
                                          FIRSTDA=min(
                                            strptime(x$DRUG.ADMINISTRATION.DATE, format="%d-%b-%y")),
                                          LASTDA=max(
                                            strptime(x$DRUG.ADMINISTRATION.DATE, format="%d-%b-%y"))
                   )})
  pa.data <- ddply(dosing, "UID", 
                   function(x){c(PROTOCOL=x$PROTOCOL[1], 
                                 ONSTUDY=x$ONSTUDY[1])})
  cat(sum(!do.data$NUM.COURSES.WITH.DATA==do.data$MAX.COURSE.NUM), "patients have missing dosing information for one course of treatment.\n")
  print(subset(do.data, !NUM.COURSES.WITH.DATA==MAX.COURSE.NUM)[, c("UID", "PROTOCOL", 
                                                                    "FIRST.COURSE.START", 
                                                                    "MAX.COURSE.NUM", 
                                                                    "NUM.COURSES.WITH.DATA")])
  # 3 patients have missing dosing information for one course number
  #    5856:USC 022 is missing data for course 13 out of 16
  #    T99-0048:COH-025 is missing data for course 3 out of 4
  #    T99-0048:USC-02 is missing data for course 1 out of 2
  stopifnot(3==sum(!do.data$NUM.COURSES.WITH.DATA==do.data$MAX.COURSE.NUM))
  # check that last drug administration date is after first drug administration date
  stopifnot(0==sum(do.data$FIRSTDA > do.data$LASTDA))
  # check that last course start date isn't before first course start date
  stopifnot(0==sum(do.data$LASTCSD < do.data$FIRST.COURSE.START))
  # check that first drug administration date isn't before first course start date
  stopifnot(0==sum(do.data$FIRSTDA < do.data$FIRST.COURSE.START))
  # check that last drug administration date isn't before last course start date
  stopifnot(0==sum(do.data$LASTDA < do.data$LASTCSD))
}

# get three kinds of outcome--dose reduction, our DLT, protocol-defined DLT
GetOutcome <- function(){
  reductions <- DoseReductions(1)
  library(plyr)
  DLT <- GetDLT()
  outcome <- merge(reductions, DLT, all.x=T)
  # some patients have no toxicities, like 6432:16-4-34 
  outcome$OUR.DLT[is.na(outcome$OUR.DLT)] <- 0
  outcome$PROT.DLT[is.na(outcome$PROT.DLT)] <- 0
  outcome$MOD.DLT <- !is.na(outcome$MOD.TYPE) | outcome$OFF.STUDY.REASON=="Toxicity/Side Effects"
  outcome$MOD.DLT.CYCLE <- ifelse(outcome$OFF.STUDY.REASON=="Toxicity/Side Effects", outcome$MAX.CYCLE, NA)
  outcome$MOD.DLT.CYCLE <- ifelse(!is.na(outcome$MOD.TYPE), outcome$MOD.CYC, outcome$MOD.DLT.CYCLE)
  outcome$PROT.DLT <- as.logical(outcome$PROT.DLT)
  outcome$OUR.DLT <- as.logical(outcome$OUR.DLT)
  outcome <- outcome[, c("UID", "OUR.DLT", "OUR.DLT.CYCLE", "PROT.DLT", "PROT.DLT.CYCLE", "MOD.DLT", "MOD.DLT.CYCLE")]
}

GetSchedule <- function(days, cycle.length, num.cycles, remove=0){
  d <- days
  s <- rep(1, length(days))
  for (i in 1:num.cycles){
    d <- c(d, days + cycle.length * i)
    s <- c(s, rep(i + 1, length(days)))
  }
  d <- d[(remove+1):length(d)]
  s <- s[(remove+1):length(s)]
  d <- d - min(d)
  return(list(schedule=d, cycles=s))
}

Schedule <- function(protocol, UID){
  if (protocol=="T99-0047"){
    return(GetSchedule(c(1,4), cycle.length=14, num.cycles=100))
  }
  if (protocol%in% c("94", "T99-0048")){
    return(GetSchedule(c(1,4,8,11,15,18,22,25), cycle.length=42, num.cycles=100))
  }
  if (protocol %in% "T99-0071"){
    sch <- read.table(paste0(not.dropbox.dir, "T99-0071.schedules.txt"), header=T, sep="\t")
    schedule <- sch[sch$UID==UID, "SCHEDULE"]
    if (schedule==1){
      return(GetSchedule(c(1,4,8,11,15,18,22,25), cycle.length=42, num.cycles=100))
    } else {
      return(GetSchedule(c(1,4,8,11), cycle.length=21, num.cycles=100))
    }
  }
  if (protocol=="1857"){
    sch <- read.table(paste0(not.dropbox.dir, "1857.schedules.txt"), header=T, sep="\t")
    schedule <- sch[sch$UID==UID, "SCHEDULE"]
    if (schedule==1){
      return(GetSchedule(c(1,4), cycle.length=7, num.cycles=100))
    } else {
      return(GetSchedule(c(1,8), cycle.length=21, num.cycles=100))
    }
  }
  if (protocol=="1858"){
    return(GetSchedule(c(2,5,9,12), cycle.length=21, num.cycles=100, remove=2))
  }
  if (protocol=="1860"){
    return(GetSchedule(c(1,4,8), cycle.length=21, num.cycles=100))
  }
  if (protocol %in% c("3771", "5326", "5856", "5874", "6126", "6432", "6520")){
    return(GetSchedule(c(1,4,8,11), cycle.length=21, num.cycles=100))
  }
}

InitialDose <- function(phase.1.only=T){
  dos <- GetRecodedDosing()
  library(plyr)
  FIRSTDOSE <- ddply(subset(dos, RECODED.DRUG=="BORTEZOMIB"), "UID", function(x){
    c(FIRST.DOSE=x$RECODED.DOSE.LEVEL[which.min(x$RECODED.DRUG.ADMINISTRATION.DATE)])})
}

DoseReductions <- function(dose.thresh=1, recompute=F){
  f <- paste0(not.dropbox.dir, "/dose.reductions.thresh.", dose.thresh, ".txt")
  if (recompute | !file.exists(f)){
    dosing <- GetRecodedDosing()
    library(plyr)
    dos <- dosing[, c("COURSE.NUMBER", "UID", "PROTOCOL", 
                      "RECODED.DOSE.LEVEL", "RECODED.DRUG", "RECODED.DOSE.LEVEL.UNIT", 
                      "RECODED.DRUG.ADMINISTRATION.DATE")]
    dos <- subset(dos, RECODED.DRUG=="BORTEZOMIB")
    library(plyr)
    prots <- unique(dos$PROTOCOL)
    mod.list <- vector('list', length(prots))
    clinical.outcome <- GetClinicalOutcome()
    for (i in 1:length(prots)){
      dos.p <- subset(dos, PROTOCOL==prots[i])
      clin.p <- subset(clinical.outcome, PROTOCOL==prots[i])
      # arrange patients in order of first dose
      p.firstdose <- ddply(dos.p, "UID", function(x){min(x$RECODED.DRUG.ADMINISTRATION.DATE)})
      p.firstdose <- arrange(p.firstdose, V1)
      ps <- p.firstdose$UID
      num.ps <- length(ps)
      mod.list[[i]] <- data.frame(UID=ps, PROTOCOL=rep(prots[i], num.ps), 
                                  MOD.TYPE=rep(NA, num.ps), 
                                  MOD.DATE=as.Date(rep(NA, num.ps)),
                                  MOD.CYC=rep(NA, num.ps))
      # get dose levels
      tt <- table(dos.p$RECODED.DOSE.LEVEL)
      dose.levels <- 1:length(tt)
      names(dose.levels) <- names(tt)
      # what is longest administration duration for all patients in study?
      max.x <- max(ddply(dos.p, "UID", function(x){
        max(x$RECODED.DRUG.ADMINISTRATION.DATE)-min(x$RECODED.DRUG.ADMINISTRATION.DATE)
      })$V1)
      for (j in 1:length(ps)){
        dos.pu <- subset(dos.p, UID==ps[j])
        off.p.date <- subset(clin.p, UID==ps[j])$DATE_OFF_PROTOCOL
        off.p.time <- as.numeric(as.Date(off.p.date, 
                      format="%d-%b-%y") - min(dos.pu$RECODED.DRUG.ADMINISTRATION.DATE))
        reason <- clin.p[clin.p$UID==ps[j], "RECODED.OFF.STUDY.REASON"]
        dos.pu <- arrange(dos.pu, RECODED.DRUG.ADMINISTRATION.DATE)
        dcodes <- dose.levels[as.character(dos.pu$RECODED.DOSE.LEVEL)]
        # which doses weren't at right dose?  
        reduced <- as.numeric(names(dcodes)) < dose.thresh * 
          as.numeric(names(dcodes[1]))
        dtimes <- c(0, cumsum(as.numeric(diff(dos.pu[, "RECODED.DRUG.ADMINISTRATION.DATE"]))))
        schedule <- Schedule(protocol=prots[i], UID=ps[j])
        reduced.date <- NULL
        if (any(reduced)){
          reduced.date <- min(dos.pu$RECODED.DRUG.ADMINISTRATION.DATE[reduced])
          reducedw <- min(which(reduced))
          reduced.cycle <- min(dos.pu$COURSE.NUMBER[reduced])
          mod.list[[i]][j, "MOD.TYPE"] <- "DOSE.REDUCED"
          mod.list[[i]][j, "MOD.DATE"] <- reduced.date
          mod.list[[i]][j, "MOD.CYC"] <- reduced.cycle
        }
        mod.list[[i]][j, "OFF.STUDY.DATE"] <- off.p.date
        mod.list[[i]][j, "OFF.STUDY.REASON"] <- reason
      }
    }
    m <- do.call('rbind', mod.list)
    m$APPROVED.SCHEDULE <- F
    m[m$PROTOCOL %in% c("3771", "5326", "5856", "5874", "6126", "6432", "6520"), 
      "APPROVED.SCHEDULE"] <- T
    sch <- read.table(paste0(not.dropbox.dir, "T99-0071.schedules.txt"), header=T, sep="\t", stringsAsFactors=F)
    m[m$UID %in% subset(sch, SCHEDULE==2)$UID, "APPROVED.SCHEDULE"] <- T
    max.cyc <- GetMaxCycles()
    m <- merge(m, max.cyc)
    write.table(m, f, 
                row.names=F, sep="\t", quote=F)
  }
  read.table(f, header=T, sep="\t", stringsAsFactors=F)
}

OutcomeApprovedCycle <- function(){
  reductions <- DoseReductions(1)
  reductions <- subset(reductions, APPROVED.SCHEDULE)
  library(plyr)
  DLT <- GetDLT()
  outcome <- merge(reductions, DLT, all.x=T)
  # some patients have no toxicities, like 6432:16-4-34 
  outcome$OUR.DLT[is.na(outcome$OUR.DLT)] <- 0
  outcome$PROT.DLT[is.na(outcome$PROT.DLT)] <- 0
  outcome$MOD.DLT <- !is.na(outcome$MOD.TYPE) | outcome$OFF.STUDY.REASON=="Toxicity/Side Effects"
  outcome$MOD.DLT.CYCLE <- ifelse(outcome$OFF.STUDY.REASON=="Toxicity/Side Effects", outcome$MAX.CYCLE, NA)
  outcome$MOD.DLT.CYCLE <- ifelse(!is.na(outcome$MOD.TYPE), outcome$MOD.CYC, outcome$MOD.DLT.CYCLE)
  outcome$PROT.DLT <- as.logical(outcome$PROT.DLT)
  outcome$OUR.DLT <- as.logical(outcome$OUR.DLT)
  dosing <- GetRecodedDosing()
  # find first dose for each patient
  dosing$RECODED.DRUG.ADMINISTRATION.DATE <- as.Date(dosing$RECODED.DRUG.ADMINISTRATION.DATE, format="%d-%b-%y")
  bort.dose <- ddply(dosing, c("UID", "PROTOCOL"), 
                     function(x){c1 <- subset(x, RECODED.DRUG=="BORTEZOMIB");
                                 if (nrow(c1) > 1){
                                   d <- c1[which.min(c1$RECODED.DRUG.ADMINISTRATION.DATE), "RECODED.DOSE.LEVEL"]
                                 } else {
                                   d <- c1[['RECODED.DOSE.LEVEL']][1]
                                 }; return(c(DOSE=d))})
  bort.dose <- subset(bort.dose, !is.na(DOSE))
  outcome <- merge(outcome, bort.dose, all.x=T)
  outcome$GROUP <- cut(outcome$DOSE, breaks=c(0.25, 0.75, 1, 1.3, 2), 
                       right=T, include.lowest=T)
  return(outcome)
}

plotincidence <- function(cif, toxcode, colo=c("black", "purple", "blue", "red")){
  
  plot(cif, lwd=2, col=colo, 
       which.cif=c(toxcode), xlab="Cycle", cex=0.6, 
       xlim=c(1,8), legend=F, ylab="Probability")
}

PrintCIs <- function(cif, toxtype){
  doses <- c("Dose=0.5-0.75", "Dose=1.0", "Dose=1.3", "Dose=1.5-1.7")
  cat(toxtype, "\n")
  print(doses)
  event <- toxtype
  for (cycle in c(1,2,5)){
    cat(cycle,  ", ", sep="")
    for (dose in doses){
      info <- summary(cif)[dose][[1]][event][[1]]
      times <- info$time <= cycle
      which.row <- max(which(times))
      row <- summary(cif)[dose][[1]][event][[1]][which.row, ]
      P <- as.numeric(row["P"])
      lower <- as.numeric(row["lower"])
      upper <- as.numeric(row["upper"])
      cat(round(P, 2), " (", round(lower, 2), "-", round(upper, 2), ")", sep="")
      if (!dose==doses[length(doses)]) cat(", ")
    }
    cat("\n")
  }
}

NeuropathyTox <- function(){
  tox <- GetRecodedToxicitiesTrRelMaxInCycle()
  tox <- subset(tox, PROTOCOL %in% Phase1Studies())
  neuro <- subset(tox, FINAL.DESCR %in% 
                    c("PERIPHERAL SENSORY NEUROPATHY", 
                      "PERIPHERAL MOTOR NEUROPATHY"))
  library(plyr)
  neuro.nodup <- neuro
  neuro.nodup$FINAL.DESCR <- "PERIPHERAL NEUROPATHY"
  neuro.nodup <- ddply(neuro.nodup, c("UID", "COURSE.NUMBER"), 
                       function(x){
                         if (nrow(x)>1){
                           return(x[which.max(x$GRADE), ])
                         }
                         return(x)
                       })
  not.neuro <- subset(tox, !FINAL.DESCR %in% 
                        c("PERIPHERAL SENSORY NEUROPATHY", 
                          "PERIPHERAL MOTOR NEUROPATHY"))
  tox.neuro <- rbind(neuro.nodup, not.neuro)
  return(tox.neuro)
}

NeuropathyOutcome <- function(){
  ntox <- NeuropathyTox()
  neuro <- subset(ntox, FINAL.DESCR=="PERIPHERAL NEUROPATHY")
  neuro <- subset(neuro, GRADE >= 2)
  neuro$ONSET.DATE <- as.Date(neuro$ONSET.DATE, format="%d-%b-%y")
  g3 <- ddply(neuro, "UID", function(x){data.frame(G3orHigher=as.logical(max(x$GRADE)>=3), 
                                                   G3orHigherCourse=ifelse(max(x$GRADE>=3), 
                                                                           min(subset(x, GRADE>=3)$COURSE.NUMBER),
                                                                           NA))})
  g2recurrent <- ddply(subset(neuro, GRADE < 3), "UID", function(x){
    numg2=nrow(x)
    num.onset=length(unique(x$ONSET.DATE))
    if (num.onset>1){
      g <- arrange(x, ONSET.DATE)
      recurrent.course.number <- g[2, "COURSE.NUMBER"]
    }
    data.frame(G2Recurrent=num.onset>1, G2RecurrentCourse=ifelse(num.onset>1, 
                                                                 recurrent.course.number, 
                                                                 NA))
  })
  neuro.outcome <- merge(g3, g2recurrent, all.x=T)
  neuro.outcome
}

SurvivalOutcome <- function(){
  # one patient has Death during Follow-up Period
  # taken off protocol and died 9 days later partway through first cycle
  progression.events <- c("Death during Follow-up Period", "Death on Study", 
                          "Disease Progression On Study")
  censoring.events <- c("Complicating Disease/Intercurrent Illness", "Other", 
                        "Protocol Violation", "Refused Further Treatment", 
                        "Study Complete: No Protocol-Specified Follow-up ", 
                        "Switched to Alternative Treatment")
  toxicity.events <- c("Toxicity/Side Effects")
  outcome <- OutcomeApprovedCycle()
  # modification-based 
  outcome$MOD.EV.CYC <- pmin(outcome$MOD.CYC, outcome$MAX.CYCLE, na.rm=T)
  # toxicity-based
  outcome$TOX.EV.CYC <- pmin(outcome$OUR.DLT.CYCLE, outcome$MAX.CYCLE, na.rm=T)
  outcome$MOD.EV.TYPE[outcome$OFF.STUDY.REASON %in% censoring.events] <- "Censored"
  outcome$MOD.EV.TYPE[outcome$OFF.STUDY.REASON %in% toxicity.events] <- "Toxicity"
  outcome$MOD.EV.TYPE[outcome$OFF.STUDY.REASON %in% progression.events] <- "Progression"
  outcome$TOX.EV.TYPE <- outcome$MOD.EV.TYPE
  outcome$MOD.EV.TYPE[!is.na(outcome$MOD.CYC)] <- "Toxicity"
  outcome$TOX.EV.TYPE[outcome$OUR.DLT] <- "Toxicity"
  outcome$PTOX.EV.CYC <- pmin(outcome$PROT.DLT.CYCLE, outcome$MAX.CYCLE, na.rm=T)
  outcome$PTOX.EV.TYPE <- "Censored"
  outcome$PTOX.EV.TYPE[outcome$OFF.STUDY.REASON %in% progression.events] <- "Progression"
  outcome$PTOX.EV.TYPE[outcome$PROT.DLT] <- "Toxicity"
  return(outcome)
}

GetDLT <- function(){
  tox.tr.rel <- GetRecodedToxicitiesTrRelMaxInCycle(phase.1.only = T)
  library(plyr)
  our.DLT <- ddply(tox.tr.rel, "UID", function(x){
    c(#PROT.DLT=any(x$DLT), 
      #PROT.DLT.CYCLE=ifelse(any(x$DLT), min(x$COURSE.NUMBER[x$DLT]), NA))})
      #,
      OUR.DLT=any(x$OUR.DLT.REL), 
      OUR.DLT.CYCLE=ifelse(any(x$OUR.DLT.REL), min(x$COURSE.NUMBER[x$OUR.DLT.REL]), NA))})
  toxicities <- GetToxicities()
  toxicities <- subset(toxicities, !is.na(DLT))
  prot.DLT <- ddply(toxicities, "UID", function(x){
    c(PROT.DLT=any(x$DLT), 
      PROT.DLT.CYCLE=ifelse(any(x$DLT), min(x$COURSE.NUMBER[x$DLT]), NA))})
  DLT <- merge(our.DLT, prot.DLT, all.x=T)
  return(DLT)
}

AllCycles <- function(phase.1.only=T){
  maxc <- GetMaxCycles()
  maxc <- subset(maxc, !is.na(MAX.CYCLE))
  library(plyr)
  l <- dlply(maxc, "UID", function(x){
    data.frame(UID=x$UID, 
               COURSE.NUMBER=1:x$MAX.CYCLE)})
  all.cycles <- do.call('rbind', l)
  if (phase.1.only){
    subset(all.cycles, UID %in% Phase1PatientsUIDs())
  } else all.cycles
}

# calculate TBS score
TBS <- function(deathequals5=T){
  f <- paste0(not.dropbox.dir, "/TBS.txt")
  if (!file.exists(f)){
    all.cycles <- AllCycles(phase.1.only=T)
    tox <- GetRecodedToxicitiesMaxInCycle(phase.1.only = T)
    tbs <- ddply(tox, c("UID", "COURSE.NUMBER"), function(x){
      sum <- 0
      pl <- subset(x, FINAL.DESCR=="PLATELET COUNT DECREASED")
      if (nrow(pl)>0){
        grade.pl <- pl[['GRADE']]
        sum <- sum + c(0.17, 0.17, 0.40, 0.85, 0.85)[grade.pl]
      }
      neur.types <- c("PERIPHERAL SENSORY NEUROPATHY", 
                      "PERIPHERAL MOTOR NEUROPATHY")
      neur <- subset(x, FINAL.DESCR %in% neur.types)
      if (nrow(neur)>0){
        grade.neur <- max(neur[['GRADE']])
        sum <- sum + c(0.19, 0.64, 1.03, 2.53, 2.53)[grade.neur]
      }
      grad3plusnonhem <- subset(x, !HEMATOLOGIC & GRADE >= 3 & !FINAL.DESCR %in% neur.types)
      sum <- sum + nrow(grad3plusnonhem) * 0.17
      # if patient died, set TBS equal to 5
      c(TBS=sum, DEATH=max(x$GRADE)==5)
    })
    TBS <- merge(all.cycles, tbs, all.x=T)
    TBS$TBS[is.na(TBS$TBS)] <- 0
    write.table(TBS, f, sep="\t", quote=F)
  }
  #cat("reading", f, "\n")
  TBS <- read.table(f, header=T, sep="\t", stringsAsFactors=F)
  if (deathequals5){
    TBS$TBS[!is.na(TBS$DEATH) & TBS$DEATH==1] <- 5
  }
  TBS
}