# SNOMED lookups------------------------------------------------
# Ewen Harrison
# Centre for Medical Informatics, University of Edinburgh 2022

## The idea here is to expand all current snomed concepts and map them to read2 and ICD10

# Packages ----
library(Rdiagnosislist)
library(tidyverse)

# READ2 maps ----
## These are from here: https://isd.digital.nhs.uk/trud/users/authenticated/filters/0/categories/9/items/9/releases
readmaps = loadREADMAPS(
  "/home/common/snomed/nhs_datamigration_29.0.0_20200401000001/Mapping Tables/Updated/Not Clinically Assured/rcsctmap_uk_20200401000001.txt",
  "/home/common/snomed/nhs_datamigration_29.0.0_20200401000001/Mapping Tables/Updated/Not Clinically Assured/rctermsctmap_uk_20200401000001.txt",
  "/home/common/snomed/nhs_datamigration_29.0.0_20200401000001/Mapping Tables/Updated/Clinically Assured/ctv3sctmap2_uk_20200401000001.txt"
) 

# SNOMED lookup for read2 and ICD10 ----
## Environment, created using Rdiagnosislist::loadSNOMED()
## Follow instructions in package
## Reload the 'SNOMED' environment from file
SNOMED <- readRDS('/home/common/snomed/mySNOMED_int20200731_uk20201125.RDS')

# Extract the whole caboodle - this is the master lookup
## Comment in below if want to include other lookups
include_ctv_opcs4 = FALSE
# include_ctv_opcs4 = TRUE

if(include_ctv_opcs4){
  snomed = SNOMEDconcept('', SNOMED = SNOMED, exact_match = FALSE) %>% 
    getMaps(to = c("read2", "icd10", 
                   "ctv3", "ctv3simple", "opcs4"
    ), mappingtable = readmaps) 
} else {
  snomed = SNOMEDconcept('', SNOMED = SNOMED, exact_match = FALSE) %>% 
    getMaps(to = c("read2", "icd10"
    ), mappingtable = readmaps) 
}

# This is needed as data.table int64 format doesn't work well in tibble
## I know, just do it all in data.table. No thank you. 
snomed = snomed[, conceptId:=as.character(conceptId)]

# Final lookup only containing rows with either read2 or icd10
snomed = snomed %>% 
  as_tibble() %>% 
  unnest(icd10_code, keep_empty = TRUE) %>% 
  unnest(c(read2_code, read2_term), keep_empty = TRUE) %>% {
    if(include_ctv_opcs4){
      unnest(., c(opcs4_code), keep_empty = TRUE) %>% 
        unnest(c(ctv3_concept, ctv3_termid), keep_empty = TRUE) %>% 
        finalfit::rm_empty_block(read2_code, icd10_code, opcs4_code, ctv3_concept) # Remove rows with no lookup
    } else {
      finalfit::rm_empty_block(., read2_code, icd10_code) # Remove rows with no lookup
    }
  }
# rows = 185,677 if read2, icd10 only
# rows = 1,382,854 if includes ctv3 and opcs4

# makeMaps function ----
## This takes a folder of .xlsx files
## They were coded locally as column 1 = icd10 code, column 2 = icd10 description
## They were formatted with a decimal dot, e.g. E66.5, which is removed
## Output is mapped .csv files to new folder. 

makeMaps <- function(.path_in, .path_out, .snomed, .remove_dot = TRUE){
  files_in = list.files(.path_in, full.names = TRUE)
  file_names = list.files(.path_in) %>% 
    stringr::str_remove("\\..+") %>% 
    paste0(.path_out, "/", ., ".csv")
  
  files_in %>% 
    purrr::map2(file_names, ~ readxl::read_excel(.x) %>% 
                  rename("icd10_code" = 1, "icd10_description" = 2) %>% {
                    if(.remove_dot){
                      mutate(., icd10_code = str_remove(icd10_code, "\\."))
                    }
                  } %>% 
                  left_join(.snomed) %>% 
                  rename(snomed_conceptId = conceptId,
                         snomed_term = term) %>% 
                  write_csv(.y)
    )
  
  
}

makeMaps("data_in", "data_out", snomed)
