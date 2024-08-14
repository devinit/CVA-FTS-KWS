suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))

# Load and merge project-level data

project2017 <- fread("projects/project_data_2017.csv")
project2018 <- fread("projects/project_data_2018.csv")
project2019 <- fread("projects/project_data_2019.csv")
project2020 <- fread("projects/project_data_2020.csv")
project2021 <- fread("projects/project_data_2021.csv")
project2022 <- fread("projects/project_data_2022.csv")
project2023 <- fread("projects/project_data_2023.csv")
project2024 <- fread("projects/project_data_2024.csv")

all_projects <- rbindlist(
  list(
    project2017, project2018, project2019, project2020, project2021, project2022, project2023, project2024
    )
)
questions <- unique(all_projects$question)
write.csv(questions, "questions.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)

# Search for cash questions
cash.noncase.keywords <- c(
  "cash",
  "voucher",
  "vouchers",
  "cash transfer",
  "cash grant", 
  "unconditional cash",
  "money",
  "conditional cash transfer",
  "argent",
  "monetaires",
  "bons",
  "espèces",
  "monnaie",
  "monétaires",
  "tokens",
  "coupons",
  "cupones",
  "transfert monétaire",
  "transfer monétaire",
  "transferencias monetarias",
  "public works programme",
  "social assistance",
  "social safety net",
  "social transfer",
  "social protection",
  "CVA",
  "CCT",
  "UCT",
  "CTP",
  "CFW",
  "CFA",
  "SSN",
  "ESSN",
  "MPC",
  "MPCT")

cash.noncase.keywords = paste0(
  "\\b",
  paste(cash.noncase.keywords, collapse="\\b|\\b"),
  "\\b"
)
cash_projects <- all_projects[grepl(cash.noncase.keywords, question, ignore.case=T)]
cash_projects <- cash_projects[!grepl("beneficiaries|justification|is it better|people", question, ignore.case = TRUE)]
boolean_cash_projects = subset(cash_projects, answer %in% c("true","false","Qui", "Non", "Yes", "No", "yes", "no"))
boolean_cash_projects = subset(boolean_cash_projects, !grepl("genre|women", question, ignore.case=T))

pattern <- "\\d+\\.\\d+|\\d+%|\\d+"
cash_projects <- cash_projects[grepl(pattern, answer)]

# Standardize answers
standardize_percentage <- function(x) {
  x <- trimws(tolower(x))
  if (grepl("%", x)) {
    num <- gsub(".*?(\\d+(\\.\\d+)?%).*", "\\1", x)  
    num <- gsub("%", "", num)  
  } else if (grepl("less than 1", x)) {
    num <- "0"
  } else if (grepl("percent", x)) {
    num <- gsub(".*?(\\d+(\\.\\d+)? percent).*", "\\1", x)  
    num <- gsub("percent", "", num)  
  } else if (grepl("^[0-9]+(\\.[0-9]+)?$", x)) {
    num <- x
  } else {
    num <- gsub(".*?(\\d+(\\.\\d+)?%).*", "\\1", x)  
    if (num == "") {
      num <- NA
    } else {
      num <- gsub("%", "", num)  
    }
  }
    num <- gsub("[^0-9.]", "", num)
    num <- as.numeric(num)
    return(num)
}
cash_projects <- cash_projects[, standardized_percentage := sapply(answer, standardize_percentage)]

cash_projects = cash_projects[,.(cva_percentage = sum(standardized_percentage)), by=.(project_id)]
cash_projects$cva_percentage[which(cash_projects$cva_percentage > 100)] = 100
cash_projects$cva_percentage = cash_projects$cva_percentage / 100

standardize_boolean = function(x){
  if(tolower(x) %in% c("true", "qui", "yes")){
    return(T)
  }
  return(F)
}

boolean_cash_projects$boolean_answer = sapply(boolean_cash_projects$answer, standardize_boolean)

boolean_cash_projects = boolean_cash_projects[,.(cva=max(boolean_answer)==1), by=.(project_id)]

# Find and fix overlaps
zero_percents = subset(cash_projects, cva_percentage == 0)
zero_to_bool = data.table(project_id = zero_percents$project_id, cva=F)
new_zero_ids = setdiff(zero_to_bool$project_id, boolean_cash_projects$project_id)
zero_to_bool = subset(zero_to_bool, project_id %in% new_zero_ids)
boolean_cash_projects = rbind(boolean_cash_projects, zero_to_bool)

false_bools = subset(boolean_cash_projects, !cva)
bool_to_zero = data.table(project_id = false_bools$project_id, cva_percentage=0)
new_bool_ids = setdiff(bool_to_zero$project_id, cash_projects$project_id)
bool_to_zero = subset(bool_to_zero, project_id %in% new_bool_ids)
cash_projects = rbind(cash_projects, bool_to_zero)
# 
# diff = setdiff(cash_projects$project_id, boolean_cash_projects$project_id)
# diff_projects = subset(all_projects, project_id %in% diff)

cash_bool_and_percentage = merge(cash_projects, boolean_cash_projects, all=T)
cash_bool_and_percentage$cva[which(cash_bool_and_percentage$cva_percentage > 0)] = T
cash_bool_and_percentage$cva[which(cash_bool_and_percentage$cva_percentage==0)] = F

fwrite(cash_bool_and_percentage, "projects/cash_projects.csv")

