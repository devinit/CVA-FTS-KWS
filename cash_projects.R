suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))

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

cash.noncase.keywords <- paste(cash.noncase.keywords, collapse = "|")
cash_projects <- all_projects[grepl(cash.noncase.keywords, question)]
pattern <- "\\d+\\.\\d+|\\d+%|\\d+"
cash_projects <- cash_projects[grepl(pattern, answer)]
cash_projects <- cash_projects[!grepl("beneficiaries|justification|is it better|people", question, ignore.case = TRUE)]


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
fwrite(cash_projects, "projects/cash_project_percentages.csv")
