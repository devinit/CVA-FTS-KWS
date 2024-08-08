project2016 <- fread("C:/git/CVA-FTS-KWS/all_projects_2016.csv")
project2017 <- fread("C:/git/CVA-FTS-KWS/all_projects_2017.csv")
project2018 <- fread("C:/git/CVA-FTS-KWS/all_projects_2018.csv")
project2019 <- fread("C:/git/CVA-FTS-KWS/all_projects_2019.csv")
project2020 <- fread("C:/git/CVA-FTS-KWS/all_projects_2020.csv")
project2021 <- fread("C:/git/CVA-FTS-KWS/all_projects_2021.csv")
project2022 <- fread("C:/git/CVA-FTS-KWS/all_projects_2022.csv")
project2023 <- fread("C:/git/CVA-FTS-KWS/all_projects_2023.csv")

all_projects <- rbind(project2016, project2017, project2018, project2019, project2020, project2021, project2022, project2023)
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
  "ESSN")

cash.noncase.keywords <- paste(cash.noncase.keywords, collapse = "|")
cash_projects <- all_projects[grepl(cash.noncase.keywords, question)]
pattern <- "\\d+\\.\\d+|\\d+%|\\d+"
cash_projects <- cash_projects[grepl(pattern, answer)]
cash_projects <- cash_projects[!grepl("beneficiaries|justification|is it better", question, ignore.case = TRUE)]


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
