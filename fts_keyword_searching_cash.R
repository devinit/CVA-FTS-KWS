suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))

##load in fts
years <- 2017:2024
fts_curated <- list()
for (i in 1:length(years)){
  year <- years[i]
  fts_curated[[i]] <- fread(paste0("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/datasets/fts_curated_master/fts_curated_",year,".csv"))
  message(year)
  
}

fts <- rbindlist(fts_curated, use.names=T)
fts <- fts[as.character(year) >= 2017]

# Load in project data
project_metadata = fread("projects/cash_projects.csv")
project_metadata$project_id = as.character(project_metadata$project_id)
project_text = fread("projects/project_text.csv")
project_text$text = paste(project_text$project_name, project_text$project_objective)
project_text[,c("project_name", "project_objective")] = NULL
project_data = merge(project_text, project_metadata, all=T)
names(project_data) = c("sourceObjects_Project.id", "project_text", "project_cva_percentage", "project_cva")
## Maybe add in keep function to have only required columns
fts$sourceObjects_Project.id = as.character(fts$sourceObjects_Project.id)
fts = merge(fts, project_data, by="sourceObjects_Project.id", all.x=T)

fts$all_text = paste(fts$description, fts$project_text)

##Keywords from Nik's keyword list, can we tweak so acroynms only picked up if standalone?

#keywords are not case sensitive
cash.noncase.keywords <- c(
  "cash",
  "voucher",
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
  "monétaire",
  "tokens",
  "coupons",
  "cupones",
  "public works programme",
  "social assistance",
  "social safety net",
  "social transfer",
  "social protection"
)

#acronyms are case-sensitive
cash.acronyms <- c(
  "CCT",
  "UCT",
  "CTP",
  "CFW",
  "CFA",
  "SSN",
  "ESSN",
  "MPC",
  "MPCT",
  "CVA"
)

cash_regex = paste0(
  "\\b",
  paste(c(tolower(cash.noncase.keywords), tolower(cash.acronyms)), collapse="\\b|\\b"),
  "\\b"
)

##Relevant clusters from cluster mapping
cash_clusters <- c(
  "Basic Needs / Multi-Purpose Cash",
  "Cash à usage multiple",
  "Multi Purpose Cash",
  "Multi-cluster/Multi-Purpose Cash",
  "Multi-Purpose Cash & Social Protection",
  "Multipurpose Cash Assistance (MPC)",
  "Multi-Purpose Cash Assistance (MPCA)",
  "Multipurpose cash/ IDPs/ multisector",
  "Multi-sector Cash/Social Protection COVID-19",
  "Cash",
  "Multi-purpose Cash",
  "Multipurpose cash assistance",
  "Multi-Purpose Cash Assistance",
  "Multipurpose Cash Assistance COVID-19",
  "Multi-Purpose Cash Assistance COVID-19",
  "Multi-purpose Cash COVID-19",
  "Multipurpose cash",
  "Protection: Multi-Purpose Cash Assistance",
  "Cash Transfer COVID-19"
  )

fts$relevance <- "None"

## Define relevance based on sector and/or method
fts[method == "Cash transfer programming (CTP)", relevance := "Full"]
fts[destinationObjects_Cluster.name %in% cash_clusters, relevance := "Full"]

#TODO select partial sectors with cash cluster and
fts[grepl(";", destinationObjects_Cluster.name) == T & grepl(paste0(cash_clusters, collapse = "|"), destinationObjects_Cluster.name), relevance := "Partial"]
## was by use of grepl | and if cash_clusters == T??

#Count number of keywords appearing in description
fts$keyword_match = grepl(cash_regex, fts$all_text, ignore.case=T)
mean(fts$keyword_match > 0)
##below checks where relevance is none and there are or are not keywords
##second line below useful for identifying new keywords maybe missing
View(fts[relevance == "None" & keyword_match][,"all_text"])
View(fts[relevance != "None" & !keyword_match][,"all_text"])

fts_flagged <- fts[keyword_match | relevance != "None"]

fwrite(fts_flagged, "fts_output_CVA.csv")
#
