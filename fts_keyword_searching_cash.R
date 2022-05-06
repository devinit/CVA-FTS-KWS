suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

##setwd(dirname(getActiveDocumentContext()$path))
##setwd("..")

##load in fts
years <- 2016:2021
fts_curated <- list()
for (i in 1:length(years)){
  year <- years[i]
  fts_curated[[i]] <- fread(paste0)("https://github.com/devinit/gha_automation/tree/main/IHA/datasets/fts_curated_master/fts_curated_",year,".csv")
  message(year)
  
}

fts <- rbindlist(fts_curated)
fts <- fts[as.charachter(year) >= 2016]


## Maybe add in keep function to have only required columns


##Keywords from Nik's keyword list, can we tweak so acroynms only picked up if standalone?

cash.keywords <- c(
  "cash",
  "voucher",
  "cash transfer",
  "cash grant", 
  "unconditional cash",
  "money",
  "conditional cash transfer",
  "CCT",
  "UCT",
  "argent",
  "monetaires",
  "bons",
  "espèces",
  "monnaie",
  "monétaires",
  "tokens",
  "coupons",
  "ctp",
  "public works programme",
  "cfw",
  "cfa",
  "social assistance",
  "social safety net",
  "ssn",
  "essn",
  "social transfer",
  "social protection"
  
)


##Relevant clusters from cluster mapping
cash_clusters <- c(
  "Cash",
  "Multi-purpose Cash",
  "Multipurpose cash assistance",
  "Multi-purpose Cash Assistance",
  "Multipurpose Cash Assistance COVID-19",
  "Multi-Purpose Cash Assistance COVID-19",
  "Multi-purpose Cash COVID-19",
  "Multipurpose cash",
  "Protection: Multi-Purpose Cash Assistance",
  "Cash Transfer COVID-19"
  
  )

fts$relevance <- "None"



## Define relevance based on sector and/or method
fts[method == "Cash transfer programming (CTP)", relevance := "Total"]
fts[destinationObjects_Cluster.name %in% cash_clusters, relevance := "Total"]


#TODO select partial sectors with cash cluster and
fts[multisector == T &, relevance := "Partial"]
## was by use of grepl | and if cash_clusters == T??



#Count number of keywords appearing in description
fts[, keywordcount := unlist(lapply(tolower(paste0(description)), function(x) sum(gregexpr(tolower(paste0(major.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]


##below checks where relevance is none and there are or are not keywords
##second line below useful for identifying new keywords maybe missing
fts[relevance == "None" & keywordcount > 0]
fts[relevance != "None" & keywordcount == 0]

write.csv(fts, "fts_output_CVA.csv", fileEncoding = "UTF-8", row.names = F)
#
