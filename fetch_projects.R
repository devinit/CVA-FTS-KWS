suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))

base_path <- "C:/Users/mike.pearson/Development Initiatives Poverty Research Limited/Humanitarian Team - Documents/GHA Programme/OA1 - Crisis trends, needs, resource flows/GHA Report 2024/Datasets/FTS - Full Dataset/Datasets/"
year <- 2023
filename = paste0(base_path, "fts_curated_", year, ".csv")
fts <- fread(filename)

unique_project_ids <- unique(fts$destinationObjects_Project.id)
unique_project_ids <- unique_project_ids[complete.cases(unique_project_ids)]

base_url = "https://api.hpc.tools/v2/public/project/"

project_list <- list()
project_index <- 1
pb <- txtProgressBar(max = length(unique_project_ids), style = 3)
for (i in 1: length(unique_project_ids)) {
  setTxtProgressBar(pb, i)
  project_id <- unique_project_ids[i]
  project_url <- paste0(base_url, project_id)
  project_json <- fromJSON(project_url, simplifyVector = FALSE)
  
  
  
  project = project_json$data$projectVersion
  global_clusters_json = project$globalClusters
  global_clusters = c()
  for(global_cluster in global_clusters_json){
    global_clusters = c(global_clusters, global_cluster$name)
  }
  global_clusters_string = paste0(global_clusters, collapse=" | ")
  organisation_json = project$organizations
  organisation_ids = c()
  organisation_names = c()
  for(organisation in organisation_json){
    organisation_ids = c(organisation_ids, organisation$id)
    organisation_names = c(organisation_names, organisation$name)
  }
  organisation_ids_string = paste0(organisation_ids, collapse=" | ")
  organisation_names_string = paste0(organisation_names, collapse=" | ")
  field_definitions = list()
  for(def in project$plans[[1]]$conditionFields){
    field_definitions[[as.character(def$id)]] = def
  }
  
  field_values = project$projectVersionPlans[[1]]$projectVersionFields
  if(length(field_values) == 0){
    project_df = data.frame(
      "project_id" = project_id,
      "project_year" = year,
      "currently_requested_funds" = project$currentRequestedFunds,
      "plan_id" = project$plans[[1]]$planVersion$id,
      "plan_name" = project$plans[[1]]$planVersion$name,
      "global_clusters" = global_clusters_string,
      "organisation_ids" = organisation_ids_string,
      "organisation_names" = organisation_names_string,
      "question" = "No field questions",
      "answer" = "No field answers"
    )
    project_list[[project_index]] = project_df
    project_index = project_index + 1
  }else{
    for(field in field_values){
      def = field_definitions[[as.character(field$conditionFieldId)]]
      if(!is.null(def) & !is.null(field$value)){
        project_df = data.frame(
          "project_id" = project_id,
          "project_year" = year,
          "currently_requested_funds" = project$currentRequestedFunds,
          "plan_id" = project$plans[[1]]$planVersion$id,
          "plan_name" = project$plans[[1]]$planVersion$name,
          "global_clusters" = global_clusters_string,
          "organisation_ids" = organisation_ids_string,
          "organisation_names" = organisation_names_string,
          "question" = def$name,
          "answer" = field$value
        )
        project_list[[project_index]] = project_df
        project_index = project_index + 1
      }
    }}}

close(pb)
all_projects <- rbindlist(project_list)
write.csv(all_projects, "all_projects_2023.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)

