#' @importFrom dplyr tbl collect
NULL

cp_version_lut <- 
  tibble::tribble(
    ~sqlite, ~version, ~version_string,
    "2014-07-23T17:45:00 6c2d896", "2.1.1", "2.1.1 (6c2d896)",
    "2016-05-03T18:31:00 ac0529e", "2.2.0", "2.2.0 (ac0529e)",
    "4.0.5", "4.0.5", "4.0.5"
  )

# quick and dirty for rapid development
get_sqlite_pool <- function(path) {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = path
  )
}

get_experiments <- function(pool) {
  pool %>% tbl("Experiment")

}

get_last_experiment <- function(pool) {
  get_experiments(pool) %>%
    filter(experiment_id == max(experiment_id, na.rm = T)) %>%
    pull(experiment_id)
}

get_experiment_properties <- function(pool, fields = NULL, experiments = NULL) {
  res <- pool %>%
    tbl("Experiment_Properties")
  if(!is.null(experiments)) res <- res %>% filter(experiment_id %in% experiments)
  if(!is.null(fields)) res <- res %>% filter(field %in% fields)
  res
}

get_table_prefix <- function(pool, experiment = get_last_experiment(pool)) {
  table_prefix <- get_experiment_properties(pool, "class_table", experiment) %>% pull(value)
  if(table_prefix != "") return(table_prefix)
  # determine if no prefix or missing prefix (e.g. in CP 2.1.1)
  tables <- DBI::dbListTables(pool)
  if("Per_Experiment" %in% tables) return("")
  # else, find Per_Experiment table and return prefix
  tables %>% 
    Filter(function(x) {str_detect(x, "Per_Experiment$")}, .) %>% 
    stringr::str_replace("Per_Experiment", "")

}

get_cp_info_table <- function(pool, experiment = get_last_experiment(pool)) {
  table_prefix <- get_table_prefix(pool, experiment)
  pool %>% tbl(f("{table_prefix}Per_Experiment"))
}

get_cp_pipeline <- function(pool, experiment = get_last_experiment(pool)) {
  get_cp_info_table(pool, experiment) %>% 
    collect() %>%
    pluck("Pipeline_Pipeline", 1) %>%
    as.raw %>%
    rawToChar() %>%
    stringi::stri_unescape_unicode() %>%
    paste(collapse = "\n")
}

get_cp_version <- function(pool, experiment = get_last_experiment(pool)) {
  sql_cp_version <- get_cp_info_table(pool, experiment) %>% pull(CellProfiler_Version)
  res <- cp_version_lut %>% semi_join(tibble(sqlite = sql_cp_version), by = "sqlite")
  if(nrow(res) == 0) {return(sql_cp_version)}
  return(res$version)
}

get_cp_time <- function(pool, experiment = get_last_experiment(pool)) {
   get_cp_info_table(pool, experiment) %>% 
    pull(Run_Timestamp) %>%
    as.POSIXct(tz = "", "%Y-%m-%dT%H:%M:%OS") %>% 
    format("%Y-%m-%d %H:%M:%S")

}

get_image_table <- function(pool, experiment = get_last_experiment(pool)) {
  image_table <- get_experiment_properties(pool, "image_table", experiment) %>% pull(value)
  pool %>% tbl(image_table)
}

get_object_table <- function(pool, experiment = get_last_experiment(pool)) {
  object_table <- get_experiment_properties(pool, "object_table", experiment) %>% pull(value)
  pool %>% tbl(object_table)
}

get_data_for_cptrackr <- function(pool, tracked_object, experiment = get_last_experiment(pool)) {
  image_id_col <- get_experiment_properties(pool, "image_id", experiment) %>% pull(value)
  group_cols <- get_experiment_properties(pool, c("group_id", "timepoint_id"), experiment) %>% pull(value)
  get_image_table(pool, experiment) %>% 
    select(image_id_col, group_cols) %>%
    left_join(get_object_table(pool, experiment), by = image_id_col)
}

get_object_names <- function(pool, experiment = get_last_experiment(pool)) {
  get_object_table(pool, experiment) %>%
    colnames() %>%
    Filter(function(x) endsWith(x, "_Number_Object_Number"), .) %>%
    stringr::str_replace("_Number_Object_Number", "")
}

get_tracked_objects <- function(pool, experiment = get_last_experiment(pool)) {
  get_object_table(pool, experiment) %>%
    colnames() %>%
    purrr::keep(function(x) stringr::str_detect(x, "_TrackObjects_")) %>%
    str_split_n(stringr::fixed("_"), 1) %>%
    unique()
}

has_tracking <- function(pool, experiment = get_last_experiment(pool)) {
  length(get_tracked_objects(pool, experiment)) > 0
} 

get_object_col <- function(pool, object, experiment = get_last_experiment(pool)) {
  obj_col <- get_object_table(pool, experiment) %>%
    colnames() %>%
    purrr::keep(function(x) stringr::str_detect(x, f("{object}_Number_Object_Number")))
  stopifnot(length(obj_col) > 0)
  obj_col
}

get_object_parent_col <- function(pool, object, experiment = get_last_experiment(pool)) {
  obj_par_col <- get_object_table(pool, experiment) %>%
    colnames() %>%
    purrr::keep(function(x) stringr::str_detect(x, f("{object}_TrackObjects_ParentObjectNumber")))
  stopifnot(length(obj_par_col) > 0)
  obj_par_col
}

get_group_data <- function(pool, experiment = get_last_experiment(pool), groups = NULL) {
  group_id_col <- get_experiment_properties(pool, "group_id", experiment) %>% pull(value)
  cols <- get_experiment_properties(pool, c("image_id", "group_id", "timepoint_id"), experiment) %>% 
    pull(value)
  group_data <- get_image_table(pool, experiment) %>%
    select(all_of(cols))
  if(!is.null(groups)) group_data <- group_data %>% filter(.data[[group_id_col]] %in% groups)
  group_data
}

get_object_data_with_groups <- function(pool, experiment = get_last_experiment(pool), groups = NULL) {
  image_id_col <- get_experiment_properties(pool, "image_id", experiment) %>% pull(value)
  group_id_col <- get_experiment_properties(pool, "group_id", experiment) %>% pull(value)
  timepoint_id_col <- get_experiment_properties(pool, "timepoint_id", experiment) %>% pull(value)
  
  get_group_data(pool, experiment, groups) %>% 
    left_join(get_object_table(pool, experiment), by = image_id_col, suffix = suffix)
}

create_tracking_lut <- function(pool, experiment = get_last_experiment(pool), groups = NULL) {
  tracked_objects <- get_tracked_objects(pool, experiment)
  stopifnot(length(tracked_objects) > 0)
  tracked_object <- tracked_objects[1]
  
  image_id_col <- get_experiment_properties(pool, "image_id", experiment) %>% pull(value)
  group_id_col <- get_experiment_properties(pool, "group_id", experiment) %>% pull(value)
  timepoint_id_col <- get_experiment_properties(pool, "timepoint_id", experiment) %>% pull(value)
  
  object_col <- f("{tracked_object}_Number_Object_Number")
  par_object_col <- get_object_parent_col(pool, tracked_object, experiment)
  

  all <- get_object_data_with_groups(pool, experiment, groups) %>%
    select(all_of(c(image_id_col, group_id_col, timepoint_id_col, object_col, par_object_col))) %>% 
    collect()
  
  lut <- CPTrackR::createLUT(all, group_vars = !!as.symbol(group_id_col), frame_var = !!as.symbol(timepoint_id_col),
                      obj_var = !!as.symbol(object_col), par_obj_var = !!as.symbol(par_object_col))
  
  get_group_data(pool, experiment, groups) %>% 
    collect() %>%
    left_join(lut, by = c(group_id_col, timepoint_id_col))
  
}

get_unique_group_metadata <- function(pool, experiment = get_last_experiment(pool)) {
  group_id_col <- get_experiment_properties(pool, "group_id", experiment) %>% pull(value)
  get_image_table(pool, experiment) %>%
    select(all_of(group_id_col), starts_with("Image_Metadata")) %>% 
    collect() %>% 
    pivot_longer(-all_of(group_id_col)) %>% 
    group_by(across(c(all_of(group_id_col), name))) %>% 
    filter(n_distinct(value) == 1) %>% # keep only metadata that is same over the group (so not unique to the frame)
    unique() %>% 
    group_by(name) %>% 
    filter(!all(value == 0)) %>% # keep only metadata that is not 0 for all groups (likely means that it's missing)
    pivot_wider()
}

get_sql_group_metadata_col <- function(pool, experiment = get_last_experiment(pool)) {
  group_id_col <- get_experiment_properties(pool, "group_id", experiment) %>% pull(value)
  grouping_tags <- get_cp_info_table(pool, experiment) %>%
    pull(Metadata_GroupingTags) %>%
    jsonlite::fromJSON() %>%
    f("Image", ., .sep = "_")
  get_image_table(pool, experiment) %>%
    select(all_of(group_id_col), all_of(grouping_tags)) %>%
    collect() %>%
    unique()
}


