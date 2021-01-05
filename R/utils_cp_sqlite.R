#' @importFrom dplyr tbl collect
NULL

cp_version_lut <- 
  tibble::tribble(
    ~sqlite, ~version, ~version_string,
    "2016-05-03T18:31:00 ac0529e", "2.2.0", "2.2.0 (ac0529e)",
    "4.0.5", "4.0.5", "4.0.5"
  )

# quick and dirty for rapid development
get_sqlite_pool <- function(path = "~/../../stack/00_dump/h5cp_sqlite/test.db" 
                       ) {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = path
  )
}
get_sqlite_alt_pool <- function() {
  get_sqlite_pool(path = "~/../../DefaultDB.db")
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

get_cp_info_table <- function(pool, experiment) {
  table_prefix <- get_experiment_properties(pool, "class_table", experiment) %>% pull(value)
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
