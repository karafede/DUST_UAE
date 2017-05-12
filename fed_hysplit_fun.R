fed_run_hysplit <- function (latitude = 51.5, longitude = -0.1, start = NA, end = NA, 
          interval = "3 hour", runtime = -96, start_height = 10, model_height = 10000, 
          hysplit_exec = "exec/", hysplit_input, hysplit_output = "hysplit_output/", 
          delete = FALSE, drop = TRUE, site = NA, source = NA, verbose = FALSE) 
{
  hysplit_exec <- path.expand(hysplit_exec)
  hysplit_input <- path.expand(hysplit_input)
  hysplit_output <- path.expand(hysplit_output)
  hysplit_exec <- str_c(hysplit_exec, .Platform$file.sep)
  hysplit_input <- str_c(hysplit_input, .Platform$file.sep)
  hysplit_output <- str_c(hysplit_output, .Platform$file.sep)
  files_old <- list.files(hysplit_output, full.names = TRUE)
  if (delete) {
    message("Deleting old files...")
    file.remove(files_old)
  }
  else {
    while (length(files_old) > 1) {
      input <- readline("There are files in the output directory. Should these be deleted?\n")
      input <- str_to_upper(input)
      input <- ifelse(input %in% c("YES", "Y", "T", "TRUE", 
                                   "TR", "TRU"), TRUE, FALSE)
      if (input) {
        message("Deleting old files...")
        file.remove(files_old)
        break
      }
      else {
        break
      }
    }
  }
  # start <- parse_date_arguments(start, "start")
  # end <- parse_date_arguments(end, "end")
  
  start <- strptime(start, "%y/%m/%d %H:%M:%S")
  end <- strptime(end, "%y/%m/%d %H:%M:%S")
  
  coordinates <- str_c(latitude, longitude, start_height, sep = " ")
  control_file <- file.path(hysplit_exec, "CONTROL")
  wd <- getwd()
  date_sequence <- seq(start, end, interval)
  message(str_c("Running ", length(date_sequence), " HYSPLIT trajectories..."))
  progress <- ifelse(verbose, "none", "time")
  plyr::l_ply(date_sequence, run_trajectory, hysplit_exec = hysplit_exec, 
              hysplit_input = hysplit_input, hysplit_output = hysplit_output, 
              control_file = control_file, coordinates = coordinates, 
              runtime = runtime, model_height = model_height, verbose = verbose, 
              .progress = progress)
  setwd(wd)
  message("Binding HYSPLIT files...")
  file_list <- list.files(hysplit_output, "hysplit_output.txt", 
                          full.name = TRUE)
  df <- plyr::ldply(file_list, read_hysplit_file, drop = drop)
  df$start_height <- start_height
  if (!is.na(site)) 
    df$site <- site
  if (!is.na(source)) 
    df$source <- source
  if (delete) 
    file.remove(file_list)
  df
}
