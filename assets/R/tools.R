# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Used by export_apps_json
get_apps_info <- function(appsdir, destdir, quiet = FALSE) {
  apps_info <- list()
  apps_info$category <- tail(strsplit(list.dirs(appsdir)[1], "/")[[1]], n = 1)
  apps_info$apps <- lapply(
      list.dirs(appsdir)[-1],
      \(appdir) {
          app_info <- shinylive:::app_info_obj(appdir, subdir = "", shinylive:::read_app_files(appdir, destdir))
          app_info$about <- app_info$files[[2]]$content
          app_info$title <- tail(strsplit(app_info$appdir, "/")[[1]], n = 1)
          # clean up dummy metadata file
          app_info$files[[2]] <- NULL
          app_info$appdir <- NULL
          app_info$subdir <- NULL
          attr(app_info, "class") <- NULL
          app_info
      }
  )
  
  dropNulls(apps_info)
}

#' This function is supposed to be called from a top level folder containing
#' sub-folder(s) with shinyapps. Each subfolder will be a different category in shinylive.
#' Within each subfolder, each subsubfolder will be an app example. For each app example,
#' adding a about.txt adds some description label to the shinylive app. The app title is given
#' by the app subsubfolder name.
export_apps_json <- function(appsdir, destdir, quiet = FALSE) {
  res <- list()
  res$engine <- "r"
  res$examples <- lapply(list.dirs(appsdir, recursive = FALSE), get_apps_info, destdir = destdir)

  app_destdir <- fs::path(destdir)
  app_json_output_file <- fs::path(app_destdir, "examples.json")
  fs::dir_create(app_destdir)
  cli::cli_progress_step("Writing {.path {app_json_output_file}}")

  #jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE)

  jsonlite::write_json(list(res), path = app_json_output_file,  auto_unbox = TRUE, pretty = TRUE)
  cli::cli_progress_done()
  invisible(app_json_output_file)
}

#' Create iframe container for shinylive apps. Useful for quarto slides.
create_link_iframe <- function(url) {
  shiny::tags$iframe(
    # To allow the content to fill the full screen card
    class = "html-fill-item",
    src = url,
    height = "800",
    width = "100%",
    style = "border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem;",
    allowfullscreen = "",
    allow = "autoplay",
    `data-external` = "1"
  )
}