#pak::pak("parmsam/r-shinylive@feat/encode-decode-url")

create_link_iframe <- function(url, mode = c("app", "editor"), header = TRUE) {
  mode <- match.arg(mode)

  if (mode != "editor") url <- gsub("editor", mode, url)

  if (!header) {
    url <- paste0(url, "&h=0")
  }

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

create_shinylive_links <- function(path) {

  dirs <- list.dirs(path)[-1]

  vapply(
    list.dirs(path)[-1],
    shinylive:::url_encode_dir,
    FUN.VALUE = character(1)
  )
}