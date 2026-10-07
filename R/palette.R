
#' Personal color palette
#'
#' @param ...
#'
#' @return
#' Automatically set the color values for ggplot2
#' @export
#'
zack_color <- function(...) {

  pal <-  unname(palette_light()) %>% rep(100)
  scale_color_manual(values = pal)
}


#' Personal fill palette
#'
#' @param ...
#'
#' @return
#' Automatically set the fill values for ggplot2
#' @export
#'
zack_fill <- function(...) {

  pal <- unname(palette_light()) %>% rep(100)
  scale_fill_manual(values = pal)
}


zack_palette <- function(level,select){
  pal <- palette_light()
  as.vector(pal[1:level])[select]

}


palette_light <- function() {
  c(
    bridge.blue  = "#293851",
    gold         = "#ff9e00",
    bridge.red   = "#91343b",
    timefall     = "#8b977f",
    bb           = "#ffdd7c",
    forest       = "#012F12"
  ) %>% toupper()
}

zack_theme <- function(size) {
  theme_minimal() +
    theme(text = element_text(family="Times New Roman", face="bold", size = size))
}

zack_markdown <- function(){
  cat("output:
  html_document:
    toc: TRUE
    toc_float: TRUE
    toc_depth: 4
    theme: yeti
    number_sections: TRUE
    df_print: paged")
}

mediqr <- function(x){
  median <- round(median(x, na.rm = TRUE), digits = 1)
  q1 <-  round(quantile(x[!is.na(x)], 0.25),1)
  q3 <- round(quantile(x[!is.na(x)], 0.75),1)
  paste(median, " ","(",q1,",",q3,")", sep = "")
}

luid <- function(x){
  length(unique(x))
}

purge <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

dupid <- function(id,data) {
  ave(data$id, data$id, FUN = seq_along)
}

cheatsheet <- function() {
  browseURL("cheatsheet.html")
}

to_monthly <- function(x) {
  as.Date(paste(substr(x,1,7),"01",sep = "-"))
}
