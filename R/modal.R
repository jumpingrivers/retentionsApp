#' Can you download the data in the modal?
#'
#' Determines the modal footer for the sankey pop-up table.
#' @param retention_version is the app 'public' or 'private'?
get_modal_footer = function(retention_version) {
  if (retention_version == "private") {
    modal_footer = shiny::tagList(
      shiny::modalButton("Close"),
      shiny::actionButton("download-csv",
                          "Download as CSV",
                          onclick = "Reactable.downloadDataCSV('sankey-data')")
    )
  } else {
    modal_footer = shiny::tagList(
      shiny::modalButton("Close")
    )
  }
  return(modal_footer)
}
