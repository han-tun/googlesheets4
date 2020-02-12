#' Update cell(s) content
#'
#' Update the content of individual cell(s)
#'
#' @param df dataframe with columns `loc` for A1 type location and `value` with value to update with
#' @template ss
#' @param sheet Sheet to update
#' @param valueInputOption optional, determines if input is raw or should be parsed
#' @return The input `ss`, as an instance of [`sheets_id`]
#'
#' @export
sheets_sheet_update_cells <- function(df,ss,sheet,valueInputOption=c("RAW","USER_ENTERD")){
  ssid <- as_sheets_id(ss)
  check_sheet(sheet, nm = "sheet")

  x <- sheets_get(ssid)
  s <- lookup_sheet(sheet, sheets_df = x$sheets)

  range <- df$loc

  msg <- glue::glue("Updating sheet {s$name} from {sq(x$name)} at {paste0(range,collapse=', ')}")
  message_collapse(msg)



  sid <- s$id

  parameters <- df %>%
    select(loc,value) %>%
    apply(1,function(x){
      list(values=list(list(x[[2]])),
           valueInputOption = valueInputOption[1],
           spreadsheetId=as.character(ssid),
           range=paste0(sheet,"!",x[[1]]))
    })

  response <- lapply(parameters,function(p){
    req <- request_generate(
      endpoint = "sheets.spreadsheets.values.update",
      params = p
      )
    raw_response <- request_make(req)
    gargle::response_process(raw_response)
  })

  invisible(ssid)

}


