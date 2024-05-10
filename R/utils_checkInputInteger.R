#' checkInputInteger
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#' @param input_data reactive shiny object representing data
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
checkInputInteger <- function(input_env, input_id, input_data) {

  if (!is.null(input_env[[input_id]]) && !is.integer(input_data[[input_env[[input_id]]]])) {
    shinyFeedback::showFeedbackWarning(
      inputId = input_id
      ,text = "The data must be in integer format!"
    )
  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
