#' Pomodoro timer
#'
#' Pomodoro timer that runs as a background job
#'
#' @return NULL
#' @param time_focus focus time in minutes
#' @param time_break break time in minutes
#' @export
#' @importFrom beepr beep
#' @importFrom glue glue
#' @importFrom rstudioapi showDialog
#' @importFrom job job
pomodoro <- function(time_focus = 25, time_break = 5) {
  suppressMessages(
    job(pomodoro = {
      while(TRUE) {
        Sys.sleep(60 * {time_focus}) # focus time
        counter <- counter + 1
        beep(1)
        showDialog('Pomodoro timer', 'Pomodoro period finished! Time to take a break!')
        Sys.sleep(60 * {time_break}) # break time
        beep(5)
        showDialog('Pomodoro timer', 'Time to get back to work!')
      }
    })
  )
}
