#' Pomodoro timer
#'
#' Pomodoro timer that runs as a background job
#'
#' @return NULL
#' @param time_focus focus time in minutes
#' @param short_break short break time in minutes
#' @param long_break long break time in minutes
#' @export
#' @importFrom rstudioapi jobRunScript executeCommand
pomodoro <- function(time_focus = 25, short_break = 5, long_break = 15) {
  code <- paste0("
    counter <- 0
    while(TRUE) {
        Sys.sleep(60 * ", time_focus, ")
        beepr::beep(1)
        rstudioapi::showDialog('Pomodoro timer', 'Pomodoro period finished! Time to take a break!')
        time_break <- ifelse(counter %% 4, ", short_break, ", ", long_break, ")
        Sys.sleep(60 * time_break) # break time
        beepr::beep(5)
        rstudioapi::showDialog('Pomodoro timer', 'Time to get back to work!')
    }")
  script_file <- tempfile()
  write(code, file = script_file)
  job <- jobRunScript(script_file, "pomodoro")
  console <- executeCommand("activateConsole")
}
