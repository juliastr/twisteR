#' Function for spinning the Twister.
#'
#' \code{twister} is the virtual equivalent of running the Twister spinner
#' and finding out which limb to place on which colour.
#'
#' @param .player An optional string containing the name of the player.
#' @param .speech A logical value indicating whether the instruction should be
#'     read aloud by the computer, currently only working under Mac OS.
#'
#' @return A console message containing the play instruction and a plot illustrating it.
#'
#'
#' @export
#'
#' @examples
#' twister("Dude")

twister <- function(.player = NA, .speech = FALSE){
  limbs <- c("right hand", "left hand", "right foot", "left foot")
  colours <- c("red", "green", "blue", "yellow")

  x <- sample(limbs, 1)
  y <- sample(colours, 1)

  msg <- ifelse(is.na(.player),
                paste0("Place your ", x, " on ", y, "!"),
                paste0(.player, ", place your ", x, " on ", y, "!"))
  message(msg)

  pic <- ggplot2::ggplot(data.frame(x=1, y=1), ggplot2::aes(x, y)) +
    ggplot2::geom_point(shape = 16, size = 140, colour = y) +
    ggplot2::annotate("text", 1, 1, label = toupper(gsub(" ", "\n", x)),
                      fontface = "bold", size = 28) +
    ggplot2::theme(line = ggplot2::element_blank(),
                   text = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()
    )
  print(pic)

  if (.speech == TRUE){
    Sys.sleep(0.3)
    twister_speech(.message = msg)
  }
}


#' Function for running a game of Twister.
#'
#' \code{auto_twister} lets you run a game of Twister with a set of players
#' for a specified number of rounds.
#'
#' @param .rounds An integer value of the number of rounds to be played.
#' @param .wait An integer value of the seconds to wait between play instructions.
#' @param .players A character vector containing the names of all players.
#' @param .speech A logical value indicating whether the instruction should be
#'     read aloud by the computer, currently only working under Mac OS.
#'
#' @return A sequence of console messages containing the play instruction and
#'     accompanying plots that illustrate them.
#'
#' @export
#'
#' @examples
#' auto_twister(.rounds = 2, .wait = 0, .players = c("Alice", "Bob"))

auto_twister <- function(.rounds = 10, .wait = 10, .players = NA, .speech = FALSE) {
  operating_system <- Sys.info()[['sysname']]
  if (.speech ==TRUE &  operating_system == "Windows") {
    message(paste0("Speech output does not work under ", operating_system, "."))
    .speech <- FALSE
  }

  play_sequence <- rep(.players, .rounds)

  for (player in play_sequence) {
    twister(.player = player, .speech = .speech)
    Sys.sleep(.wait)
  }
}

#' Helper function for speech output.
#'
#' \code{twister_speech} is a helper function that is called in case the play instruction
#' should be read aloud by the computer.
#'
#' @param .message A string containing the play instruction.

twister_speech <- function(.message) {
  switch(Sys.info()[['sysname']],
         Windows= {print("Speech output is not supported by Windows.")},
         Linux  = {system("espeak", input=.message)},
         Darwin = {system(paste0("say ", .message))})
}
