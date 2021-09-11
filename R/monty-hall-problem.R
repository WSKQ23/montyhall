#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param
#'   No arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Host selected one of the doors before the game start.
#'
#' @description
#'   `select_door()` selected a door for the game to start.
#'
#' @details
#'   The host selected a door among the three doors that are closed. The
#'   host should select a door without a car.
#'
#' @param
#'   No arguments are used by the function.
#'
#' @return
#'   The function returns a numeric vector
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )
}



#' @title
#' The contestant to open one of the door.
#'
#' @description
#'   `open_goat_door()` Contestant opened one of the doors with goat.
#'
#' @details
#'   The contestant selects a door, then the host opens a door to reveal a goat
#'   and the contestant was given a chance to select again.
#'
#' @param
#'   There are arguments used by the function.
#'
#' @return
#'   The function returns a numeric vector.
#'
#' @examples
#'   open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)

   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door )
}



#' @title
#' The contestant decided to change a door when given chance to play.
#'
#' @description
#'   `change_door()` The contestant was given the change to either stay or
#'   switch.
#'
#' @details
#'   As planned in the game the contestant has the opportunity to decided
#'   whether to continue with the game or not. In this case the decided to
#'   switch. the functions the final picks of the contestant.
#'
#' @param
#'   There are arguments used by the function.
#'
#' @return
#'   The function returns a numeric vector
#'
#' @examples
#'   change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )
}



#' @title
#' The status of the game is shown whether the contestant win or lose
#'
#' @description
#'   `determine_winner()` There two things involve in this case. Either the
#'   contestant win or lose.
#'
#' @details
#'   The contestant makes the final picks of the doors. The final pick
#'   determines whether the contestant win the or lose the game.
#'
#' @param
#'   There are arguments used by the function.
#'
#' @return
#'   The function returns a character vector.
#'
#' @examples
#'   determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   The strategy used by the contestant in the game.
#'
#' @description
#'   `determine_winner()` The strategy used by the contestant assessed in this
#'   session of the functions.
#'
#' @details
#'   The pay off of the strategy used by the contestant was determine. Base on
#'   the assessment of the strategy contestant was able to know whether the
#'   strategy used is good.
#'
#' @param
#'   No arguments used by the function.
#'
#' @return
#'   The function return character vector
#'
#' @examples
#'   new.game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}




