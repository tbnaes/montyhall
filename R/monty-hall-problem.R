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
#' @param ... no arguments are used by the function.
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
#' #'   Contestant will select a door for the Monty Hall Problem game.
#' 
#' @description
#' #'   `select_door()` randomly chooses a door among the 3 doors.
#'   
#' @details
#' #'   The game setup has 3 possibilities, so the contestant must choose
#'    1 of 3 doors. 
#'   
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 1 numerical vector
#'   indicating the initial pick of the contestant.
#'   
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' #'   One of the two goat doors is opened by the host.
#' 
#' @description
#' #'   `open_goat_door()` looks behind the scenes at which doors have goats 
#'   and opens one goat door that has not been chosen by the contestant.
#'   
#' @details
#' #'   The game requires a goat door to be opened after the contestant
#'    makes the initial selection. This will lead into the contestant 
#'    choosing to stay or change doors in the following stage of the game. 
#'   
#' @param ... game argument is from create_game function and a.pick is from the
#'    select_door function.
#' 
#' @return The function returns a length 1 numerical vector
#'   indicating the position of a goat that has not been chosen by the guest.
#'   
#' @examples
#'   open_goat_door(c("goat","goat","car"), 1)
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' #'   The contestant chooses to stay with initial pick of door or 
#'    change to the other door not revealed.
#' 
#' @description
#' #'   `change_door()` works from a stay argument that indicates
#'    whether contestant will stick with initial pick or go with
#'    the other option.
#'   
#' @details
#' #'   At this point in the Monty Hall game, the guest has been shown
#'    one door with a goat behind it. They are either already paired 
#'    with a winning door or the other door has a car behind it. The guest
#'    now has the option to stay with their initial pick or change doors.
#'    This code takes in which door has been opened, and which doors 
#'    have a goat or a car which is not known to the contestant. The 
#'    change leads into determining a winner or loser in the final. 
#'   
#' @param ... the stay argument takes a logical vector for staying or
#'    changing doors. The opened.door argument is from the 
#'    open_goat_door function and the a.pick is from the select_door
#'    function.
#' 
#' @return The function returns a length 1 numerical vector
#'   indicating the position of the contestant choice.
#'   
#' @examples
#'   change_door(stay=T,1,1)
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
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#' #'   The contestant is either a winner or a loser!
#' 
#' @description
#' #'   `determine_winner()` takes all previous return statements
#'    and reveals whether the contestant is a winner or a loser.
#'   
#' @details
#' #'   At this last stage of the game, the contestant has already
#'    been given the choice to stay or change doors. Their final
#'    pick is then measured up against what the code knows to be
#'    the winner or the loser. If the guest's door is paired with
#'    car, then they are a winner of the Monty Hall game. 
#'   
#' @param ... final.pick is returned from the change_door function. 
#'    game pulls from the initial arrangement of two goats and a car.
#' 
#' @return The function returns a length 1 character vector
#'   indicating the final door and game pairing.
#'   
#' @examples
#'   determine_winner(1,c("goat","goat","car"))
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
#' #'   Pay the Monty Hall game in its entirety.
#' 
#' @description
#' #'   `play_game()` uses the entire code already shown to
#'   output the win or loss based on staying or changing doors.
#'   
#' @details
#' #'   The game setup has already been established by the previous
#'    code, so all that's left is to play the Monty Hall game. By
#'    using this function, all previous functions are called in order
#'    such that the final output is a data frame based on the options
#'    to stay or change doors.  
#'   
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a 2 by 2 data frame
#'   indicating the Win or Loss matching with Stay or 
#'   Change doors.
#'   
#' @examples
#'   play_game()
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






#' @title
#' #'   Play the Monty Hall game as many times as you'd like!
#' 
#' @description
#' #'   `play_n_games()` is much like the previous function where
#'    you can run the entire setup, but this function allows for
#'    an argument of how many games to be played.
#'   
#' @details
#' #'   The Monty Hall game allows for some serious consideration
#'    of probability statistics. Knowing that some will want to
#'    run this game several times in order to see the chances
#'    and study the options, this function has a built in number
#'    taker that determines how many times the game will be played. 
#'   
#' @param ... the n argument takes an integer.
#' 
#' @return The function returns a data frame of the outcomes.
#'   
#' @examples
#'   play_n_games(100)
#'   
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}
