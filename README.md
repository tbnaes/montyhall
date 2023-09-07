# montyhall
This is a package to run the Monty Hall game and an assignment for ASU's PAF514 graduate program course. 

Some background info on the Monty Hall game itself is that this is a classic game show set up where a guest
has three doors to choose from. Behind the doors are two goats and one car. The contestant picks one door from
the three, then is shown one goat from the doors not chosen. The guest then can choose to stay with the initial
pick or change to the other unopened door. Finally, the contestant's door is opened to reveal a goat or a car!

To test if the package installed correctly, try the function create_game(). This should output some combination of
"goat" "goat" "car". 

The primary function of this package is play_game() which will run the entire code and output win or lose for
the contestant based upon staying or changing doors.

The advanced function of this package is play_n_games() which will accept an integer for how many games you
would like to play. This will output a data frame of the probabilities of winning based upon staying or losing 
for n number of games, as well as show the games and the outcomes for each individual playing.

Thanks for visiting my github! 
