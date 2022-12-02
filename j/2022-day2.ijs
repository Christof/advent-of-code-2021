
readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day2-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day2.txt'
data =. readfile input
boxedValues =: (< ;. _2) (data , LF)

NB. 1 pt for X (Rock)
NB. 2 pts for Y (Paper)
NB. 3 pts for Z (Scissors)

ownPlays =: > {: each  boxedValues
pointsFromOwnPlays =: (+/ ownPlays = 'X') + (2 * +/ ownPlays = 'Y') + (3 * +/ ownPlays = 'Z')

NB. 0 pt for loosing
NB. 3 pts for draw
NB. 6 pts for win

plays =: 'C X'; 'B Z'; 'A Y'; 'A Z'; 'C Y'; 'B X'
pointsForPlays =: 6 6 6 0 0 0 3

pointsFromResults =: +/ (plays i. boxedValues) { pointsForPlays

totalPoints =: pointsFromOwnPlays + pointsFromResults NB. answer for input 15572