
readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day2-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day2.txt'
data =. readfile input
boxedValues =: (< ;. _2) (data , LF)

NB. A Rock
NB. B Paper
NB. C Scissors

NB. 1 pt for X (Rock)
NB. 2 pts for Y (Paper)
NB. 3 pts for Z (Scissors)

ownPlays =: > {: each  boxedValues
pointsFromOwnPlays =: (+/ ownPlays = 'X') + (2 * +/ ownPlays = 'Y') + (3 * +/ ownPlays = 'Z')

NB. 0 pt for loosing (X)
NB. 3 pts for draw (Y)
NB. 6 pts for win (Z)

plays =: 'C X'; 'B Z'; 'A Y'; 'A Z'; 'C Y'; 'B X'; 'A X'; 'B Y'; 'C Z'
pointsForPlays =: 6 6 6 0 0 0 3 3 3

pointsFromResults =: +/ (plays i. boxedValues) { pointsForPlays

totalPoints =: pointsFromOwnPlays + pointsFromResults NB. answer for input 15572
NB. C X -> loosing (0) -> Scissors beat Paper (2) -> 2
NB. B Z -> win (6) -> Paper is beaten by Scissors (3) -> 9
NB. A Y -> draw (3) -> both Rock (1) -> 4
NB. A Z -> win (6) -> Rock is beaten by Paper (2) -> 8
NB. C Y -> draw (3) -> both Scissors (3) -> 6
NB. B X -> lossing (0) -> Paper beats Rock (1) -> 1
NB. A X -> loosing (0) -> Rock beats Scissors (3) -> 3
NB. B Y -> draw (3) -> both Paper (2) -> 5
NB. C Z -> win (6) -> Scissors are beaten by Rock (1) -> 7
totalPointsForPlaysPart2 =: 2 9 4 8 6 1 3 5 7
solutionPart2 =: +/ (plays i. boxedValues) { totalPointsForPlaysPart2 NB. answer for input 16098