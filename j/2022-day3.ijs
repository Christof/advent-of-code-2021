readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day3-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day3.txt'
data =. readfile input
boxedValues =: (< ;. _2) (data , LF)

sharedItem =. 3 : 0
  items =. > y
  halfLength =. ($ items) % 2
  a =. halfLength {. items
  b =. (-halfLength) {. items
  matchingItemInA =. a e./ b
  shared =. {. matchingItemInA # a
)

sharedItems =. sharedItem each boxedValues

lowercaseLetters =. a. {~ 97+i.26
uppercaseLetters =. a. {~ 65+i.26
letters =. lowercaseLetters , uppercaseLetters

NB. lookup is zero-based, so we add the length of sharedItems
sumOfPriorities =. ($ sharedItems) + +/ letters i. > sharedItems NB.input answer is 7674