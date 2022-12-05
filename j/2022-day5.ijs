readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day5-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day5.txt'
data =. readfile example
boxedValues =: (< ;. _2) (data , LF)
separation =. boxedValues e. (< '')
stacksWithLabels =. separation > ;. _2 boxedValues
stacksData =. _1 }."2 stacksWithLabels
stacks =. 1 5 9 {"1 stacksData

movesData =. separation > ;. _1 boxedValues
movesTable =. ;: movesData
moves =. 1 3 5 {"1 movesTable NB. count | from | to