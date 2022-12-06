readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day5-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day5.txt'
data =. readfile example
boxedValues =: (< ;. _2) (data , LF)
separation =. boxedValues e. (< '')
stacksWithLabels =. separation > ;. _2 boxedValues
stacksData =. _1 }."2 stacksWithLabels
stacks =. 1 5 9 {"1 stacksData
s =. 0 { stacks
transposed =. |: s
listOfStacks =. removeSpaces each , _3 <\"1 transposed

removeSpaces =. 3 : 0
  (-. ' ' = y) # y
)

movesData =. separation > ;. _1 boxedValues
movesTable =. ;: movesData
moves =. 1 3 5 {"1 movesTable NB. count | from | to
m =. 3 <\ ,m NB. List of triples count, from, to

move =. 3 : 0
  from =. 0 { y
  to =. 1 { y
  fromStack =. from { x
  item =. {. fromStack
  newFromStack =. }. fromStack
)
