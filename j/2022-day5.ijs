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
removeSpaces =. 3 : 0
  (-. ' ' = y) # y
)
listOfStacks =. removeSpaces each , _3 <\"1 transposed


movesData =. separation > ;. _1 boxedValues
movesTable =. ;: movesData
moves =. 1 3 5 {"1 movesTable NB. count | from | to
m =. > 3 <\ ,moves NB. List of triples count, from, to

move =. 4 : 0
  from =. (". > 1 { y) - 1
  to =. (". > 2 { y) - 1
  fromStack =. > from { x
  item =. {. fromStack
  newFromStack =. }. fromStack
  new =. ((i. from) { > x) , newFromStack , ((from+1+i.((#x)-(from+1))) { >x)
  toStack =. > to { x
  newToStack =. item, toStack
  newListOfStacks =. ((i. to) { new) , newToStack , ((to+1+i.((#new)-(to+1))) { new)
)

moveWithCount =. 4 : 0
  newCount =. (". > 0 { y) - 1
  newList =. x move y

  if. newCount > 0 do. newList moveWithCount ((< newCount) 0} y) else. newList end.
)
