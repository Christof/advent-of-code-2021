readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day4-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day4.txt'
data =. readfile input
boxedValues =: (< ;. _2) (data , LF)

sectionsContained =. 3 : 0
  words =. ;: > y
  sectionStartA =. ". > {. words
  sectionEndA =. ". > 2 { words
  sectionStartB =. ". > 4 { words
  sectionEndB =. ". > _1 { words

  AInB =. (sectionStartA >: sectionStartB) *. (sectionEndA <: sectionEndB)
  BInA =. (sectionStartB >: sectionStartA) *. (sectionEndB <: sectionEndA)

  AInB +. BInA
)

containedPairs =. sectionsContained each boxedValues
countOfContainedPairs =. +/ > containedPairs NB. input answer is 571

sectionsOverlapped =. 3 : 0
  words =. ;: > y
  sectionStartA =. ". > {. words
  sectionEndA =. ". > 2 { words
  sectionStartB =. ". > 4 { words
  sectionEndB =. ". > _1 { words

  StartAInB =. (sectionStartA >: sectionStartB) *. (sectionStartA <: sectionEndB)
  EndAInB =. (sectionEndA >: sectionStartB) *. (sectionEndA <: sectionEndB)
  StartBInA =. (sectionStartB >: sectionStartA) *. (sectionStartB <: sectionEndA)
  EndBInA =. (sectionEndB >: sectionStartA) *. (sectionEndB <: sectionEndA)

  StartAInB +. EndAInB +. StartBInA +. EndBInA
)

overlappedPairs =. sectionsOverlapped each boxedValues
countOfOverlappedPairs =. +/ > overlappedPairs NB. input answer is 917