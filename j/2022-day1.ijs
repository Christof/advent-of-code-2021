readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day1-example.txt' NB.< '../inputs/2022-day1-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day1.txt'
data =. readfile input
boxedValues =: (< ;. _2) (data , LF)
boxedValuesWithEmptyEnding =: boxedValues , (< '')
separators =: boxedValuesWithEmptyEnding e. (< '')
grouped =: separators < ;. _2 boxedValuesWithEmptyEnding
sums =: +/ each ". each > each grouped
max =: >./ > sums NB. input answer is 66719

sortedSums =: /:~  > sums
max3Sum =: +/ _3 {. sortedSums NB. input answer is 198551