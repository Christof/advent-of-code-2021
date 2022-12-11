readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day8-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day8.txt'

run =. 3 : 0
  data =. readfile y
  grid =. ". each (, ;. _2) (data , LF)

  sideLength =. {. $ grid
  borderCount =. (4 * sideLength) - 4

  visibleCount =. borderCount

  i =. 1 + i. (sideLength - 2)
  for_row. i do.
      for_col. i do.
        treeRow =. > row { grid
        treeColumn =. > col {"1 grid
        treeHeight =. col { treeRow
        smoutput 'row: '; row; ' col: '; col; treeHeight

        leftIndices =. i. col
        allSmallerLeft =. *./ (leftIndices { treeRow) < treeHeight

        rightIndices =. col + 1 + i. ((sideLength - col) - 1)
        allSmallerRight =. *./ (rightIndices { treeRow) < treeHeight

        topIndices =. i. row
        allSmallerTop =. *./ (topIndices { treeColumn) < treeHeight
        bottomIndices =. row + 1 + i. ((sideLength - row) - 1)
        allSmallerBottom =. *./ (bottomIndices { treeColumn) < treeHeight
        smoutput allSmallerLeft; allSmallerRight; allSmallerTop; allSmallerBottom

        visibleCount =. visibleCount + +./allSmallerLeft, allSmallerRight, allSmallerTop, allSmallerBottom
      end.
  end.

  visibleCount
)

run example
run input NB. 1684

runPart2 =. 3 : 0
  data =. readfile y
  grid =. ". each (, ;. _2) (data , LF)

  sideLength =. {. $ grid
  borderCount =. (4 * sideLength) - 4

  maxScore =. 0

  i =. 1 + i. (sideLength - 2)
  for_row. i do.
      for_col. i do.
        treeRow =. > row { grid
        treeColumn =. > col {"1 grid
        treeHeight =. col { treeRow
        smoutput 'row: '; row; ' col: '; col; treeHeight

        leftIndices =. i. col
        smallerLeft =. |. (leftIndices { treeRow) < treeHeight
        leftScore =. (# leftIndices) <. 1 + {. +/;._2 (smallerLeft, 0)

        rightIndices =. col + 1 + i. ((sideLength - col) - 1)
        smallerRight =. (rightIndices { treeRow) < treeHeight
        rightScore =. (#rightIndices) <. 1 + {. +/;._2 (smallerRight, 0)

        topIndices =. i. row
        smallerTop =. |. (topIndices { treeColumn) < treeHeight
        topScore =. (#topIndices) <. 1 + {. +/;._2 (smallerTop, 0)

        bottomIndices =. row + 1 + i. ((sideLength - row) - 1)
        smallerBottom =. (bottomIndices { treeColumn) < treeHeight
        bottomScore =. (#bottomIndices) <. 1 + {. +/;._2 (smallerBottom, 0)

        smoutput topScore; leftScore; rightScore; bottomScore

        score =. leftScore * rightScore * topScore * bottomScore
        smoutput 'score'; score
        maxScore =. maxScore >. score
      end.
  end.

  maxScore
)

runPart2 example
runPart2 input NB. 486540