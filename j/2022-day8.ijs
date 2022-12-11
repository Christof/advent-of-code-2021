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
run input