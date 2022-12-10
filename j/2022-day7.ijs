readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day7-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day7.txt'

data =. readfile example
boxedValues =: (< ;. _2) (data , LF)

startsWith =. 4 : 0
  *./ y = (# y) {. x
)

dirContainsFile =. 4 : 0
  file =. > y
  dir =. (> x) , '/'
  if. 1 = # (> x) do.
    dir =. '/'
  end.
  l =. 1 + file i: '/'
  dirOfFile =. l {. file
  if. (# dirOfFile) = (# dir) do.
    *./ dirOfFile = dir
  else.
    0
  end.
)

process =. 3 : 0
  currentDir =. '/'
  directories =. 0 $ 0
  files =. 0 $ 0
  sizes =. 0 $ 0
  for_i. y do.
    line =. > i
    smoutput 'output' ; line; currentDir
    select. i
    case. '$ cd /' do. currentDir =. '/'
    case. '$ cd ..' do. currentDir =. (1 + (_1 }. currentDir) i: '/') {. currentDir
    case. '$ ls' do. smoutput 'ls in'; currentDir
    case. do.
      if. line startsWith '$ cd ' do. 
        currentDir =. currentDir, (5 }. line), '/'
      elseif. line startsWith 'dir ' do.
        newDir =. < currentDir , 4 }. line
        directories =. directories , newDir
        smoutput 'Add dir'; newDir
      else.
        spaceIndex =. line i. ' '
        size =. ". spaceIndex {. line
        file =. currentDir , (spaceIndex + 1) }. line
        files =. files , <file
        sizes =. sizes , size
        smoutput file; size
      end.
    end.
  end.

  smoutput 'directories'; directories
  smoutput files
  smoutput sizes


  directorySizes =. 0 $ 0
  for_d. directories do.
    NB. smoutput d

    filesInDir =. > (d dirContainsFile each files)
    NB. smoutput 'filesInDir' ; filesInDir
    f =. filesInDir # files
    s =. filesInDir # sizes
    totalDirSize =. +/ s
    directorySizes =. directorySizes , totalDirSize

    NB. smoutput totalDirSize
  end.

  for_d. directories do.
    NB. smoutput d
    dirsInDir =. > (d dirContainsFile each directories)
    d =. dirsInDir # directories
    addDirSize =. +/ dirsInDir # directorySizes
    fullSize =. addDirSize + d_index { directorySizes
    fullSize d_index} directorySizes

    NB. smoutput fullSize
  end.

  smoutput 'directorySizes'; directorySizes
  +/ (directorySizes <: 100000) # directorySizes
)

process boxedValues