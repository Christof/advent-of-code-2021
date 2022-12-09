readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day7-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day7.txt'

data =. readfile example
boxedValues =: (< ;. _2) (data , LF)

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
      if. *./ '$ cd ' = 5 {. line do. 
        currentDir =. currentDir, (5 }. line), '/'
      elseif. *./ 'dir ' = 4 {. line do.
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
)

process boxedValues