readfile =: 1!:1
example =. < '/home/christof/Documents/advent-of-code/inputs/2022-day7-example.txt'
input =. < '/home/christof/Documents/advent-of-code/inputs/2022-day7.txt'

data =. readfile example
boxedValues =: (< ;. _2) (data , LF)

process =. 3 : 0
  currentDir =. '/'
  for_i. y do.
    line =. > i
    smoutput 'output' ; line; currentDir
    select. i
    case. '$ cd /' do. currentDir =. '/'
    case. '$ cd ..' do. currentDir =. (currentDir i: '/') {. currentDir
    case. '$ ls' do. smoutput 'ls in'; currentDir
    case. 'dir' do. smoutput 'dir skipping'
    case. do.
      p5 =. 5 {. line
      cd =. '$ cd ' = p5
      NB. smoutput 'isCd'; cd; p5
      if. *./ '$ cd ' = 5 {. line do. 
        currentDir =. currentDir, (5 }. line), '/'
      else.
        l =. $ line
        smoutput 'filesize'; line; l
      end.
    end.
  end.
)

process boxedValues