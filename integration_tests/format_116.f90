module character_output
implicit none
contains
subroutine output(a, unit)
character(*), intent(in) :: a(:)
integer, intent(in) :: unit
integer :: i, j
character(30) :: fmt
write(unit, "('data:',*(1x,a))") (trim(a(i)), i=1,size(a)), 'end'
write(unit, "('whole: ',*(a,'|'))") a, 'end'
write(unit, "('reverse:',*(1x,a))") (trim(a(i)), i=size(a),1,-1), 'end'
write(unit, "('joined:',*(:,1x,a))", advance='no') (trim(a(i)), i=1,size(a))
write(unit, '(1x,a)') 'end'
fmt = '(a10,*(1x,a10))'
write(unit, fmt) 'index', (trim(a(i)), i=1,size(a))
write(unit, "('empty:',*(1x,a))") (trim(a(i)), i=1,0), 'end'
write(unit, "('two:',*(1x,a))") (trim(a(i)), i=1,size(a)), (trim(a(j)), j=1,size(a)), 'end'
write(unit, "('nested:',*(1x,a))") ((trim(a(i)), i=1,size(a)), j=1,2), 'end'
end subroutine
subroutine output_empty(a, unit)
character(*), intent(in) :: a(:)
integer, intent(in) :: unit
integer :: i
write(unit, "('runtime-empty:',*(1x,a))") (trim(a(i)), i=1,size(a)), 'end'
end subroutine
end module

program format_116
use character_output
implicit none
character(5) :: words(3)
character(80) :: line
integer :: ios
words = [character(5) :: 'red', 'green', 'blue']
open(10, status='scratch', action='readwrite')
call output(words, 10)
call output_empty(words(:0), 10)
rewind(10)
read(10, '(a)') line
if (line /= 'data: red green blue end') error stop 'implied-do format restarted'
read(10, '(a)') line
if (line /= 'whole: red  |green|blue |end|') error stop 'whole array field lengths'
read(10, '(a)') line
if (line /= 'reverse: blue green red end') error stop 'implied-do item order'
read(10, '(a)') line
if (line /= 'joined: red green blue end') error stop 'nonadvancing implied-do'
read(10, '(a)') line
if (line /= '     index        red      green       blue') error stop 'header format cursor'
read(10, '(a)') line
if (line /= 'empty: end') error stop 'empty implied-do siblings'
read(10, '(a)') line
if (line /= 'two: red green blue red green blue end') error stop 'multiple implied-do siblings'
read(10, '(a)') line
if (line /= 'nested: red green blue red green blue end') error stop 'nested implied-do'
read(10, '(a)') line
if (line /= 'runtime-empty: end') error stop 'runtime empty implied-do'
read(10, '(a)', iostat=ios) line
if (ios >= 0) error stop 'extra formatted record'
close(10)
end program
