program format_117
implicit none
type item
    character(3) :: first
    character(5) :: second
end type
type(item) :: empty(0)
character(40) :: line
integer :: ios
open(10, status='scratch', action='readwrite')
! Intrinsic I/O transfers the components of each element; a zero-size
! array has no components to transfer, even inside a runtime-bound dummy.
write(10, "('fixed:',*(1x,a))") empty, 'end'
call output(empty, 10)
rewind(10)
read(10, '(a)') line
if (line /= 'fixed: end') error stop 'empty fixed derived array'
read(10, '(a)') line
if (line /= 'runtime: end') error stop 'empty runtime derived array'
read(10, '(a)', iostat=ios) line
if (ios >= 0) error stop 'extra empty-array record'
close(10)
contains
subroutine output(a, unit)
type(item), intent(in) :: a(:)
integer, intent(in) :: unit
write(unit, "('runtime:',*(1x,a))") a, 'end'
end subroutine
end program
