! Main program without a PROGRAM statement, with internal procedures.
! `lfortran fmt` must not invent a PROGRAM statement for it.
implicit none
integer :: x
x = twice(21)
if (x /= 42) error stop
print *, x
contains
    integer function twice(i)
    integer, intent(in) :: i
    twice = 2*i
    end function twice
end
