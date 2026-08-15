program format_a_integer_01
! 'a' editing with an integer i/o item is a legacy Fortran 66 feature
! (deleted in Fortran 77); LFortran supports it but warns.
implicit none
character(len=*), parameter :: fmt = '(*(a))'
integer(1), parameter :: esc = 27_1
integer :: word
character(len=4) :: out4
word = transfer("abcd", 0)
write(out4, "(a)") word
write(*, fmt) esc, '[2J'
end program format_a_integer_01
