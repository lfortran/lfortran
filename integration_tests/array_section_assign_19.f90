! An array assigned to a substring of each element of a character array
! section has the shape of the section. Here it has 4 elements and the
! section has 3, which is reported at run time.
program array_section_assign_19
    implicit none
    character(len=4) :: names(3)
    character(len=2) :: t(4)
    integer :: n
    t = ['xy', 'zw', 'uv', 'qr']
    names = ['abcd', 'efgh', 'ijkl']
    n = 4
    names(:)(2:3) = t(1:n)
    print *, names
end program
