! copy of integration_tests/format_08.f90 but splits edit
! descriptor by separating it with commas i.e. changes
! "(aai6)" to "(a,a,i6)", which is exactly how GFortran
! also expects it as well, see:
! https://gcc.gnu.org/onlinedocs/gfortran/Commas-in-FORMAT-specifications.html
program format_08s
    implicit none
    character(:), allocatable :: a
    integer :: b(10)
    a = "xx"
    b = 1
    print "(a,a,i6)", a,"hi",15
    print "(1000(i6))", b
end program