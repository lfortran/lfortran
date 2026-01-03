program format_08
    implicit none
    character(:), allocatable :: a
    integer :: b(10)
    a = "xx"
    b = 1
    print "(aai6)", a,"hi",15
    print "(1000(i6))", b
end program