program test
    type :: para
        integer :: a
        real :: b
    end type
    type(para), parameter :: temp(3) = [para(8, 5.0E0), para(5, 1.0E0), para(10, 12.0E0)]

    print *, temp
end program
