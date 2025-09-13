program print5
    implicit none
    character(len=:), allocatable :: s

    s = 'ABC' // achar(0) // 'XYZ'

    print *, s
end program print5
