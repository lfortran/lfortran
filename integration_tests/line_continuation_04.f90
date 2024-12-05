program line_continuation_04
    implicit none
    integer :: result, a, b, c
    character(len=20) :: textblock
    textblock = '&!'
    print *, textblock
    if (textblock /= "&!") error stop

    textblock = "apple&
!apple
"
    print *, textblock
    if (textblock /= "apple") error stop

    ! textblock = "apple'&!apple"
    ! if (textblock /= "apple'&!apple") error stop

    a = 1
    b = 2
    c = 3
    ! the below line doesn't have &! in a string
    ! hence, everything after ! is treated as a
    ! comment
    result = a + b +&!addition here
    c
    print *, result
    if (result /= 6) error stop

    textblock = "& !"
    print *, textblock
    if (textblock /= "& !") error stop
end program line_continuation_04
