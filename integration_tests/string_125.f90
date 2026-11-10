! A function with a long fixed-length character result called in a loop
! must not leak the result (#6444)
module string_125_mod
    implicit none
contains
    function ff() result(r)
        character(10000) :: r
        r = "hello"
    end function ff
end module string_125_mod

program string_125
    use string_125_mod
    implicit none
    character(20) :: x
    integer :: i
    do i = 0, 1000
        x = ff()
    end do
    print *, x
    if (x /= "hello") error stop
    if (len(ff()) /= 10000) error stop
end program string_125
