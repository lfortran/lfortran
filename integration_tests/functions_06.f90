module functions_06_m
implicit none

contains

    function trim_wrapper(s) result(r)
        character(len=*) :: s
        character(len=len(s)) :: r
        r = trim(s)
    end function

    function int_return(s) result(r)
        character(len=*) :: s
        integer :: r(len(s))
        integer :: i
        do i = 1, len(s)
            r(i) = 2
        end do
    end function

    function real_return(s) result(r)
        character(len=*) :: s
        real :: r(len(s))
        integer :: i
        do i = 1, len(s)
            r(i) = 2.0
        end do
    end function

end module

program functions_06
    use :: functions_06_m, only: trim_wrapper, int_return, real_return
    implicit none

    character(len=5) :: string = "  abc"
    print *, trim_wrapper(string)

    print *, int_return(string)

    print *, real_return(string)

end program
