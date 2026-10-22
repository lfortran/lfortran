module do_concurrent_20_mod
    implicit none
contains
    pure integer function twice(x)
        integer, intent(in) :: x
        twice = 2 * x
    end function

    subroutine run_test(a)
        integer, intent(out) :: a(3)
        integer :: j

        j = 91

        do concurrent (integer :: j = 1:3)
            a(j) = twice(j)
        end do

        if (j /= 91) error stop
    end subroutine
end module

program do_concurrent_20
    use do_concurrent_20_mod
    implicit none
    integer :: a(3)

    call run_test(a)

    if (any(a /= [2, 4, 6])) error stop

    print *, "PASS"
end program
