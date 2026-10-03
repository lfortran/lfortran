module nested_25_mod
contains
    subroutine outer(i)
        integer(4), intent(in) :: i
    contains
        subroutine inner()
            integer(4) :: i
            i = 1
            if (i /= 1) error stop
        end subroutine inner
    end subroutine outer
end module nested_25_mod

program nested_25
    use nested_25_mod
    implicit none
    call outer(5)
    print *, "ok"
end program nested_25
