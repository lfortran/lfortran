module select_case_03_mod
contains
subroutine empty_select()
    integer :: x
    x = 1
    select case (x)
    end select

    select case (x)
    case default
        x = 2
    end select
    if (x /= 2) error stop

    select case (1)
    end select
    print *, "ok"
end subroutine
end module

program select_case_03
use select_case_03_mod
call empty_select()
end program
