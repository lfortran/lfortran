module nested_22_mod
    type :: tmp_type
        integer :: a
    end type tmp_type

contains
    subroutine tmp_sub()
        type(tmp_type), allocatable :: x(:)
        allocate(x(1))
        call inner_sub()
        print *, x(2)%a
        if (size(x) /= 2) error stop
        if (x(2)%a /= 42) error stop
    contains
        subroutine inner_sub()
            type(tmp_type) :: tmp_var
            x = [x, tmp_var]
            x(2)%a = 42
        end subroutine inner_sub
    end subroutine tmp_sub
end module nested_22_mod

program nested_22
    use nested_22_mod
    implicit none
    call tmp_sub()
end program nested_22
