module nested_vars_07_module
    implicit none
contains
    subroutine set_scalar(x)
        class(*) :: x(:)
        call set_generic()
    contains
        subroutine set_generic()
            select type(x)
            type is(integer)
                x = x * 2
            end select
        end subroutine set_generic
    end subroutine set_scalar
end module nested_vars_07_module

program nested_vars_07
    use nested_vars_07_module
    implicit none
    integer :: arr(3)
    
    arr = [1, 2, 3]
    call set_scalar(arr)
    
    if (any(arr /= [2, 4, 6])) then
        print *, "Mismatch: ", arr
        error stop
    end if
    
end program nested_vars_07

