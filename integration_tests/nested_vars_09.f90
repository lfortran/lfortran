module nested_vars_09_mod
    implicit none

    abstract interface
        subroutine callback_func(i, n)
            integer, intent(in) :: i
            integer, intent(in) :: n
        end subroutine callback_func
    end interface

contains

    subroutine invoker(cb)
        procedure(callback_func) :: cb
        call cb(1, 3)
    end subroutine invoker

    subroutine outer(vec)
        character(len=*), dimension(:), allocatable, intent(out) :: vec

        call invoker(fill_element)

    contains

        subroutine fill_element(i, n)
            integer, intent(in) :: i
            integer, intent(in) :: n

            if (.not. allocated(vec)) allocate(vec(n))
            vec(i) = 'hi'
        end subroutine fill_element

    end subroutine outer

end module nested_vars_09_mod

program nested_vars_09
    use nested_vars_09_mod, only: outer
    implicit none

    character(len=5), dimension(:), allocatable :: v

    call outer(v)

    if (.not. allocated(v)) error stop
    if (size(v) /= 3) error stop
    if (v(1) /= 'hi') error stop
end program nested_vars_09
