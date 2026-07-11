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

    ! Assumed-length allocatable character array captured by nested procedure
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

    ! Non-constant expression length rewritten to deferred-length in nested_vars
    subroutine outer_varlen(nlen, vec)
        integer, intent(in) :: nlen
        character(len=nlen), dimension(:), allocatable, intent(out) :: vec

        call invoker(fill_element_varlen)

    contains

        subroutine fill_element_varlen(i, n)
            integer, intent(in) :: i
            integer, intent(in) :: n

            if (.not. allocated(vec)) allocate(vec(n))
            vec(i) = 'hi'
        end subroutine fill_element_varlen

    end subroutine outer_varlen

end module nested_vars_09_mod

program nested_vars_09
    use nested_vars_09_mod, only: outer, outer_varlen
    implicit none

    character(len=5), dimension(:), allocatable :: v
    character(len=5), dimension(:), allocatable :: w

    call outer(v)

    if (.not. allocated(v)) error stop
    if (size(v) /= 3) error stop
    if (len(v) /= 5) error stop
    if (len(v(1)) /= 5) error stop
    if (v(1) /= 'hi') error stop

    call outer_varlen(5, w)

    if (.not. allocated(w)) error stop
    if (size(w) /= 3) error stop
    if (len(w) /= 5) error stop
    if (len(w(1)) /= 5) error stop
    if (w(1) /= 'hi') error stop
end program nested_vars_09
