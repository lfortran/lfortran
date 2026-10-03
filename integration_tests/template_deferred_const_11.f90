! Inline instantiation (`call s{...}()`, `f{...}()`) with constant
! expressions for deferred constants: literals, expressions of named
! constants, keyword arguments, local named constants of an internal
! procedure and of a template procedure (#13411).

module template_deferred_const_11_m
    implicit none
contains
    template subroutine set_n{n}(r)
        deferred integer, parameter :: n
        integer, intent(out) :: r
        r = n
    end subroutine

    template function get_n{n}() result(r)
        deferred integer, parameter :: n
        integer :: r
        r = n
    end function

    template subroutine set_n_nested{n}(r)
        deferred integer, parameter :: n
        integer, intent(out) :: r
        r = n
    end subroutine

    template subroutine set_k_plus_1{m}(r)
        deferred integer, parameter :: m
        integer, intent(out) :: r
        integer, parameter :: k = 3
        call set_n_nested{k + 1}(r)
    end subroutine
end module

program template_deferred_const_11
    use template_deferred_const_11_m
    implicit none
    integer, parameter :: two = 2
    integer :: r

    call set_n{3}(r)
    if (r /= 3) error stop 1
    call set_n{2 * two + 1}(r)
    if (r /= 5) error stop 2
    call set_n{n = two - 1}(r)
    if (r /= 1) error stop 3
    if (get_n{7}() /= 7) error stop 4
    if (get_n{n = two + 1}() /= 3) error stop 5
    call set_k_plus_1{5}(r)
    if (r /= 4) error stop 6
    call local_constants()
    print *, "ok"
contains
    subroutine local_constants()
        integer, parameter :: k = 6
        call set_n{k + 1}(r)
        if (r /= 7) error stop 7
        if (get_n{k * two}() /= 12) error stop 8
    end subroutine
end program
