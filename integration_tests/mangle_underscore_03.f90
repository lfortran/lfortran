module mangle_underscore_03_m
    implicit none
    interface
        integer function mu03_twice(n)
            integer, intent(in) :: n
        end function
    end interface
    ! A specific procedure with the name of its generic interface
    interface mu03_gen
        subroutine mu03_gen(k)
            integer, intent(inout) :: k
        end subroutine
        subroutine mu03_genr(x)
            real, intent(inout) :: x
        end subroutine
    end interface
    abstract interface
        integer function mu03_op(n)
            integer, intent(in) :: n
        end function
    end interface
contains
    subroutine drv(k)
        integer, intent(inout) :: k
        call mu03_add(k, 3)
        k = mu03_twice(k)
        call inner(k)
    contains
        subroutine inner(j)
            integer, intent(inout) :: j
            external :: mu03_add
            call mu03_add(j, 1)
        end subroutine
    end subroutine

    subroutine gen(k, x)
        integer, intent(inout) :: k
        real, intent(inout) :: x
        call mu03_gen(k)
        call mu03_gen(x)
    end subroutine

    integer function apply(f, n) result(r)
        procedure(mu03_op) :: f
        integer, intent(in) :: n
        r = f(n)
    end function
end module

program mangle_underscore_03
    use mangle_underscore_03_m, only: drv, gen, apply
    implicit none
    integer :: k
    real :: x
    integer, external :: mu03_twice
    k = 2
    call drv(k)
    print '(I0)', k
    if (k /= 11) error stop
    k = 1
    x = 1.0
    call gen(k, x)
    print '(I0, 1X, F4.1)', k, x
    if (k /= 2) error stop
    if (abs(x - 3.0) > 1e-6) error stop
    k = apply(mu03_twice, 4)
    print '(I0)', k
    if (k /= 8) error stop
end program
