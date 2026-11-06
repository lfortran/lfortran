! Components of a derived type declared in a procedure with an assumed-shape
! dummy (which pass_array_by_data specialises) and used in its contained
! procedures keep their mangled names (#16091)
subroutine mangle_underscore_06_outer(x)
    implicit none
    real, intent(in) :: x(:)
    type v
        integer :: k = 5
    end type
    type(v) :: d
    call inner()
    if (d%k /= 3) error stop
contains
    subroutine inner()
        d%k = int(sum(x))
        d = v(d%k)
    end subroutine
end subroutine

subroutine mangle_underscore_06_outer_spec(x)
    implicit none
    real, intent(in) :: x(:)
    type v
        integer :: k = 5
    end type
    call inner_spec(x)
contains
    subroutine inner_spec(y)
        real, intent(in) :: y(:)
        type(v) :: d
        if (d%k /= 5) error stop
        d = v(int(y(1)) + 10)
        if (d%k /= 11) error stop
    end subroutine
end subroutine

program mangle_underscore_06
    implicit none
    interface
        subroutine mangle_underscore_06_outer(x)
            real, intent(in) :: x(:)
        end subroutine
        subroutine mangle_underscore_06_outer_spec(x)
            real, intent(in) :: x(:)
        end subroutine
    end interface
    call mangle_underscore_06_outer([1.0, 2.0])
    call mangle_underscore_06_outer_spec([1.0, 2.0])
    print *, "ok"
end program
