! Issue: https://github.com/lfortran/lfortran/issues/8825
! select type with parametrized derived types (kind parameters)
program select_type_53
    implicit none

    type :: parent
    end type parent

    type, extends(parent) :: child_int(k)
        integer, kind :: k = 4
        integer(k) :: a
    end type child_int

    type, extends(parent) :: child_real(k)
        integer, kind :: k = 8
        real(k) :: a
    end type child_real

    type(child_int(4)) :: i
    type(child_real(8)) :: r
    class(parent), allocatable :: p

    i%a = 42
    r%a = 3.141592653589793_8

    allocate(p, source=i)
    call check_int(p)
    deallocate(p)

    allocate(p, source=r)
    call check_real(p)
    deallocate(p)

    ! Associate-name form
    call try_select(i)
    call try_select(r)

    print *, "ok"
contains
    subroutine check_int(par)
        class(parent), intent(in) :: par
        select type (par)
        type is (child_int(4))
            if (par%a /= 42) error stop 1
        class default
            error stop 2
        end select
    end subroutine check_int

    subroutine check_real(par)
        class(parent), intent(in) :: par
        select type (par)
        type is (child_real(8))
            if (abs(par%a - 3.141592653589793_8) > 1.0e-12_8) error stop 3
        class default
            error stop 4
        end select
    end subroutine check_real

    subroutine try_select(par)
        class(parent), intent(in) :: par
        select type (q => par)
        type is (child_int(4))
            if (q%a /= 42) error stop 5
        type is (child_real(8))
            if (abs(q%a - 3.141592653589793_8) > 1.0e-12_8) error stop 6
        class default
            error stop 7
        end select
    end subroutine try_select
end program select_type_53
