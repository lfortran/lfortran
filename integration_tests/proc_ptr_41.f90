! Calls, direct and through procedure pointers, of procedures whose
! explicit-shape dummy has a use-associated extent, from a program and from
! a module that reaches the interface only through another module, with
! assumed-shape and explicit-shape actual arguments
module proc_ptr_41_sizes
    implicit none
    integer :: n = 3
end module

module proc_ptr_41_iface
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_41_sizes, only: n
            real, intent(inout) :: a(n)
        end subroutine
    end interface
contains
    subroutine g(a)
        use proc_ptr_41_sizes, only: n
        real, intent(inout) :: a(n)
        a = a + 1
    end subroutine
end module

module proc_ptr_41_types
    use proc_ptr_41_iface, only: cb
    implicit none
    type :: holder
        procedure(cb), pointer, nopass :: f => null()
    end type
    procedure(cb), pointer :: gp => null()
end module

module proc_ptr_41_user
    use proc_ptr_41_types, only: holder, gp
    implicit none
contains
    subroutine run(x)
        real, intent(inout) :: x(:)
        type(holder) :: h
        h%f => gp
        call gp(x)
        call h%f(x)
    end subroutine

    subroutine run_explicit(x)
        real, intent(inout) :: x(3)
        type(holder) :: h
        h%f => gp
        call gp(x)
        call h%f(x)
    end subroutine
end module

program proc_ptr_41
    use proc_ptr_41_iface, only: cb, g
    use proc_ptr_41_types, only: gp
    use proc_ptr_41_user, only: run, run_explicit
    implicit none
    procedure(cb), pointer :: q
    real :: x(3)
    x = 0
    q => g
    call q(x)
    call g(x)
    if (any(x /= 2)) error stop
    gp => g
    call run(x)
    if (any(x /= 4)) error stop
    call run_explicit(x)
    if (any(x /= 6)) error stop
    print *, x
end program
