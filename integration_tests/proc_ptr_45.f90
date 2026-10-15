! A procedure pointer component whose interface has an explicit-shape dummy
! with a use-associated extent and an optional dummy (#14514)
module proc_ptr_45_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_45_m
    implicit none
    abstract interface
        subroutine cb(a, k)
            use proc_ptr_45_sizes, only: n
            real, intent(inout) :: a(n)
            integer, intent(in), optional :: k
        end subroutine
    end interface
    type :: t
        procedure(cb), pointer, nopass :: f => null()
    end type
contains
    subroutine fa(a, k)
        use proc_ptr_45_sizes, only: n
        real, intent(inout) :: a(n)
        integer, intent(in), optional :: k
        if (present(k)) then
            a = real(k)
        else
            a = 2.0
        end if
    end subroutine
end module

program proc_ptr_45
    use proc_ptr_45_m
    implicit none
    type(t) :: x
    real :: z(2)
    x%f => fa
    call x%f(z)
    if (any(z /= 2.0)) error stop
    call x%f(z, 5)
    if (any(z /= 5.0)) error stop
    print *, z
end program
