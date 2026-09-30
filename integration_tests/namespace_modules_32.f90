! Derived types of a module whose default accessibility is PRIVATE, made
! public by an accessibility statement or by the PUBLIC attribute of the type
! definition, and a generic constructor named like its type, accessed
! through a module entity.
module namespace_modules_32_m
    implicit none
    private
    public :: t, make, pt
    type :: t
        integer :: i = 1
    end type
    type, public :: u
        integer :: j = 2
    end type
    type :: hidden
        integer :: k = 3
    end type
    type :: pt
        integer :: x = 0
    end type
    interface pt
        module procedure mk
    end interface
contains
    function make(i) result(r)
        integer, intent(in) :: i
        type(t) :: r
        type(hidden) :: h
        r%i = i + h%k
    end function

    function mk(i) result(p)
        integer, intent(in) :: i
        type(pt) :: p
        p%x = 10*i
    end function
end module

program namespace_modules_32
    use, namespace :: m => namespace_modules_32_m
    implicit none
    type(m%t) :: a
    type(m%u) :: b
    class(m%t), allocatable :: c
    type(m%pt) :: p, q
    a = m%make(4)
    allocate(m%t :: c)
    p = m%pt(3)
    q = m%pt(x=5)
    print *, a%i, b%j, c%i, p%x, q%x
    if (a%i /= 7) error stop
    if (b%j /= 2) error stop
    if (c%i /= 1) error stop
    if (p%x /= 30) error stop
    if (q%x /= 5) error stop
end program
