! same_type_as and extends_type_of on objects of the same instantiation of a
! parameterized derived type with a length type parameter
program pdt_28
    implicit none
    type :: t(n)
        integer, len :: n
        integer :: a(n)
    end type t
    type(t(3)) :: x
    type(t(3)), allocatable :: y
    type(t(3)), pointer :: z => null()
    allocate(y)
    if (.not. same_type_as(x, y)) error stop 1
    if (.not. extends_type_of(x, y)) error stop 2
    if (.not. same_type_as(z, x)) error stop 3
    if (.not. extends_type_of(z, y)) error stop 4
    print *, "ok"
end program pdt_28
