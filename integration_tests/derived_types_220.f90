! Rank 2 and rank 3 array components with a scalar default initializer,
! declared in a module, and in a nested derived type (#14211).
module derived_types_220_m
    implicit none
    type :: inner
        integer :: x(2,2) = 3
        real :: r(2,2,2) = 2.5
        character(len=2) :: c(2,2) = "q"
        logical :: l(2,2) = .true.
    end type inner

    type :: outer
        integer :: n = 1
        type(inner) :: in = inner()
        integer :: m(3,2) = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    end type outer

    type :: holder
        integer :: x(2,2) = 3
        real :: r(2,2,2) = 2.5
    end type holder

    type(holder) :: module_var
contains
    integer function total(o) result(s)
        type(outer), intent(in) :: o
        s = sum(o%in%x) + sum(o%m)
    end function total
end module derived_types_220_m

program derived_types_220
    use derived_types_220_m, only: inner, outer, module_var, total
    implicit none
    type(inner) :: a
    type(outer) :: o, p

    if (any(module_var%x /= 3)) error stop
    if (any(module_var%r /= 2.5)) error stop

    if (any(a%x /= 3) .or. any(shape(a%x) /= [2, 2])) error stop
    if (any(a%r /= 2.5) .or. any(shape(a%r) /= [2, 2, 2])) error stop
    if (any(a%c /= "q ")) error stop
    if (.not. all(a%l)) error stop

    a = inner(r=1.0)
    if (any(a%x /= 3)) error stop
    if (any(a%r /= 1.0)) error stop
    if (any(a%c /= "q")) error stop

    if (o%n /= 1 .or. any(o%in%x /= 3) .or. o%m(3, 2) /= 6) error stop
    print *, total(o)
    if (total(o) /= 33) error stop

    p = outer(n=2)
    if (any(p%in%x /= 3) .or. any(p%in%r /= 2.5)) error stop
    if (p%m(2, 1) /= 2 .or. p%m(1, 2) /= 4) error stop

    p = outer(in=inner(x=5))
    if (p%n /= 1 .or. any(p%in%x /= 5) .or. any(p%in%r /= 2.5)) error stop
    if (.not. all(p%in%l)) error stop
end program derived_types_220
