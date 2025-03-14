! Test pointer association
program derived_types_44
    type :: sds
        real, dimension(:), pointer :: f
    end type
    real, dimension(:), pointer :: x
    call sub_to_allocate(x)
    print *, "size(x): ", size(x)
    print *, "lbound(x): ", lbound(x)
    if (lbound(x,1) /= 1) error stop
    print *, "ubound(x): ", ubound(x)
    if ( abs(x(1) - 1.0) > 1e-8 ) error stop
    if ( abs(x(2) - 2.0) > 1e-8 ) error stop
    print *, "x in sub = ", x
    deallocate(x)
contains
    subroutine sub_to_allocate(x)
        real, dimension(:), pointer, intent(inout) :: x
        type(sds) :: s
        allocate(s%f(2))
        x => s%f
        x(1)=1.0
        x(2)=2.0
        print *, "x = ", x
    end subroutine
end program
