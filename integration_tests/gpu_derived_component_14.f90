! A local derived-type variable, not read after a `do concurrent`, of
! which every iteration defines one component and then reads another
! that keeps the value it had before the loop: the other components are
! not temporaries of the iteration, so each thread keeps a private copy of
! only the component it defines, and reads the others from the variable.
module gpu_derived_component_14_m
    implicit none
    type :: point
        real :: x, y
    end type
    type :: outer
        type(point) :: a
        integer :: k
    end type
contains
    pure real function sumxy(p)
        type(point), intent(in) :: p
        sumxy = p%x + p%y
    end function
end module

program gpu_derived_component_14
    use gpu_derived_component_14_m
    implicit none
    type(outer) :: o
    type(point) :: q, r
    real :: b(100), c(100), d(100)
    integer :: i
    o%a = point(0, 1)
    o%k = 0
    q = point(0, 5)
    do concurrent (i = 1:100)
        o%a%x = i
        b(i) = sumxy(o%a)
        q%x = 2 * i
        c(i) = q%x + q%y
        r = point(i, -i)
        d(i) = r%x - r%y
    end do
    print *, b(7), c(7), d(7)
    do i = 1, 100
        if (b(i) /= i + 1) error stop 1
        if (c(i) /= 2 * i + 5) error stop 2
        if (d(i) /= 2 * i) error stop 3
    end do
end program
