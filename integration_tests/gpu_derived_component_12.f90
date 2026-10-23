! A derived-type component of a derived-type variable that a `do
! concurrent` defines: a scalar part of it in every iteration, with the
! component then passed whole to a function, and the whole component in
! one iteration, of a variable with an array component the iterations
! define element by element. Each thread keeps a private copy of the
! component.
module gpu_derived_component_12_m
    implicit none
    type :: point
        real :: x, y
    end type
    type :: outer
        type(point) :: a
        integer :: k
    end type
    type :: inner_t
        integer :: k
        real :: x
    end type
    type :: big_t
        real :: a(1000)
        type(inner_t) :: inner
    end type
contains
    pure real function sumxy(p)
        type(point), intent(in) :: p
        sumxy = p%x + p%y
    end function

    subroutine s(y)
        type(big_t), intent(inout) :: y
        integer :: i
        do concurrent (i = 1:1000)
            y%a(i) = i
            if (i == 3) y%inner = inner_t(i, 2.0)
        end do
    end subroutine
end module

program gpu_derived_component_12
    use gpu_derived_component_12_m
    implicit none
    type(outer) :: o
    type(big_t) :: y
    real :: b(100)
    integer :: i
    o%a = point(0, 1)
    o%k = 0
    do concurrent (i = 1:100)
        o%a%x = i
        b(i) = sumxy(o%a)
    end do
    print *, b(7), b(100), o%k
    do i = 1, 100
        if (b(i) /= i + 1) error stop 1
    end do
    if (o%k /= 0 .or. o%a%y /= 1) error stop 4
    y%inner = inner_t(0, 0.0)
    call s(y)
    print *, y%a(7), y%inner%k, y%inner%x
    if (y%a(7) /= 7) error stop 2
    if (y%inner%k /= 3 .or. y%inner%x /= 2) error stop 3
end program
