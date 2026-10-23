! gpu decline reason: LiveoutDerivedType
! A derived-type component of a dummy argument, of which every iteration
! defines a scalar component and then passes the component whole to a
! function: each thread would keep a private copy of the component, which
! is too large.
module gpu_decline_41_m
    implicit none
    type :: inner_t
        real :: a(2000)
        integer :: k
    end type
    type :: outer_t
        type(inner_t) :: in
    end type
contains
    pure real function f(p)
        type(inner_t), intent(in) :: p
        f = p%a(1) + p%k
    end function

    subroutine s(y, b)
        type(outer_t), intent(inout) :: y
        real, intent(out) :: b(:)
        integer :: i
        do concurrent (i = 1:size(b))
            y%in%k = i
            b(i) = f(y%in)
        end do
    end subroutine
end module

program gpu_decline_41
    use gpu_decline_41_m
    implicit none
    type(outer_t) :: y
    real :: b(100)
    y%in%a = 1
    call s(y, b)
    print *, b(7)
    if (b(7) /= 8) error stop 1
end program
