! gpu decline reason: LiveoutDerivedType
! An array component of a derived-type dummy argument that every iteration
! assigns whole and then reads, a temporary of the iteration, that is too
! large for each thread to keep a private copy of.
module gpu_decline_40_m
    implicit none
    type :: big_t
        real :: t(2000)
    end type
contains
    subroutine s(w, x, b)
        type(big_t), intent(inout) :: w
        real, intent(in) :: x(:)
        real, intent(out) :: b(:)
        integer :: i
        do concurrent (i = 1:size(b))
            w%t = x(i)
            b(i) = w%t(1) + w%t(2000)
        end do
    end subroutine
end module

program gpu_decline_40
    use gpu_decline_40_m
    implicit none
    type(big_t) :: w
    real :: x(100), b(100)
    integer :: i
    do i = 1, 100
        x(i) = i
    end do
    call s(w, x, b)
    print *, b(7)
    if (b(7) /= 14) error stop 1
end program
