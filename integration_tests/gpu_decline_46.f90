! gpu decline reason: LiveoutDerivedType
! A derived-type dummy argument with an allocatable component that every
! iteration assigns whole: its value is kept after the loop, and no thread
! can keep a private copy of a value with an allocatable component.
module gpu_decline_46_m
    implicit none
    type :: a_t
        real, allocatable :: w(:)
        real :: c(3)
        real :: k
    end type
contains
    subroutine run(x, y, g, g0)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        type(a_t), intent(inout) :: g
        type(a_t), intent(in) :: g0
        integer :: i
        do concurrent (i = 1:size(x))
            g = g0
            y(i) = g%k + x(i)
        end do
    end subroutine
end module
program gpu_decline_46
    use gpu_decline_46_m
    implicit none
    real :: x(100), y(100)
    type(a_t) :: g, g0
    integer :: i
    do i = 1, 100
        x(i) = i
    end do
    g0%k = 1; g0%c = 2
    allocate(g0%w(3), g%w(3)); g0%w = 0; g%w = 0
    g%k = 0; g%c = 0
    call run(x, y, g, g0)
    print *, y(7)
    if (y(7) /= 8) error stop 1
end program
