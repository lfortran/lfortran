! gpu decline reason: LiveoutDerivedType
! A scalar component of a polymorphic dummy argument that one iteration
! defines: no thread can keep a private copy of a part of a polymorphic
! variable yet.
module gpu_decline_42_m
    implicit none
    type :: acc_t
        real :: v
        integer :: n
    end type
contains
    subroutine run(self, x, b)
        class(acc_t), intent(inout) :: self
        real, intent(in) :: x(:)
        real, intent(out) :: b(:)
        integer :: i
        do concurrent (i = 1:size(b))
            b(i) = x(i) * 2
            if (i == 3) self%n = 7
        end do
    end subroutine
end module

program gpu_decline_42
    use gpu_decline_42_m
    implicit none
    type(acc_t) :: a
    real :: x(100), b(100)
    x = 1
    a%n = 0
    call run(a, x, b)
    print *, a%n, b(5)
    if (a%n /= 7) error stop 1
    if (b(5) /= 2) error stop 2
end program
