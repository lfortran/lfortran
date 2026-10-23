! A type-bound subroutine called in a `do concurrent` through a derived-type
! variable that it is passed as other than its first argument
! (PASS(self)), and that it defines in one iteration: each thread saves,
! compares and puts back the parts of its copy once after the call.
module gpu_liveout_15_m
    implicit none
    type :: acc_t
        real :: c
        integer :: n
    contains
        procedure, pass(self) :: upd
    end type
contains
    pure subroutine upd(i, self, y)
        integer, intent(in) :: i
        class(acc_t), intent(inout) :: self
        real, intent(out) :: y
        y = self%c * i
        if (i == 3) self%n = 7
    end subroutine
end module

program gpu_liveout_15
    use gpu_liveout_15_m
    implicit none
    type(acc_t) :: m
    real :: y(100)
    integer :: i
    m%c = 2
    m%n = 0
    do concurrent (i = 1:100)
        call m%upd(i, y(i))
    end do
    print *, m%n, y(7)
    if (m%n /= 7 .or. y(7) /= 14) error stop 1
end program
