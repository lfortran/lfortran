module contiguous_dummy_03_m
    implicit none
    type :: t
        real :: sum_of_data = 0.0
    contains
        procedure :: add
        procedure :: add_and_modify
        generic :: accumulate => add
    end type
contains
    subroutine add(this, a)
        class(t), intent(inout) :: this
        real, contiguous, intent(in) :: a(:)
        call add_raw(this, a, size(a))
    end subroutine

    subroutine add_and_modify(this, a)
        class(t), intent(inout) :: this
        real, contiguous, intent(inout) :: a(:)
        call add_raw(this, a, size(a))
        a = -a
    end subroutine

    subroutine add_raw(this, p, n)
        class(t), intent(inout) :: this
        real, intent(in) :: p(*)
        integer, intent(in) :: n
        integer :: i
        do i = 1, n
            this%sum_of_data = this%sum_of_data + p(i)*10.0**(i-1)
        end do
    end subroutine
end module

program contiguous_dummy_03
    use contiguous_dummy_03_m
    implicit none
    type(t) :: f
    real :: v(2, 3)
    v = reshape([1, 2, 3, 4, 5, 6], shape(v))
    call f%add(v(1, :))
    if (abs(f%sum_of_data - 531.0) > 1e-3) error stop 1
    f%sum_of_data = 0.0
    call f%accumulate(v(2, :))
    if (abs(f%sum_of_data - 642.0) > 1e-3) error stop 2
    call f%add_and_modify(v(1, :))
    if (any(v(1, :) /= [-1.0, -3.0, -5.0])) error stop 3
    if (any(v(2, :) /= [2.0, 4.0, 6.0])) error stop 4
end program
