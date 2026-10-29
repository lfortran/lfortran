module block_entry_m
    implicit none
    type :: accumulator
        real :: total = 0
    contains
        procedure :: automatic_block, saved_block, observer_block
    end type
contains
    subroutine automatic_block(self, x)
        class(accumulator), intent(inout) :: self
        real, intent(in) :: x(:)
        block
            real :: work(size(x))
            block
                work = 2*x
                self%total = sum(work)
            end block
        end block
    end subroutine

    subroutine saved_block(self, x)
        class(accumulator), intent(inout) :: self
        real, intent(in) :: x(:)
        block
            integer, save :: count = 0
            count = count + 1
            self%total = count + sum(x)
        end block
    end subroutine

    subroutine observer_block(self, x)
        class(accumulator), intent(inout) :: self
        real, intent(in) :: x(:)
        block
            if (is_contiguous(x)) then
                self%total = 1
            else
                self%total = -1
            end if
        end block
    end subroutine
end module

program block_entry_test
    use block_entry_m
    implicit none
    type(accumulator), target :: storage
    class(accumulator), pointer :: object
    real :: values(6) = [1., 2., 3., 4., 5., 6.]
    object => storage
    call object%automatic_block(values)
    if (object%total /= 42.) error stop 1
    call object%automatic_block(values(6:1:-2))
    if (object%total /= 24.) error stop 2
    call object%automatic_block(values(1:0))
    if (object%total /= 0.) error stop 3
    call object%observer_block(values(::2))
    if (object%total /= -1.) error stop 4
    call object%observer_block(values)
    if (object%total /= 1.) error stop 5
    call object%saved_block(values)
    if (object%total /= 22.) error stop 6
    call object%saved_block(values)
    if (object%total /= 23.) error stop 7
end program
