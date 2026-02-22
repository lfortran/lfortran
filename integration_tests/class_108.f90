program class_108
    implicit none

    type ring_t
    end type

    type(ring_t) :: ring
    integer :: i
    logical :: b

    do i = 1, 2000000
        b = test(ring)
        if (b) error stop 1
    end do

    print *, 'done'

contains

    logical function test(this)
        class(ring_t), intent(inout) :: this
        test = .false.
    end function test

end program
