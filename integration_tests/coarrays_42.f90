program test_scalar_mix
    implicit none

    type :: MixedStruct
        integer(1) :: a
        integer(8) :: b
        real(4)    :: r
        complex(8) :: c
        logical    :: flag
    end type MixedStruct

    type(MixedStruct) :: item

    if (this_image() == 1) then
        item%a = 7_1
        item%b = 987654321_8
        item%r = 3.14_4
        item%c = (1.0_8, 2.0_8)
        item%flag = .true.
    else
        item%a = 0_1
        item%b = 0_8
        item%r = 0.0_4
        item%c = (0.0_8, 0.0_8)
        item%flag = .false.
    end if

    call co_broadcast(item, 1)

    sync all

    if (item%a /= 7_1) error stop
    if (item%b /= 987654321_8) error stop
    if (item%r /= 3.14_4) error stop
    if (item%c /= (1.0_8, 2.0_8)) error stop
    if (.not. item%flag) error stop

end program test_scalar_mix