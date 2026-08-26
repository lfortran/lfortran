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
        item%a = 7
        item%b = 987654321_8
        item%r = 3.14_4
        item%c = (1.0_8, 2.0_8)
        item%flag = .true.
    else
        item%a = 0
        item%b = 0
        item%r = 0.0
        item%c = (0.0_8, 0.0_8)
        item%flag = .false.
    end if
    
    call co_broadcast(item, 1)
    
    sync all
    print *, "Image", this_image(), "b =", item%b, "r =", item%r, "flag =", item%flag
end program test_scalar_mix