module arrayitem_module_01
    type :: block_1
        integer :: a, c(5)
    end type
    type(block_1) :: struct_instance_block_1
end module

program arrayitem_01
    use arrayitem_module_01
    struct_instance_block_1%c(1) = 19
    if (struct_instance_block_1%c(1) /= 19) error stop
end program