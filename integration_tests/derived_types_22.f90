module derived_types_22_file_common_block_sample
    type :: sample
        real :: A, B, E
    end type
    type(sample) :: struct_instance_sample
end module

subroutine pass()
    use derived_types_22_file_common_block_sample
    print *, struct_instance_sample%A, struct_instance_sample%B
    if (abs(struct_instance_sample%A - 10.0) > 1.0e-7)  error stop
    if (abs(struct_instance_sample%B - 20.0) > 1.0e-7)  error stop
end subroutine

program derived_types_22
    use derived_types_22_file_common_block_sample
    struct_instance_sample%A = 10.0
    struct_instance_sample%B = 20.0
    call pass()
end program
