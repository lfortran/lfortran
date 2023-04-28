
module derived_types_21
    implicit none

    private

    type build_target_ptr

        type(build_target_t), pointer :: ptr

    end type build_target_ptr


    !> Type describing a generated build target
    type build_target_t

        !> File path of build target object relative to cwd
        character(:), allocatable :: output_file

        !> Resolved build dependencies
        type(build_target_ptr), allocatable :: dependencies(:)

        !> Target type
        integer :: target_type = 0

    end type build_target_t


    contains

    integer function check1()
        implicit none

        type(build_target_t) :: package

        type(build_target_ptr), allocatable :: targets(:)

        check1 = 10
    end function

end module derived_types_21

program derived_types_21_program
    use derived_types_21
    implicit none
    integer :: z
    z = check1()
    if (z /= 10) error stop
end program derived_types_21_program
