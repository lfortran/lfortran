program data_stmt_parameter_01
    implicit none
    type :: box
        real :: value
    end type

    type(box), parameter :: item = box(4.0)

    data item%value / 3.0 /
end program data_stmt_parameter_01
