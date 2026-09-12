program test_array_struct
    implicit none

    type :: MatrixData
        integer :: id
        real(8) :: coords(3)
        integer :: matrix(2, 2)
    end type MatrixData

    type(MatrixData) :: md

    if (this_image() == 1) then
        md%id = 42
        md%coords = [1.1_8, 2.2_8, 3.3_8]
        md%matrix = reshape([1, 2, 3, 4], [2, 2])
    else
        md%id = 0
        md%coords = 0.0_8
        md%matrix = 0
    end if

    call co_broadcast(md, 1)

    sync all

    if (md%id /= 42) error stop
    if (md%coords(1) /= 1.1_8) error stop
    if (md%coords(2) /= 2.2_8) error stop
    if (md%coords(3) /= 3.3_8) error stop
    if (md%matrix(1, 1) /= 1) error stop
    if (md%matrix(2, 1) /= 2) error stop
    if (md%matrix(1, 2) /= 3) error stop
    if (md%matrix(2, 2) /= 4) error stop

end program test_array_struct