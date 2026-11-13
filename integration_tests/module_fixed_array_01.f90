module module_fixed_array_01_m
    implicit none
    integer, save, target :: data(-1:0, 2:4) = reshape([11, 12, 21, 22, 31, 32], [2, 3])
end module

program module_fixed_array_01
    use module_fixed_array_01_m, only: data
    implicit none
    if (data(-1,2) /= 11 .or. data(0,2) /= 12) error stop 1
    if (data(-1,3) /= 21 .or. data(0,3) /= 22) error stop 2
    if (data(-1,4) /= 31 .or. data(0,4) /= 32) error stop 3
    data(0,2) = 42
    if (data(0,2) /= 42 .or. data(-1,3) /= 21) error stop 4
    data(-1,4) = 51
    if (data(-1,4) /= 51 .or. data(0,4) /= 32) error stop 5
end program
