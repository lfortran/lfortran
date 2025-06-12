module arrays_91_mod

contains

    function matprod21(x, y) result(z)
        implicit none
        integer, intent(in) :: x(:, :)
        integer, intent(in) :: y(:)
        integer, allocatable :: z(:)
        allocate(z(size(x, 1)))
        z = matmul(x, y)
    end function matprod21

    function matprod22(x, y) result(z)
        implicit none
        integer, intent(in) :: x(:, :)
        integer, intent(in) :: y(:, :)
        integer, allocatable :: z(:, :)
        allocate(z(size(x, 1), size(y, 2)))
        z = matmul(x, y)
    end function matprod22

    function matprod12(x, y) result(z)
        implicit none
        integer, intent(in) :: x(:)
        integer, intent(in) :: y(:, :)
        integer, allocatable :: z(:)
        allocate(z(size(y, 2)))
        z = matmul(x, y)
    end function matprod12
end module

program arrays_91
    use arrays_91_mod
    implicit none
    integer :: x_1(2), x_2(2, 3), y_1(3), y_2(3, 4), z_1(2), z_2(2, 4), y_2_(2, 3), z_1_(3)
    x_2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    y_1 = [1, 2, 3]
    z_1 = matprod21(x_2, y_1)
    print *, "z_1: ", z_1
    if (any(z_1 /= [22, 28])) error stop

    y_2 = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4])
    z_2 = matprod22(x_2, y_2)
    print *, "z_2: ", z_2
    if (any(z_2 /= reshape([22, 28, 49, 64, 76, 100, 103, 136], [2, 4]))) error stop

    x_1 = [1, 2]
    y_2_ = reshape([4, 5, 6, 7, 8, 9], [2, 3])
    z_1_ = matprod12(x_1, y_2_)
    print *, "z_1_: ", z_1_
    if (any(z_1_ /= [14, 20, 26])) error stop
end program
