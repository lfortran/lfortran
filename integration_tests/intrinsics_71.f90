program intrinsics_71
    implicit none
    integer :: result_value
    result_value = routine(3)

    print *, result_value
    if (result_value /= 600) error stop

contains

    function routine(p) result(r)
        integer, intent(in) :: p
        integer :: a(1, p)
        integer :: b(p, 1)
        integer :: c(1, 1)
        integer :: r

        a(1, 1) = 10
        a(1, 2) = 10
        a(1, 3) = 10

        b(1, 1) = 20
        b(2, 1) = 20
        b(3, 1) = 20

        c = matmul(a, b)
        r = c(1, 1)
    end function routine
end program
