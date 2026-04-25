module math_submodule_46
    implicit none
    integer, parameter :: sp = 4

    interface linspace
        pure module function linspace_sp(start, end, n) result(res)
            real(sp), intent(in) :: start, end
            integer, intent(in) :: n
            real(sp) :: res(max(n, 0))
        end function
    end interface

end module

submodule (math_submodule_46) linspace_submod_46
    implicit none
contains

    module procedure linspace_sp
        real(sp) :: x(max(n, 0))
        integer :: i

        if (n <= 0) return
        if (n == 1) then
            res(1) = end
            return
        end if

        do i = 1, n
            x(i) = start + (end - start) * real(i - 1, sp) / real(n - 1, sp)
        end do
        res = x
    end procedure

end submodule

program submodule_46
    use math_submodule_46
    implicit none

    real(4) :: result(5)

    result = linspace(0.0, 1.0, 5)

    if (abs(result(1) - 0.0) > 1.0e-6) error stop
    if (abs(result(5) - 1.0) > 1.0e-6) error stop
    if (abs(result(3) - 0.5) > 1.0e-6) error stop
    print *, result
end program
