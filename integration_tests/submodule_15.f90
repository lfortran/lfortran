! Test for https://github.com/lfortran/lfortran/issues/8857
! Submodule function with a result clause where result name differs from function name
module submodule_15_interface
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    interface
        real(dp) module function pi() result(p)
        end function pi
    end interface
end module submodule_15_interface

submodule (submodule_15_interface) submodule_15_impl
    implicit none
contains
    real(dp) module function pi() result(p)
        p = acos(-1.0_dp)
    end function pi
end submodule submodule_15_impl

program submodule_15
    use submodule_15_interface
    implicit none
    real(dp) :: val
    val = pi()
    if (abs(val - 3.14159265358979323846_dp) > 1.0e-10_dp) error stop
    print *, "pi is", val
end program submodule_15
