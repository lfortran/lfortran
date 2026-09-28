! Namespaces and submodules: a namespace imported in a module is
! accessible in its submodules by host association, and a submodule can
! import its own module entities.
module namespace_modules_13_helper
    implicit none
    integer, parameter :: offset = 100
contains
    integer function triple(i)
        integer, intent(in) :: i
        triple = 3*i
    end function
end module

module namespace_modules_13_other
    implicit none
    integer, parameter :: offset = 1000
end module

module namespace_modules_13_api
    use, namespace :: h => namespace_modules_13_helper
    implicit none
    interface
        module integer function compute(i)
            integer, intent(in) :: i
        end function

        module integer function compute_other(i)
            integer, intent(in) :: i
        end function
    end interface
end module

submodule (namespace_modules_13_api) namespace_modules_13_impl
    use, namespace :: o => namespace_modules_13_other
    implicit none
contains
    module integer function compute(i)
        integer, intent(in) :: i
        compute = h%triple(i) + h%offset
    end function

    module integer function compute_other(i)
        integer, intent(in) :: i
        compute_other = i + o%offset + h%offset
    end function
end submodule

program namespace_modules_13
    use namespace_modules_13_api, only: compute, compute_other
    implicit none
    if (compute(2) /= 106) error stop
    if (compute_other(1) /= 1101) error stop
    print *, compute(2), compute_other(1)
end program
