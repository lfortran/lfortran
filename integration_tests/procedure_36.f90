module procedure_36_mod
    implicit none

    abstract interface
        real function reduction_sub_dp() result(r)
        end function
    end interface

    type, public :: linop_dp_type
        procedure(reduction_sub_dp), nopass, pointer :: inner_product => default_dot_dp
    end type

contains

    real function default_dot_dp() result(r)
        r = 0.0
    end function

end module procedure_36_mod

program procedure_36
    use procedure_36_mod
    implicit none
    type(linop_dp_type) :: op
    if (abs(op%inner_product() - 0.0) > 1e-6) error stop
end program procedure_36
