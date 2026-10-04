module traits_optional_ordinary_arg_mismatch_01_m
    implicit none

    abstract interface :: ISetter
        subroutine set_value(value)
            integer, optional, intent(in) :: value
        end subroutine set_value
    end interface ISetter

    type :: Box
        integer :: value
    end type Box

    implements ISetter :: Box
        procedure, nopass :: set_value => box_set_value
    end implements Box

contains

    subroutine box_set_value(value)
        integer, intent(in) :: value
    end subroutine box_set_value
end module traits_optional_ordinary_arg_mismatch_01_m
