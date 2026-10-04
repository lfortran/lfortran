module traits_missing_type_inference_01_m
    implicit none

    abstract interface :: IValue
        subroutine fill_value(out)
            integer, intent(out) :: out
        end subroutine fill_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, nopass :: fill_value => box_fill_value
    end implements Box

contains

    subroutine box_fill_value(out)
        integer, intent(out) :: out
        out = 17
    end subroutine box_fill_value

    function make_value{IValue :: T}() result(out)
        integer :: out
        out = 17
    end function make_value
end module traits_missing_type_inference_01_m

program traits_missing_type_inference_01
    use traits_missing_type_inference_01_m
    implicit none
    integer :: value
    value = make_value()
end program traits_missing_type_inference_01
