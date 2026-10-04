module traits_static_02_m
    implicit none

    abstract interface :: IShift
        function shift_value(delta, scale) result(res)
            integer, intent(in) :: delta
            integer, intent(in) :: scale
            integer :: res
        end function shift_value
    end interface IShift

    type :: OffsetBox
        integer :: value
    end type OffsetBox

    implements IShift :: OffsetBox
        procedure, pass :: shift_value => offset_shift_value
    end implements OffsetBox

contains

    function offset_shift_value(self, delta, scale) result(res)
        class(OffsetBox), intent(in) :: self
        integer, intent(in) :: delta
        integer, intent(in) :: scale
        integer :: res
        res = self%value + scale*delta
    end function offset_shift_value

    function adjust{IShift :: T}(x, delta, scale) result(res)
        type(T), intent(in) :: x
        integer, intent(in) :: delta
        integer, intent(in) :: scale
        integer :: res
        res = x%shift_value(delta, scale)
    end function adjust
end module traits_static_02_m

program traits_static_02
    use traits_static_02_m
    implicit none
    type(OffsetBox) :: box

    box = OffsetBox(10)

    if (adjust(box, 3, 2) /= 16) error stop
    if (adjust{OffsetBox}(box, delta=3, scale=2) /= 16) error stop
    if (adjust{OffsetBox}(box, 3, 2) /= 16) error stop
end program traits_static_02
