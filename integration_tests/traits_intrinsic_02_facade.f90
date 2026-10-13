module traits_intrinsic_02_facade_m
    ! Full USE is deliberate: this is the positive facade/re-export path.
    use traits_intrinsic_02_provider_m
    implicit none
contains

    function facade_read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = read_value{T}(x) + 1
    end function facade_read_value

end module traits_intrinsic_02_facade_m
