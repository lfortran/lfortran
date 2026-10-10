module traits_component_loaded_pure_client_02
    use traits_component_loaded_pure_lib
    implicit none
contains
    ! The loaded procedure calls a dummy procedure whose effects are unknown.
    pure subroutine assign_through_loaded_dummy(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call apply(overwrite, x, y)
    end subroutine
end module
