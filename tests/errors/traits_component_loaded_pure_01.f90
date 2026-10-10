module traits_component_loaded_pure_client_01
    use traits_component_loaded_pure_lib
    implicit none
contains
    ! The loaded procedure reaches the effects through a later procedure.
    pure subroutine assign_loaded(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call middle(x, y)
    end subroutine
end module
