module traits_runtime_component_04_facade_m
    use traits_runtime_component_04_provider_m, only: Box => Holder
    implicit none
    private
    public :: Box, read_box
contains
    pure integer function read_box(object)
        type(Box), intent(in) :: object
        read_box = object%item%value()
    end function
end module
