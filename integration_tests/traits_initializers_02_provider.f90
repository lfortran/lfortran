module traits_initializers_02_provider_m
    implicit none
    private
    public :: Box
    type :: Box
        integer :: n = 0
    contains
        initial :: make
    end type
contains
    function make(value) result(object)
        integer, intent(in) :: value
        type(Box) :: object
        object%n = value + 10
    end function
end module
