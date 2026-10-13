module traits_initializers_02_oracle_provider_m
    implicit none
    private
    public :: Box
    type :: Box
        integer :: n = 0
    end type
    interface Box
        module procedure make
    end interface
contains
    function make(value) result(object)
        integer, intent(in) :: value
        type(Box) :: object
        object%n = value + 10
    end function
end module

module traits_initializers_02_oracle_facade_m
    use traits_initializers_02_oracle_provider_m, only: Renamed => Box
    implicit none
    private
    public :: Renamed
end module

program traits_initializers_02_oracle
    use traits_initializers_02_oracle_facade_m, only: Item => Renamed
    implicit none
    type(Item) :: x, y, z
    x = Item(value=5)
    y = Item(n=7)
    z = Item()
    if (x%n /= 15) error stop 1
    if (y%n /= 7) error stop 2
    if (z%n /= 0) error stop 3
    x = Item(6)
    if (x%n /= 16) error stop 4
end program
