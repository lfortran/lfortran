module separate_compilation_69a_base
implicit none
type :: base_t
contains
    procedure :: get
    generic :: value => get
end type
contains
    integer function get(self)
        class(base_t), intent(in) :: self
        get = 42
    end function
end module

module separate_compilation_69a
use separate_compilation_69a_base, only: base_t
implicit none
type, extends(base_t) :: child_t
end type
end module
