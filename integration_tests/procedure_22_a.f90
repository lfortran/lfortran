module procedure_22_mod_a
    implicit none

    type :: deps_t
        character(:), allocatable :: str
    contains
        procedure :: find_string => find_by_name
        generic :: find => find_string
    end type deps_t

contains
    integer function find_by_name(self, name)
        class(deps_t), intent(in) :: self
        character(len=*), intent(in) :: name
        find_by_name = len_trim(name)
    end function find_by_name

end module procedure_22_mod_a


module procedure_22_mod_b
    use procedure_22_mod_a, only: deps_t
contains 
    function func(model) result(id)
        class(deps_t), intent(in) :: model
        integer :: id
        id = model%find(model%str)
    end function
end module procedure_22_mod_b