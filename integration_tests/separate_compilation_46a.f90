module separate_compilation_46a_types
    implicit none
    type :: obj_t
        integer :: val = 0
    contains
        final :: obj_destroy
    end type obj_t
contains
    subroutine obj_destroy(self)
        type(obj_t), intent(inout) :: self
        self%val = -1
    end subroutine obj_destroy
end module separate_compilation_46a_types

module separate_compilation_46a_global
    use separate_compilation_46a_types, only: obj_t
    implicit none
    class(obj_t), allocatable, save :: global_obj
end module separate_compilation_46a_global
