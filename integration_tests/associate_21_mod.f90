module associate_21_mod_a
    implicit none
    type :: dependency_t
        character(len=:), allocatable :: name
        integer :: key
    end type dependency_t
end module associate_21_mod_a


module associate_21_mod_b
    use associate_21_mod_a
    implicit none

    type :: model_t
        type(dependency_t), allocatable :: dependency(:)
    contains
        procedure :: update_dependency 
    end type model_t

contains

    subroutine update_dependency(self, ii, key)
        class(model_t), intent(inout) :: self
        integer, intent(in) :: ii
        integer, intent(in) :: key

        associate (dep => self%dependency(ii))
            dep%name = "LFortran"
            dep%key = key
        end associate
    end subroutine update_dependency

end module associate_21_mod_b