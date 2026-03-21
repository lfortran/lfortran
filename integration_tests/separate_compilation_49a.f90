module mod_separate_compilation_49
    implicit none

    type :: mytype
        real, allocatable :: weights(:,:)
        real, allocatable :: dw(:,:)
        real, allocatable :: bias(:)
    end type mytype

    interface
        ! Case 1: 1D pointer => null() in a function
        module function get_params_1d(self) result(params)
            class(mytype), intent(in), target :: self
            real, allocatable :: params(:)
        end function get_params_1d

        ! Case 2: second 1D pointer => null() in a different function
        module function get_gradients_1d(self) result(grads)
            class(mytype), intent(in), target :: self
            real, allocatable :: grads(:)
        end function get_gradients_1d

        ! Case 3: 2D pointer => null() in a function
        module subroutine set_params_2d(self, params)
            class(mytype), intent(inout) :: self
            real, intent(in), target :: params(:)
        end subroutine set_params_2d

        ! Case 4: multiple pointers in the same function
        module function get_all(self) result(all_params)
            class(mytype), intent(in), target :: self
            real, allocatable :: all_params(:)
        end function get_all
    end interface

end module mod_separate_compilation_49
