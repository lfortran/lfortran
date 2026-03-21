submodule(mod_separate_compilation_49) submod_separate_compilation_49
    implicit none
contains

    ! Case 1: 1D pointer => null() — mirrors nf_embedding get_params
    module function get_params_1d(self) result(params)
        class(mytype), intent(in), target :: self
        real, allocatable :: params(:)
        real, pointer :: w_(:) => null()

        w_(1:product(shape(self%weights))) => self%weights
        params = w_
    end function get_params_1d

    ! Case 2: second 1D pointer => null() — mirrors nf_embedding get_gradients
    module function get_gradients_1d(self) result(grads)
        class(mytype), intent(in), target :: self
        real, allocatable :: grads(:)
        real, pointer :: dw_(:) => null()

        dw_(1:product(shape(self%dw))) => self%dw
        grads = dw_
    end function get_gradients_1d

    ! Case 3: 2D pointer => null() — mirrors nf_embedding set_params
    module subroutine set_params_2d(self, params)
        class(mytype), intent(inout) :: self
        real, intent(in), target :: params(:)
        real, pointer :: p_(:,:) => null()
        integer :: rows, cols

        rows = size(self%weights, 1)
        cols = size(self%weights, 2)
        p_(1:rows, 1:cols) => params
        self%weights = p_
    end subroutine set_params_2d

    ! Case 4: multiple pointers in same function — mirrors nf_linear2d
    module function get_all(self) result(all_params)
        class(mytype), intent(in), target :: self
        real, allocatable :: all_params(:)
        real, pointer :: w_(:) => null()
        real, pointer :: dw_(:) => null()

        w_(1:product(shape(self%weights))) => self%weights
        dw_(1:product(shape(self%dw))) => self%dw
        all_params = [w_, dw_]
    end function get_all

end submodule submod_separate_compilation_49
