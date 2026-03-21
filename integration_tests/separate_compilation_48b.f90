submodule(mod_separate_compilation_48) submod_separate_compilation_48
    implicit none
contains

    module function get_params(self) result(params)
        class(mytype), intent(in), target :: self
        real, allocatable :: params(:)
        real, pointer :: w_(:) => null()

        w_(1:product(shape(self%weights))) => self%weights
        params = w_
    end function get_params

end submodule submod_separate_compilation_48
