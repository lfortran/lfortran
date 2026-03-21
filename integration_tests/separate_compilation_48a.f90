module mod_separate_compilation_48
    implicit none

    type :: mytype
        real, allocatable :: weights(:,:)
    end type mytype

    interface
        module function get_params(self) result(params)
            class(mytype), intent(in), target :: self
            real, allocatable :: params(:)
        end function get_params
    end interface

end module mod_separate_compilation_48
