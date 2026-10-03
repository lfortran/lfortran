module separate_compilation_33a
    implicit none

    type tensor_1d_t
    contains
        procedure :: divergence_1d_weights
    end type tensor_1d_t

    type, extends(tensor_1d_t) :: divergence_1d_t
    contains
        generic :: weights => divergence_1d_weights
    end type divergence_1d_t

contains

    integer function divergence_1d_weights(self) result(weights)
        class(tensor_1d_t), intent(in) :: self
        weights = storage_size(self)
    end function divergence_1d_weights

end module separate_compilation_33a
