module separate_compilation_73a
implicit none
real(10), parameter :: c = 1.5_10
contains
    real(10) function half(x)
    real(10), intent(in) :: x
    half = x / 2
    end function
end module
