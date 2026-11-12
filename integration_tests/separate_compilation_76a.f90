module separate_compilation_76a
    implicit none
contains
    subroutine sum_saved(res)
        real(16), intent(out) :: res
        real(16), dimension(2) :: c = [1.5_16, 2.5_16]
        integer :: n = 7
        res = c(1) + c(2) + n
    end subroutine
end module
