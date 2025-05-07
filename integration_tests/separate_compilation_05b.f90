module separate_compilation_05b_module

use separate_compilation_05a_module
implicit none

contains

subroutine temp(A)
real, allocatable :: A(:, :)
call resol_lu(A)
end subroutine

end module
