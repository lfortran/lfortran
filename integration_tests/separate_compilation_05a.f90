module separate_compilation_05a_module
implicit none

contains

subroutine lu()
end subroutine


subroutine resol_lu(L)
real, allocatable :: L(:,:)
print *, sum(L)
if ( abs(sum(L) - 2.08361094e+04 ) > 1e-8 ) error stop
end subroutine

end module
