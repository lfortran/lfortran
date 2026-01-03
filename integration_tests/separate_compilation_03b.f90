module separate_compilation_03b_module

contains

real function mixing_anderson(x0)
! Finds "x" so that R(x) = 0, uses x0 as the initial estimate
real, intent(in) :: x0(:)
mixing_anderson = sum(x0)
end function

end module
