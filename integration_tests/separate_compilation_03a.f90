module separate_compilation_03a_module

contains

real function integrate_trapz_1(Rp) result(s)
real, intent(in) :: Rp(:)
s = sum(Rp)
end function

end module
