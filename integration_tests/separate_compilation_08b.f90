module module_using_all_2_separate_compilation_08b
contains
subroutine test_all_2(x)
real :: x(5)
if ( all(abs(x - 19.2134) < 1e-8 )) error stop
end subroutine
end module
