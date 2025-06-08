module module_using_all_1_separate_compilation_08a
contains
subroutine test_all(x)
real :: x(5)
if ( all(abs(x - 9.2134) < 1e-8 ) ) error stop
end subroutine
end module
