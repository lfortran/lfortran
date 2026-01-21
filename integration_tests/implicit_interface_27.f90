program implicit_interface_27
implicit none
complex :: c
complex, external :: test_complex
c = test_complex()
if (abs(real(c) - 21.0) > 1e-6) error stop
if (abs(aimag(c) - (-3.0)) > 1e-6) error stop
print *, "ok"
end program
