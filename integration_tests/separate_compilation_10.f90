program separate_compilation_10
use history_mod_separate_compilation_10a
real :: xhist(5,5)
call rangehist(xhist)
print *, xhist
if (any(abs(xhist - 99.124) > 1e-8)) error stop
end program
