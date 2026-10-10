program separate_compilation_59
use separate_compilation_59b_module, only: next_value
implicit none
interface
    integer function fixed_value()
    end function
end interface
if (next_value() /= 41) error stop
if (fixed_value() /= 3) error stop
print *, next_value(), fixed_value()
end program
