module separate_compilation_59b_module
use separate_compilation_59a_module, only: base
implicit none
contains
integer function next_value()
next_value = base + 1
end function
end module
