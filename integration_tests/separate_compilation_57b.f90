module separate_compilation_57b_module
use separate_compilation_57a_module, only: base
implicit none
contains
integer function next_value()
next_value = base + 1
end function
end module
