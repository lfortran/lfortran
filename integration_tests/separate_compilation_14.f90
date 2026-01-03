program separate_compilation_14a
use separate_compilation_14a_module
print *, global_var
print *, global_var_initialised
if (global_var_initialised /= 89) error stop
end program separate_compilation_14a
