module module_namespaces_01_mod
implicit none
integer :: dummy = 3
end module


program module_namespaces_01
use, namespace :: module_namespaces_01_mod
implicit none

print *, module_namespaces_01 % dummy
end
