! Pointer declaration initializers whose target is a designator rather than a
! whole variable: an array element, a component, and a component's element.
module global_init_05_m
implicit none

type :: holder
    integer :: v = 3
    integer :: w(2) = [7, 8]
end type

integer, save, target :: arr(3) = [1, 2, 3]
type(holder), save, target :: dt

integer, pointer :: p_elem => arr(2)
integer, pointer :: p_comp => dt%v
integer, pointer :: p_compelem => dt%w(2)

end module

program global_init_05
use global_init_05_m
implicit none

if (.not. associated(p_elem, arr(2))) error stop 1
if (p_elem /= 2) error stop 2
p_elem = 20
if (arr(2) /= 20) error stop 3

if (.not. associated(p_comp, dt%v)) error stop 4
if (p_comp /= 3) error stop 5
p_comp = 30
if (dt%v /= 30) error stop 6

if (p_compelem /= 8) error stop 7
p_compelem = 80
if (dt%w(2) /= 80) error stop 8

print *, "ok"
end program
