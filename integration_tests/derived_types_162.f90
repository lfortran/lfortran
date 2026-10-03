module derived_types_162_mod
implicit none

type :: inner_t
    integer :: handle = 0
end type inner_t

type(inner_t), parameter :: inner_null = inner_t(5)

type :: outer_t
    type(inner_t) :: part = inner_null
end type outer_t

type(inner_t) :: module_var = inner_null

end module derived_types_162_mod

program derived_types_162
use derived_types_162_mod, only: outer_t, module_var
implicit none

type :: a_t
    integer :: h
end type a_t

type(a_t), parameter :: z = a_t(7)

type :: b_t
    type(a_t) :: p = z
end type b_t

type(outer_t) :: v
type(b_t) :: w
type(a_t) :: local_var = z

print *, v%part%handle, w%p%h, module_var%handle, local_var%h
if (v%part%handle /= 5) error stop
if (w%p%h /= 7) error stop
if (module_var%handle /= 5) error stop
if (local_var%h /= 7) error stop
end program derived_types_162
