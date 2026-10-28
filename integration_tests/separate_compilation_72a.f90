! Module of separate_compilation_72. No program unit uses it: only the
! external procedures of separate_compilation_72b.f90 do.
module separate_compilation_72_mod
implicit none
type t_empty
    character(len=8) :: s = ''
end type
type t_blank
    character(len=64) :: s = ' '
end type
type t_none
    character(len=64) :: s
end type
type t_short
    character(len=6) :: s = 'ab'
end type
type t_outer
    integer :: n = 3
    type(t_short) :: inner
end type
type(t_empty), save :: x, y
type(t_blank), save :: b1, b2
type(t_none), save :: n1, n2
type(t_short), save :: sh
type(t_outer), save :: o
end module
