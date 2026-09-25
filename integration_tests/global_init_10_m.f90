! `state` is laid out as static data that already holds `n = 7` and a null
! `p`. Only `a` needs code at startup, a descriptor of its own, which the
! startup hook of this module's object file sets up. That hook must leave `n`
! and `p` alone: a constructor that ran before it may already have changed
! them, as the one in global_init_10c.c does.
module global_init_10_m
    use iso_c_binding, only: c_int
    implicit none
    type :: t
        integer(c_int) :: n = 7
        integer(c_int), pointer :: p => null()
        integer(c_int), allocatable :: a(:)
    end type
    type(t) :: state
    integer(c_int), target :: tgt = 42
end module global_init_10_m
