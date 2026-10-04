! Module storage that the native link and load tests put in a static archive,
! a shared library of its own, or a shared library together with
! global_init_20_b and an early C constructor; see "Native link and load
! variants" in CMakeLists.txt.
module global_init_20_a
    use iso_c_binding, only: c_int
    implicit none
    type :: holder
        character(len=3) :: s = "abc"
        integer(c_int) :: v(3) = [1, 2, 3]
        integer(c_int), allocatable :: extra(:)
    end type
    type(holder), target :: h
    integer(c_int), allocatable :: arr(:)
    integer(c_int), pointer :: parr(:) => null()
contains
    ! Called by the C programs that link this module's shared library, so that
    ! a linker that drops libraries nothing refers to (--as-needed) keeps it:
    ! global_init_20_b's library relies on it without linking it.
    integer(c_int) function first() bind(c, name="global_init_20_a_first")
        first = h%v(1)
    end function first
end module global_init_20_a
