module coarray_saved_mod
    integer :: w[*]
    integer :: x(10)[*]
    integer, save :: y[*]
    integer, save :: z(10)[*]
contains
    subroutine mod_sub()
    end subroutine
end module coarray_saved_mod

subroutine coarray_saved_sub()
    integer, save :: w[*]
    integer, save :: x(10)[*]
end subroutine coarray_saved_sub

program coarray_saved_01

    use coarray_saved_mod

    integer, save :: a[*]
    integer, save :: b(10)[*]
    integer :: c[*]
    integer :: d(10)[*]

    call mod_sub()
    call coarray_saved_sub()
end program coarray_saved_01
