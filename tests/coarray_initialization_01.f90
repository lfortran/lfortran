module coarray_saved_mod
    integer :: x1[*] = 666
    integer :: y1(10)[*] = 1001
contains
    subroutine mod_sub()
    end subroutine
end module coarray_saved_mod

subroutine coarray_saved_sub()
    integer, save :: x[*] = 42
    integer, save :: y(10)[*] = 43
end subroutine coarray_saved_sub

program coarray_initialization_01
    use coarray_saved_mod
    implicit none

    integer :: a[*] = 5
    integer :: b(10)[*] = 6
    integer, save :: c[*] = 7
    integer, save :: d(10)[*] = 8
    integer :: me

    me = this_image()

    call mod_sub()
    call coarray_saved_sub()


    sync all

    if (me == 1) then
        a = a[2] + 1
    end if

    sync all
end program
