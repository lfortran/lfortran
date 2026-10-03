! Module initialization that the Fortran backend has to write out as
! Fortran: the elements of an array of a derived type get their component
! defaults from a loop of the module's initializer, one of them a string,
! next to a pointer associated with a module target and a string variable.
! CMakeLists.txt also registers this file for the `fortran` backend with the
! initialization passes spelled out, so that the Fortran that backend prints
! for them is compiled and run by GFortran. The module's name is long so
! that the names that backend derives from it have to stay within the 63
! characters a Fortran name may have.
module global_init_25_module_with_a_long_name
    implicit none
    type :: cell
        integer :: n = 7
        character(len=3) :: tag = "abc"
    end type
    type(cell) :: cells(3)
    integer, target :: tgt = 42
    integer, pointer :: p => tgt
    character(len=4) :: word = "wxyz"
end module global_init_25_module_with_a_long_name

program global_init_25
    use global_init_25_module_with_a_long_name
    implicit none
    integer :: i
    do i = 1, 3
        if (cells(i)%n /= 7) error stop 1
        if (cells(i)%tag /= "abc") error stop 2
    end do
    if (.not. associated(p, tgt)) error stop 3
    if (p /= 42) error stop 4
    if (word /= "wxyz") error stop 5
    cells(2)%n = 70
    cells(2)%tag = "xyz"
    if (cells(1)%n /= 7 .or. cells(3)%tag /= "abc") error stop 6
    p = 43
    if (tgt /= 43) error stop 7
    print *, "ok"
end program global_init_25
