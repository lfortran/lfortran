program write_array_pointer_item_c
    ! The C backend cannot write an array item, including a strided pointer
    ! array, which it reports instead of failing in the code generator.
    implicit none
    integer, target :: x(10)
    integer, pointer :: p(:)
    character(len=40) :: s
    integer :: i
    x = [(i, i = 1, 10)]
    p => x(1:10:3)
    write(s, '(4i3)') p
    print '(a)', trim(s)
end program
