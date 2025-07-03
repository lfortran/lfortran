program intrinsics_389
    implicit none
    integer :: i
    integer, save :: j

    print*, "main not saved", loc(i)
    print*, "main     saved", loc(j)
    call sub()
contains
    subroutine sub()
        integer :: i
        integer, save :: j

        print*, "sub  not saved", loc(i)
        print*, "sub      saved", loc(j)
    end
end program intrinsics_389