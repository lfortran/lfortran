! A function with a character result and an implicit interface referenced in
! BLOCK constructs (nested, inside DO, declared in the BLOCK), in SELECT TYPE
! and SELECT RANK bodies, and through a dummy procedure.
module implicit_interface_89_m
    implicit none
contains
    subroutine ii89_dummy(f)
        character(len=3), external :: f
        block
            if (f(2) /= "x2 ") error stop 1
        end block
    end subroutine

    subroutine ii89_rank(a)
        integer, intent(in) :: a(..)
        character(len=3), external :: ii89_c
        select rank (a)
        rank (1)
            if (ii89_c(size(a)) /= "x2 ") error stop 2
        rank default
            error stop 3
        end select
    end subroutine
end module

program implicit_interface_89
    use implicit_interface_89_m
    implicit none
    character(len=3), external :: ii89_c
    class(*), allocatable :: x
    integer :: i
    block
        block
            if (ii89_c(1) /= "x1 ") error stop 4
        end block
    end block
    do i = 1, 2
        block
            if (ii89_c(i) /= "x" // achar(iachar("0") + i) // " ") error stop 5
        end block
    end do
    block
        character(len=3), external :: ii89_c
        if (ii89_c(3) /= "x3 ") error stop 6
    end block
    x = 4
    select type (x)
    type is (integer)
        if (ii89_c(x) /= "x4 ") error stop 7
    end select
    call ii89_dummy(ii89_c)
    call ii89_rank([1, 2])
    print *, ii89_c(5)
end program

character(len=3) function ii89_c(i)
    integer, intent(in) :: i
    write(ii89_c, '(a,i1)') "x", i
end function
