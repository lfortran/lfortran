! The value of an elementwise assignment that reads the target only through
! the same section or the whole target is read in place, without a
! temporary, while a shifted section still needs one (issue #16105).
module array_section_assign_11_mod
    implicit none
contains

    subroutine upd(a, x, i, m)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i, m
        a(i,1:m) = a(i,1:m) + x(1:m)*2.0
        a = a + 1.0
    end subroutine

    subroutine shift(a, x, i, m)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i, m
        a(i,2:m) = a(i,1:m-1) + x(1:m-1)
    end subroutine

end module

program array_section_assign_11
    use array_section_assign_11_mod
    implicit none
    real :: a(2,4), x(4)
    a = 1.0
    x = [1.0, 2.0, 3.0, 4.0]
    call upd(a, x, 1, 3)
    if (any(a(1,:) /= [4.0, 6.0, 8.0, 2.0])) error stop 1
    if (any(a(2,:) /= 2.0)) error stop 2
    call shift(a, x, 1, 4)
    if (any(a(1,:) /= [4.0, 5.0, 8.0, 11.0])) error stop 3
    print *, a
end program
