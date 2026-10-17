! select type on a rank-2 assumed-size class(*) dummy: the associate name
! gets a valid pointer-to-array type and reads the actual argument's data.
module select_type_65_mod
    implicit none
contains

    subroutine check(a, n, kind_id)
        integer, intent(in) :: n, kind_id
        class(*), target, intent(in) :: a(2, *)
        integer :: i, j
        select type (b => a)
        type is (integer)
            if (kind_id /= 1) error stop 1
            do j = 1, n
                do i = 1, 2
                    if (b(i, j) /= i + 2*(j - 1)) error stop 2
                end do
            end do
        type is (real)
            if (kind_id /= 2) error stop 3
            if (abs(b(1, 1) - 0.5) > 1e-6) error stop 4
            if (abs(b(2, n) - 0.5*real(2*n)) > 1e-6) error stop 5
        class default
            error stop 6
        end select
    end subroutine

end module

program select_type_65
    use select_type_65_mod
    implicit none
    integer, target :: x(2, 3)
    real, target :: r(2, 2)
    integer :: i
    x = reshape([(i, i = 1, 6)], [2, 3])
    call check(x, 3, 1)
    r = reshape([(0.5*real(i), i = 1, 4)], [2, 2])
    call check(r, 2, 2)
    print *, "ok"
end program
