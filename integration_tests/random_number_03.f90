module random_number_03_mod
    implicit none
    type :: bounds_t
        integer :: n
    end type
contains
    subroutine fill_noise(mesh, total)
        type(bounds_t), intent(in) :: mesh
        real, intent(out) :: total
        real :: noise(mesh%n)
        noise = 5.0
        call random_number(noise(:))
        if (size(noise) /= mesh%n) error stop 1
        if (any(noise < 0.0) .or. any(noise >= 1.0)) error stop 2
        total = sum(noise)
    end subroutine

    subroutine fill_dummy(x)
        real, intent(inout) :: x(:)
        call random_number(x(2:3))
    end subroutine
end module

program random_number_03
    ! random_number with array section arguments
    use random_number_03_mod, only: bounds_t, fill_noise, fill_dummy
    implicit none
    type(bounds_t) :: mesh
    real :: total
    real :: a(4)
    real :: b(6)
    real :: c(3, 4)

    mesh%n = 7
    call fill_noise(mesh, total)
    if (total < 0.0 .or. total >= 7.0) error stop 3

    a = 5.0
    call random_number(a(1:4:2))
    if (a(1) < 0.0 .or. a(1) >= 1.0 .or. a(3) < 0.0 .or. a(3) >= 1.0) error stop 4
    if (a(2) /= 5.0 .or. a(4) /= 5.0) error stop 5

    a = 5.0
    call fill_dummy(a)
    if (any(a(2:3) < 0.0) .or. any(a(2:3) >= 1.0)) error stop 6
    if (a(1) /= 5.0 .or. a(4) /= 5.0) error stop 7

    b = 5.0
    call random_number(b(1:6:2))
    if (any(b(1:6:2) < 0.0) .or. any(b(1:6:2) >= 1.0)) error stop 8
    if (any(b(2:6:2) /= 5.0)) error stop 9

    c = 5.0
    call random_number(c(2:3, 2:4))
    if (any(c(2:3, 2:4) < 0.0) .or. any(c(2:3, 2:4) >= 1.0)) error stop 10
    if (any(c(1, :) /= 5.0) .or. any(c(:, 1) /= 5.0)) error stop 11
    print *, "ok"
end program
