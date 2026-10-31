! Standard-Fortran twin of traits_generic_override_01.f90: each generic
! binding is one specific binding per type-set member, overridden by the
! extensions.
module traits_generic_override_01_oracle_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: Base, Scaled, Doubled, consume_i, consume_r, calls

    type :: Base
        integer :: tag = 1
    contains
        procedure :: sum_i => base_sum_i
        procedure :: sum_r => base_sum_r
        procedure :: leaf_i => base_leaf_i
        procedure :: leaf_r => base_leaf_r
        procedure, nopass :: label_i => base_label_i
        procedure :: record_i => base_record_i
        procedure :: record_r => base_record_r
    end type Base

    type, extends(Base) :: Scaled
    contains
        procedure :: leaf_i => scaled_leaf_i
        procedure :: leaf_r => scaled_leaf_r
        procedure :: record_i => scaled_record_i
        procedure :: record_r => scaled_record_r
    end type Scaled

    type, extends(Scaled) :: Doubled
    contains
        procedure :: sum_i => doubled_sum_i
        procedure :: sum_r => doubled_sum_r
        procedure :: leaf_i => doubled_leaf_i
        procedure :: leaf_r => doubled_leaf_r
        procedure, nopass :: label_i => doubled_label_i
    end type Doubled

    integer :: calls = 0

contains

    integer function base_sum_i(self, x) result(s)
        class(Base), intent(in) :: self
        integer, intent(in) :: x(:)
        s = self%leaf_i(x)
    end function base_sum_i

    real(real64) function base_sum_r(self, x) result(s)
        class(Base), intent(in) :: self
        real(real64), intent(in) :: x(:)
        s = self%leaf_r(x)
    end function base_sum_r

    integer function base_leaf_i(self, x) result(s)
        class(Base), intent(in) :: self
        integer, intent(in) :: x(:)
        s = sum(x)
    end function base_leaf_i

    real(real64) function base_leaf_r(self, x) result(s)
        class(Base), intent(in) :: self
        real(real64), intent(in) :: x(:)
        s = sum(x)
    end function base_leaf_r

    integer function scaled_leaf_i(self, x) result(s)
        class(Scaled), intent(in) :: self
        integer, intent(in) :: x(:)
        s = 10 * sum(x)
    end function scaled_leaf_i

    real(real64) function scaled_leaf_r(self, x) result(s)
        class(Scaled), intent(in) :: self
        real(real64), intent(in) :: x(:)
        s = 10 * sum(x)
    end function scaled_leaf_r

    integer function doubled_leaf_i(self, x) result(s)
        class(Doubled), intent(in) :: self
        integer, intent(in) :: x(:)
        s = 100 * sum(x)
    end function doubled_leaf_i

    real(real64) function doubled_leaf_r(self, x) result(s)
        class(Doubled), intent(in) :: self
        real(real64), intent(in) :: x(:)
        s = 100 * sum(x)
    end function doubled_leaf_r

    integer function doubled_sum_i(self, x) result(s)
        class(Doubled), intent(in) :: self
        integer, intent(in) :: x(:)
        s = 2 * self%leaf_i(x)
    end function doubled_sum_i

    real(real64) function doubled_sum_r(self, x) result(s)
        class(Doubled), intent(in) :: self
        real(real64), intent(in) :: x(:)
        s = 2 * self%leaf_r(x)
    end function doubled_sum_r

    integer function base_label_i(x) result(s)
        integer, intent(in) :: x
        s = x + 1
    end function base_label_i

    integer function doubled_label_i(x) result(s)
        integer, intent(in) :: x
        s = x + 2
    end function doubled_label_i

    subroutine base_record_i(self, x)
        class(Base), intent(in) :: self
        integer, intent(in) :: x
        calls = calls + 1
    end subroutine base_record_i

    subroutine base_record_r(self, x)
        class(Base), intent(in) :: self
        real(real64), intent(in) :: x
        calls = calls + 1
    end subroutine base_record_r

    subroutine scaled_record_i(self, x)
        class(Scaled), intent(in) :: self
        integer, intent(in) :: x
        calls = calls + 10
    end subroutine scaled_record_i

    subroutine scaled_record_r(self, x)
        class(Scaled), intent(in) :: self
        real(real64), intent(in) :: x
        calls = calls + 10
    end subroutine scaled_record_r

    integer function consume_i(object, x) result(s)
        class(Base), intent(in) :: object
        integer, intent(in) :: x(:)
        s = object%sum_i(x) + object%leaf_i(x)
    end function consume_i

    real(real64) function consume_r(object, x) result(s)
        class(Base), intent(in) :: object
        real(real64), intent(in) :: x(:)
        s = object%sum_r(x) + object%leaf_r(x)
    end function consume_r

end module traits_generic_override_01_oracle_m

program traits_generic_override_01_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_01_oracle_m
    implicit none
    class(Base), allocatable :: b
    type(Doubled) :: d
    integer :: xi(3), i
    real(real64) :: xf(2)
    integer :: leaves(3), sums(3), consumed(3)
    real(real64) :: fleaves(3), fconsumed(3)

    xi = [1, 2, 3]
    xf = [0.5_real64, 1.5_real64]
    leaves = [6, 60, 600]
    sums = [6, 60, 1200]
    consumed = [12, 120, 1800]
    fleaves = [2.0_real64, 20.0_real64, 200.0_real64]
    fconsumed = [4.0_real64, 40.0_real64, 600.0_real64]

    do i = 1, 3
        if (allocated(b)) deallocate(b)
        select case (i)
        case (1)
            allocate(b, source=Base())
        case (2)
            allocate(b, source=Scaled())
        case (3)
            allocate(b, source=Doubled())
        end select
        if (b%leaf_i(xi) /= leaves(i)) error stop 1
        if (abs(b%leaf_r(xf) - fleaves(i)) > 1e-12_real64) error stop 3
        if (b%sum_i(xi) /= sums(i)) error stop 5
        if (consume_i(b, xi) /= consumed(i)) error stop 6
        if (abs(consume_r(b, xf) - fconsumed(i)) > 1e-12_real64) error stop 7
        calls = 0
        call b%record_i(1)
        call b%record_r(1.0_real64)
        if (i == 1 .and. calls /= 2) error stop 8
        if (i > 1 .and. calls /= 20) error stop 9
    end do

    if (b%label_i(1) /= 3) error stop 10
    deallocate(b)
    allocate(b, source=Scaled())
    if (b%label_i(1) /= 2) error stop 11

    if (d%leaf_i(xi) /= 600) error stop 12
    if (d%sum_i(xi) /= 1200) error stop 13

    print *, "ok"
end program traits_generic_override_01_oracle
