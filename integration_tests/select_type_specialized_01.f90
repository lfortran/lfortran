module select_type_specialized_01_m
    implicit none
    type :: holder_t
        integer :: count = 0
    contains
        procedure :: run_real
    end type holder_t
contains
    subroutine run_real(self, values, total)
        class(holder_t), intent(inout) :: self
        real(8), intent(in) :: values(:)
        real(8), intent(out) :: total
        call common(self, values, total)
    end subroutine run_real
    subroutine common(self, values, total)
        class(holder_t), intent(inout) :: self
        class(*), intent(in) :: values(:)
        real(8), intent(out) :: total
        call phases(self, values, total)
    end subroutine common
    subroutine phases(self, values, total)
        class(holder_t), intent(inout) :: self
        class(*), intent(in) :: values(:)
        real(8), intent(out) :: total
        integer :: rows(3)
        rows = 1
        call match(self, rows, values, total)
    end subroutine phases
    subroutine match(self, rows, values, total)
        class(holder_t), intent(inout) :: self
        integer, intent(in) :: rows(:)
        class(*), intent(in) :: values(:)
        real(8), intent(out) :: total
        total = -1
        select type (values)
        type is (real(8))
            self%count = size(values) + 100*size(rows)
            call inner(values, total)
        type is (complex(8))
            total = -2
        end select
    end subroutine match
    subroutine inner(values, total)
        real(8), intent(in) :: values(:)
        real(8), intent(out) :: total
        total = size(values) + 1000*sum(values)
    end subroutine inner
end module select_type_specialized_01_m

program select_type_specialized_01
    ! A procedure whose assumed-shape array dummy is passed by data is
    ! copied; in the copy, SELECT TYPE still associates its name with the
    ! selector dummy of the same name.
    use select_type_specialized_01_m
    implicit none
    type(holder_t) :: h
    real(8), allocatable :: a(:)
    real(8) :: t
    allocate(a(5))
    a = [1d0, 2d0, 3d0, 4d0, 5d0]
    call h%run_real(a, t)
    print *, t, h%count
    if (t /= 15005d0 .or. h%count /= 305) error stop 1
end program select_type_specialized_01
