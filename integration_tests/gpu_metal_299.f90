! BLOCK-local derived types. The local-width sweep used to accept every
! StructType without walking members, so a complex or real(8) component
! compiled as the wrong width (or as an unsupported-type comment). Both
! must stay on the host. `run_real` still offloads a derived type of
! real(4) of the same shape.
program gpu_metal_299
implicit none
type :: cpt
    complex :: z
end type
type :: dpt
    real(8) :: x
end type
type :: rpt
    real :: x
end type
real :: a(4), b(4), c(4)
integer :: i

a = 0.0
b = 0.0
c = 0.0
call run_complex(a)
call run_real8(b)
call run_real(c)

do i = 1, 4
    if (abs(a(i) - 1.0) > 1.0e-5) error stop "complex"
    if (abs(b(i) - 2.0) > 1.0e-5) error stop "real8"
    if (abs(c(i) - real(2 * i)) > 1.0e-5) error stop "real"
end do
print *, "PASS"

contains

    subroutine run_complex(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                type(cpt) :: p
                p%z = cmplx(1.0, 0.0)
                r(i) = real(p%z)
            end block
        end do
    end subroutine

    subroutine run_real8(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                type(dpt) :: p
                p%x = 2.0d0
                r(i) = real(p%x)
            end block
        end do
    end subroutine

    subroutine run_real(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                type(rpt) :: p
                p%x = real(2 * i)
                r(i) = p%x
            end block
        end do
    end subroutine

end program
