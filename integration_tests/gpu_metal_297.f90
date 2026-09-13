! BLOCK-local complex and character variables. The local-width sweep used
! to consult the table only for integer/real/logical and then accept
! everything else, so the pass committed to offload and the emitter
! printed an unsupported-type comment. Both loops must stay on the host.
! `run_real` still offloads a real local of the same shape.
program gpu_metal_297
implicit none
real :: a(4), b(4), c(4)
integer :: i

a = 0.0
b = 0.0
c = 0.0
call run_complex(a)
call run_character(b)
call run_real(c)

do i = 1, 4
    if (abs(a(i) - 1.0) > 1.0e-5) error stop "complex"
    if (abs(b(i) - 2.0) > 1.0e-5) error stop "character"
    if (abs(c(i) - real(2 * i)) > 1.0e-5) error stop "real"
end do
print *, "PASS"

contains

    subroutine run_complex(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                complex :: z
                z = cmplx(1.0, 0.0)
                r(i) = real(z)
            end block
        end do
    end subroutine

    subroutine run_character(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                character(len=1) :: ch
                ch = "x"
                if (ch == "x") then
                    r(i) = 2.0
                else
                    r(i) = 0.0
                end if
            end block
        end do
    end subroutine

    subroutine run_real(r)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            block
                real :: t
                t = real(2 * i)
                r(i) = t
            end block
        end do
    end subroutine

end program
