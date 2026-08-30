module gpu_metal_232_m
! An array constructor `[a, b, c]` is lowered by the array-constructor pass
! into a counter-driven sequence: `idx = 1`, `ac(idx) = a`, `idx = idx + 1`,
! ...  When the constructor sits inside an ASSOCIATE that GPU offload has to
! inline, the counter temporary lives in the AssociateBlock's own symbol
! table, and the inliner used to mistake every such assignment for an
! ASSOCIATE selector binding: it absorbed `idx = 1` into its substitution
! map, deleted the statement, and then rewrote every `idx` -- including the
! target of the surviving increments -- to the literal 1.  The Metal shader
! then failed to compile ("1 = (1 + 1);"), and any shape that still compiled
! would have written every element to index 1.  Hence the value checks below.
implicit none
contains

    ! Constructor inside an ASSOCIATE, assigned to an array section.
    subroutine assoc_section(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(n,2)
        integer :: j
        do concurrent (j = 1:n)
            associate (e => 2.0 * x(j))
                g(j,:) = [x(j), e]
            end associate
        end do
    end subroutine

    ! Four-element constructor inside an ASSOCIATE, assigned to a whole
    ! array.  A folded counter makes all four elements land on index 1.
    subroutine assoc_whole(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        integer :: j
        real :: t(4)
        do concurrent (j = 1:n)
            associate (e => 10.0 * x(j))
                t = [x(j), e, e + 1.0, e + 2.0]
                g(:,j) = t
            end associate
        end do
    end subroutine

    ! An ASSOCIATE whose selector is genuinely single-assignment, with no
    ! array constructor at all.  This is the fence: normal associate
    ! inlining must keep working.
    subroutine assoc_plain(x, y, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: y(n)
        integer :: j
        do concurrent (j = 1:n)
            associate (nn => n)
                associate (s => x(j) + real(nn))
                    y(j) = 3.0 * s
                end associate
            end associate
        end do
    end subroutine

end module

program gpu_metal_232
use gpu_metal_232_m
implicit none
integer, parameter :: n = 4
real :: x(n), g2(n,2), g4(4,n), y(n)
integer :: j

do j = 1, n
    x(j) = real(j)
end do

g2 = 0.0
call assoc_section(x, g2, n)
print *, g2
do j = 1, n
    if (abs(g2(j,1) - x(j)) > 1.0e-5) error stop "section: element 1"
    if (abs(g2(j,2) - 2.0 * x(j)) > 1.0e-5) error stop "section: element 2"
end do

g4 = 0.0
call assoc_whole(x, g4, n)
print *, g4
do j = 1, n
    if (abs(g4(1,j) - x(j)) > 1.0e-5) error stop "whole: element 1"
    if (abs(g4(2,j) - 10.0 * x(j)) > 1.0e-5) error stop "whole: element 2"
    if (abs(g4(3,j) - (10.0 * x(j) + 1.0)) > 1.0e-5) error stop "whole: element 3"
    if (abs(g4(4,j) - (10.0 * x(j) + 2.0)) > 1.0e-5) error stop "whole: element 4"
end do

y = 0.0
call assoc_plain(x, y, n)
print *, y
do j = 1, n
    if (abs(y(j) - 3.0 * (x(j) + real(n))) > 1.0e-5) error stop "plain associate"
end do

print *, "ok"
end program
