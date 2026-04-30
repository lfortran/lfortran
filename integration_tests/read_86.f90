module read_86_mod
implicit none
integer, parameter :: dp = kind(1.0d0)
contains

subroutine read_vec_real(text, xx, nread)
    character(len=*), intent(in) :: text
    real(kind=dp), intent(out), allocatable :: xx(:)
    integer, intent(out) :: nread
    real(kind=dp), allocatable :: xread(:)
    integer :: i, ierr, nread_, max_read_

    max_read_ = 4
    allocate(xread(max_read_))
    nread_ = 0
    do i = max_read_, 1, -1
        read(text, *, iostat=ierr) xread(:i)
        if (ierr == 0) then
            nread_ = i
            exit
        end if
    end do
    nread = nread_
    allocate(xx(nread_))
    xx = xread(:nread_)
    deallocate(xread)
end subroutine read_vec_real

end module read_86_mod

program read_86
use read_86_mod, only: read_vec_real, dp
implicit none
real(kind=dp), allocatable :: x(:)
integer :: nread

call read_vec_real("1.5 2.5 3.5", x, nread)
if (nread /= 3) error stop
if (any(abs(x - (/1.5_dp, 2.5_dp, 3.5_dp/)) > 1.0d-12)) error stop
end program read_86
