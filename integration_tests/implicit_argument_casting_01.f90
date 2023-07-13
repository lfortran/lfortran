subroutine idz_realcomp(n, a)
implicit none
integer, parameter :: dp = kind(0.d0)
integer n
real(dp) a(n)
a = 12.5d0
end subroutine


program main
implicit none
integer, parameter :: dp = kind(0.d0)
complex(dp) :: w(5)
call idzp_svd(5, w)

contains

    subroutine idzp_svd(ls, w)
    implicit none
    complex(dp) :: w(*)
    integer :: ls, isi
    isi = 1
    call idz_realcomp(ls*2, w(isi))
    print *, w(isi)
    if (abs(real(w(isi), dp) - 12.5d0) > 1e-12) error stop
    end

end program
