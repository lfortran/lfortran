module module_interface_17
interface precv
module procedure precv_d_3d
module procedure precv_d_2d
end interface precv
contains

subroutine precv_d_2d ( val, nx )
real, dimension(nx, nx), intent(in) :: val
integer, intent(in) ::  nx
print *, sum(val)
if (abs(sum(val) - 318.560028) > 1e-8) error stop
end subroutine precv_d_2d

subroutine precv_d_3d ( val, nx )
real, dimension(nx, nx, nx), intent(in) :: val
integer, intent(in) ::  nx
print *, sum(val)
if (abs(sum(val) - 1274.23975) > 1e-8) error stop
end subroutine precv_d_3d

subroutine sweep_recv_bdry ( nz )
real, dimension(nz, nz, nz):: jb_in
real, dimension(nz, nz):: jb_in_2
integer, intent(in) ::  nz
jb_in = 19.91
call precv ( jb_in, nz )
jb_in_2 = 19.91
call precv ( jb_in_2, nz )
end subroutine sweep_recv_bdry
end module module_interface_17

program interface_17
use module_interface_17
call sweep_recv_bdry(4)
end program
