module intrinsics_368_test_mod
    implicit none
    logical, parameter :: param=ichar(transfer(1,'a')) == 0
end module intrinsics_368_test_mod
program intrinsics_368
    use intrinsics_368_test_mod
    implicit none
    integer(kind=transfer(4_1, 0)) :: x1
    integer(kind=kind(0)) :: x2
    integer(kind=(4+0)) :: x3
    integer :: i
    integer(kind=sum([(i, i=0,3)])-2) :: x4
    integer(kind=INT(61*BESSEL_JN(4, 2.5))) :: x5
    integer,parameter::dp=kind(1d0),nextp=selected_real_kind(precision(1d0)+1)
    integer,parameter::ep = merge(nextp,dp,nextp>0)
    print *,'pi=acos(-1.0_dp)=', acos(-1.0_dp)
    if (abs(acos(-1.0_dp) - 3.1415926535897931) > 1e-7) error stop
    print *,'pi=acos(-1.0_ep)=',acos(-1.0_ep)
    if (abs(acos(-1.0_ep) - 3.1415926535897931) > 1e-7) error stop
end program