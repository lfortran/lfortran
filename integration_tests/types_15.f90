program types_15
    implicit none
    integer, parameter :: dp = kind(0.d0)

    integer(kind=4) :: n_src, n_des
    real(kind=4) :: z_src, z_des

    integer(kind=8) :: n_big_src, n_big_des
    real(dp) :: z_big_src, z_big_des

    n_src = 2
    n_big_src = 2_8
    z_src = 3.14
    z_big_src = 3.14_dp

    print *, n_src, n_big_src, z_src, z_big_src

    n_des = n_src
    n_big_des = n_big_src
    z_des = z_src
    z_big_des = z_big_src

    print *, n_des, n_big_des, z_des, z_big_des
    
    n_des = n_big_src
    n_big_des = z_src
    z_des = z_big_src
    z_big_des = n_src

    print *, n_des, n_big_des, z_des, z_big_des
    
    n_des = z_src
    n_big_des = z_big_src
    z_des = n_src
    z_big_des = n_big_src

    print *, n_des, n_big_des, z_des, z_big_des
    
    n_des = z_big_src
    n_big_des = n_src
    z_des = n_big_src
    z_big_des = z_src

    print *, n_des, n_big_des, z_des, z_big_des
end program
