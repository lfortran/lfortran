program intrinsics_393
       integer, parameter :: n = 10
       real(8) :: x(n)
       complex(8) :: z(n)
       complex(8) :: A(n,n)

       x(:) = 1.0d0
       A(:,:) = cmplx(1.0d0,0.0d0)

       z = matmul (A, cmplx(x,0.0,8))
       print *, z(1)
       if (abs(z(1) - (10.000000000000000,0.0000000000000000)) > 1e-12) error stop
end program intrinsics_393