! Compile and run in Release mode with:
! gfortran -O3 -march=native -ffast-math -funroll-loops matmul_01.f90 && ./a.out
! To develop, compile and run with:
! gfortran -Wall -Wextra -Wimplicit-interface -g -fcheck=all -fbacktrace matmul_01.f90 && ./a.out

module matmul_01_cpu
implicit none

contains

    subroutine matmul1(A, B, C)
    real, intent(in) :: A(:,:), B(:,:)
    real, intent(out) :: C(:,:)
    integer :: n
    integer :: i, j, k
    n = size(A, 1)
    C = 0
    do j = 1, n
    do k = 1, n
    do i = 1, n
        C(i,j) = C(i,j) + A(i,k)*B(k,j)
    end do
    end do
    end do
    end subroutine

    subroutine kernel6b(A, B, C, x, y, i1, s1)
    real, intent(in) :: A(:,:), B(:,:)
    real, intent(out) :: C(:,:)
    integer, intent(in) :: x, y, i1, s1
    integer :: k
    real, dimension(8) :: A0, A1, A2, A3, A4, A5 ! SIMD
    real, dimension(8) :: u00, u01, u10, u11, u20, u21, u30, u31, &
        u40, u41, u50, u51 ! SIMD
    ! This function computes:
    !C(x:x+6-1, y:y+16-1) = C(x:x+6-1, y:y+16-1) + matmul( &
    !    A(x:x+6-1, i1:i1+s1-1), &
    !    B(i1:i1+s1-1, y:y+16-1) )
    u00 = C(x+0,y  :y+ 7)
    u01 = C(x+0,y+8:y+15)

    u10 = C(x+1,y  :y+ 7)
    u11 = C(x+1,y+8:y+15)

    u20 = C(x+2,y  :y+ 7)
    u21 = C(x+2,y+8:y+15)

    u30 = C(x+3,y  :y+ 7)
    u31 = C(x+3,y+8:y+15)

    u40 = C(x+4,y  :y+ 7)
    u41 = C(x+4,y+8:y+15)

    u50 = C(x+5,y  :y+ 7)
    u51 = C(x+5,y+8:y+15)

    u50 = C(x+5,y  :y+ 7)
    u51 = C(x+5,y+8:y+15)

    do k = i1, i1+s1-1
        A0 = A(x+0,k)
        u00 = u00 + A0 * B(k, y  :y+ 7)
        u01 = u01 + A0 * B(k, y+8:y+15)

        A1 = A(x+1,k)
        u10 = u10 + A1 * B(k, y  :y+ 7)
        u11 = u11 + A1 * B(k, y+8:y+15)

        A2 = A(x+2,k)
        u20 = u20 + A2 * B(k, y  :y+ 7)
        u21 = u21 + A2 * B(k, y+8:y+15)

        A3 = A(x+3,k)
        u30 = u30 + A3 * B(k, y  :y+ 7)
        u31 = u31 + A3 * B(k, y+8:y+15)

        A4 = A(x+4,k)
        u40 = u40 + A4 * B(k, y  :y+ 7)
        u41 = u41 + A4 * B(k, y+8:y+15)

        A5 = A(x+5,k)
        u50 = u50 + A5 * B(k, y  :y+ 7)
        u51 = u51 + A5 * B(k, y+8:y+15)
    end do

    C(x+0,y  :y+ 7) = u00
    C(x+0,y+8:y+15) = u01

    C(x+1,y  :y+ 7) = u10
    C(x+1,y+8:y+15) = u11

    C(x+2,y  :y+ 7) = u20
    C(x+2,y+8:y+15) = u21

    C(x+3,y  :y+ 7) = u30
    C(x+3,y+8:y+15) = u31

    C(x+4,y  :y+ 7) = u40
    C(x+4,y+8:y+15) = u41

    C(x+5,y  :y+ 7) = u50
    C(x+5,y+8:y+15) = u51
    end subroutine

    ! Like kernel6b, but transposed
    ! The arguments A, B are swapped,
    ! and then all accesses to A, B, C are tranposed
    subroutine kernel6c(B, A, C, x, y, i1, s1)
    real, intent(in) :: A(:,:), B(:,:)
    real, intent(out) :: C(:,:)
    integer, intent(in) :: x, y, i1, s1
    integer :: k
    !LF$ attributes simd :: A0, A1, A2, A3, A4, A5
    real, dimension(8) :: A0, A1, A2, A3, A4, A5
    !LF$ attributes simd :: u00, u01, u10, u11, u20, u21
    !LF$ attributes simd :: u30, u31, u40, u41, u50, u51
    real, dimension(8) :: u00, u01, u10, u11, u20, u21, u30, u31, &
        u40, u41, u50, u51
    ! This function computes:
    !C(x:x+6-1, y:y+16-1) = C(x:x+6-1, y:y+16-1) + matmul( &
    !    A(x:x+6-1, i1:i1+s1-1), &
    !    B(i1:i1+s1-1, y:y+16-1) )
    u00 = C(y  :y+ 7,x+0)
    u01 = C(y+8:y+15,x+0)

    u10 = C(y  :y+ 7,x+1)
    u11 = C(y+8:y+15,x+1)

    u20 = C(y  :y+ 7,x+2)
    u21 = C(y+8:y+15,x+2)

    u30 = C(y  :y+ 7,x+3)
    u31 = C(y+8:y+15,x+3)

    u40 = C(y  :y+ 7,x+4)
    u41 = C(y+8:y+15,x+4)

    u50 = C(y  :y+ 7,x+5)
    u51 = C(y+8:y+15,x+5)

    do k = i1, i1+s1-1
        A0 = A(k,x+0)
        u00 = u00 + A0 * B(y  :y+ 7,k)
        u01 = u01 + A0 * B(y+8:y+15,k)

        A1 = A(k,x+1)
        u10 = u10 + A1 * B(y  :y+ 7,k)
        u11 = u11 + A1 * B(y+8:y+15,k)

        A2 = A(k,x+2)
        u20 = u20 + A2 * B(y  :y+ 7,k)
        u21 = u21 + A2 * B(y+8:y+15,k)

        A3 = A(k,x+3)
        u30 = u30 + A3 * B(y  :y+ 7,k)
        u31 = u31 + A3 * B(y+8:y+15,k)

        A4 = A(k,x+4)
        u40 = u40 + A4 * B(y  :y+ 7,k)
        u41 = u41 + A4 * B(y+8:y+15,k)

        A5 = A(k,x+5)
        u50 = u50 + A5 * B(y  :y+ 7,k)
        u51 = u51 + A5 * B(y+8:y+15,k)
    end do

    C(y  :y+ 7,x+0) = u00
    C(y+8:y+15,x+0) = u01

    C(y  :y+ 7,x+1) = u10
    C(y+8:y+15,x+1) = u11

    C(y  :y+ 7,x+2) = u20
    C(y+8:y+15,x+2) = u21

    C(y  :y+ 7,x+3) = u30
    C(y+8:y+15,x+3) = u31

    C(y  :y+ 7,x+4) = u40
    C(y+8:y+15,x+4) = u41

    C(y  :y+ 7,x+5) = u50
    C(y+8:y+15,x+5) = u51
    end subroutine

    subroutine kernel7(A, B, C, x, y, i1, s1)
    real, intent(in) :: A(:,:), B(:,:)
    real, intent(out) :: C(:,:)
    integer, intent(in) :: x, y, i1, s1
    integer :: i, j, k
    real :: u(0:6-1,0:16-1) ! SIMD
    real :: Ai(0:8-1) ! SIMD
    ! This function computes:
    !C(x:x+6-1, y:y+16-1) = C(x:x+6-1, y:y+16-1) + matmul( &
    !    A(x:x+6-1, i1:i1+s1-1), &
    !    B(i1:i1+s1-1, y:y+16-1) )
    do i = 0, 6-1
    do j = 0, 2-1
        u(i,j*8:j*8+8-1) = C(x+i,y+j*8:y+j*8+8-1)
    end do
    end do

    do k = i1, i1+s1-1
    do i = 0, 6-1
        Ai(:) = A(x+i,k)
        do j = 0, 2-1
            u(i, j*8:j*8+8-1) = u(i, j*8:j*8+8-1) + &
                Ai(:) * B(k, y+j*8:y+j*8+8-1)
        end do
    end do
    end do

    do i = 0, 6-1
    do j = 0, 2-1
        C(x+i,y+j*8:y+j*8+8-1) = u(i,j*8:j*8+8-1)
    end do
    end do
    end subroutine

    subroutine matmul7(A, B, C)
    real, intent(in) :: A(:,:), B(:,:)
    real, intent(out) :: C(:,:)
    integer :: s1, s2, s3, n
    integer :: i1, i2, i3, x, y
    n = size(A, 1)
    ! Use the commented out numbers for a good benchmark
    s3 = 48 ! 64
    s2 = 12 ! 120
    s1 = 24 ! 240
    C = 0
    do i3 = 1, n, s3
    do i2 = 1, n, s2
    do i1 = 1, n, s1
        do x = i2, i2+s2-1, 6
        do y = i3, i3+s3-1, 16
            call kernel6c(A, B, C, x, y, i1, s1)
        end do
        end do
    end do
    end do
    end do
    end subroutine

end module


program matmul_01
use matmul_01_cpu, only: matmul1, matmul7
implicit none

integer, parameter :: dp = kind(0.d0)
integer :: n, iter, i
real(dp) :: t1, t2, t, GHz, fma_clock, freq, measured, percent_peak
real, allocatable :: A(:,:), B(:,:), C(:,:), C2(:,:)

! Use n = 960 for a good benchmark
n = 96
! Increase `iter` so that the total time for a given benchmark is about 1s
! in order to get accurate timings
iter = 1

print *, "Size (n x n): n =", n
print *, "Iter =", iter
print *, "Size MB:", 4._dp*n*n/1024**2

allocate(A(n,n), B(n,n), C(n,n), C2(n,n))
call random_number(A)
call random_number(B)

print *, "Fortran intrinsic matmul:"
call cpu_time(t1)
do i = 1, iter
    C = matmul(A, B)
end do
call cpu_time(t2)
t = (t2-t1)/iter
GHz = 1e9_dp
fma_clock = 0.0625_dp ! This is CPU specific (Apple M1 number here)
freq = 3.2_dp*GHz
measured = t * freq / n**3
percent_peak = fma_clock / measured * 100
print *, "Time: ", t
print *, "Clock cycles per element:"
print *, "Theoretical performance peak:", fma_clock, "cycles"
print *, "Measured:                    ", measured, "cycles"
print *, "Percent peak:                ", percent_peak, "%"

print *
print *, "matmul7:"
call cpu_time(t1)
do i = 1, iter
    call matmul7(A, B, C2)
end do
call cpu_time(t2)
print *, "Error:", maxval(abs(C-C2))
t = (t2-t1)/iter
GHz = 1e9_dp
fma_clock = 0.0625_dp
freq = 3.2_dp*GHz
measured = t * freq / n**3
percent_peak = fma_clock / measured * 100
print *, "Time: ", t
print *, "Clock cycles per element:"
print *, "Theoretical performance peak:", fma_clock, "cycles"
print *, "Measured:                    ", measured, "cycles"
print *, "Percent peak:                ", percent_peak, "%"

print *
print *, "matmul1:"
call cpu_time(t1)
do i = 1, iter
    call matmul1(A, B, C2)
end do
call cpu_time(t2)
print *, "Error:", maxval(abs(C-C2))
t = (t2-t1)/iter
GHz = 1e9_dp
fma_clock = 0.0625_dp
freq = 3.2_dp*GHz
measured = t * freq / n**3
percent_peak = fma_clock / measured * 100
print *, "Time: ", t
print *, "Clock cycles per element:"
print *, "Theoretical performance peak:", fma_clock, "cycles"
print *, "Measured:                    ", measured, "cycles"
print *, "Percent peak:                ", percent_peak, "%"

end program
