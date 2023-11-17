! This test is testing the code from:
! https://github.com/lfortran/lfortran/issues/2444

module cpu_matmul_02
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

    subroutine kernel6(A, B, C, x, y, i1, s1)
    real, intent(in) :: A(0:,0:), B(0:,0:)
    real, intent(out) :: C(0:,0:)
    integer, intent(in) :: x, y, i1, s1
    integer :: k
    real :: u(0:6-1,0:16-1) ! SIMD
    real, dimension(0:8-1) :: A0, A1, A2, A3, A4, A5 ! SIMD
    ! This function computes:
    !C(x:x+6-1, y:y+16-1) = C(x:x+6-1, y:y+16-1) + matmul( &
    !    A(x:x+6-1, i1:i1+s1-1), &
    !    B(i1:i1+s1-1, y:y+16-1) )
    u(0,0*8:0*8+8-1) = C(x+0,y+0*8:y+0*8+8-1)
    u(0,1*8:1*8+8-1) = C(x+0,y+1*8:y+1*8+8-1)

    u(1,0*8:0*8+8-1) = C(x+1,y+0*8:y+0*8+8-1)
    u(1,1*8:1*8+8-1) = C(x+1,y+1*8:y+1*8+8-1)

    u(2,0*8:0*8+8-1) = C(x+2,y+0*8:y+0*8+8-1)
    u(2,1*8:1*8+8-1) = C(x+2,y+1*8:y+1*8+8-1)

    u(3,0*8:0*8+8-1) = C(x+3,y+0*8:y+0*8+8-1)
    u(3,1*8:1*8+8-1) = C(x+3,y+1*8:y+1*8+8-1)

    u(4,0*8:0*8+8-1) = C(x+4,y+0*8:y+0*8+8-1)
    u(4,1*8:1*8+8-1) = C(x+4,y+1*8:y+1*8+8-1)

    u(5,0*8:0*8+8-1) = C(x+5,y+0*8:y+0*8+8-1)
    u(5,1*8:1*8+8-1) = C(x+5,y+1*8:y+1*8+8-1)

    do k = i1, i1+s1-1
        A0(:) = A(x+0,k)
        u(0, 0*8:0*8+8-1) = u(0, 0*8:0*8+8-1) + &
            A0(:) * B(k, y+0*8:y+0*8+8-1)
        u(0, 1*8:1*8+8-1) = u(0, 1*8:1*8+8-1) + &
            A0(:) * B(k, y+1*8:y+1*8+8-1)

        A1(:) = A(x+1,k)
        u(1, 0*8:0*8+8-1) = u(1, 0*8:0*8+8-1) + &
            A1(:) * B(k, y+0*8:y+0*8+8-1)
        u(1, 1*8:1*8+8-1) = u(1, 1*8:1*8+8-1) + &
            A1(:) * B(k, y+1*8:y+1*8+8-1)

        A2(:) = A(x+2,k)
        u(2, 0*8:0*8+8-1) = u(2, 0*8:0*8+8-1) + &
            A2(:) * B(k, y+0*8:y+0*8+8-1)
        u(2, 1*8:1*8+8-1) = u(2, 1*8:1*8+8-1) + &
            A2(:) * B(k, y+1*8:y+1*8+8-1)

        A3(:) = A(x+3,k)
        u(3, 0*8:0*8+8-1) = u(3, 0*8:0*8+8-1) + &
            A3(:) * B(k, y+0*8:y+0*8+8-1)
        u(3, 1*8:1*8+8-1) = u(3, 1*8:1*8+8-1) + &
            A3(:) * B(k, y+1*8:y+1*8+8-1)

        A4(:) = A(x+4,k)
        u(4, 0*8:0*8+8-1) = u(4, 0*8:0*8+8-1) + &
            A4(:) * B(k, y+0*8:y+0*8+8-1)
        u(4, 1*8:1*8+8-1) = u(4, 1*8:1*8+8-1) + &
            A4(:) * B(k, y+1*8:y+1*8+8-1)

        A5(:) = A(x+5,k)
        u(5, 0*8:0*8+8-1) = u(5, 0*8:0*8+8-1) + &
            A5(:) * B(k, y+0*8:y+0*8+8-1)
        u(5, 1*8:1*8+8-1) = u(5, 1*8:1*8+8-1) + &
            A5(:) * B(k, y+1*8:y+1*8+8-1)
    end do

    C(x+0,y+0*8:y+0*8+8-1) = u(0,0*8:0*8+8-1)
    C(x+0,y+1*8:y+1*8+8-1) = u(0,1*8:1*8+8-1)

    C(x+1,y+0*8:y+0*8+8-1) = u(1,0*8:0*8+8-1)
    C(x+1,y+1*8:y+1*8+8-1) = u(1,1*8:1*8+8-1)

    C(x+2,y+0*8:y+0*8+8-1) = u(2,0*8:0*8+8-1)
    C(x+2,y+1*8:y+1*8+8-1) = u(2,1*8:1*8+8-1)

    C(x+3,y+0*8:y+0*8+8-1) = u(3,0*8:0*8+8-1)
    C(x+3,y+1*8:y+1*8+8-1) = u(3,1*8:1*8+8-1)

    C(x+4,y+0*8:y+0*8+8-1) = u(4,0*8:0*8+8-1)
    C(x+4,y+1*8:y+1*8+8-1) = u(4,1*8:1*8+8-1)

    C(x+5,y+0*8:y+0*8+8-1) = u(5,0*8:0*8+8-1)
    C(x+5,y+1*8:y+1*8+8-1) = u(5,1*8:1*8+8-1)
    end subroutine

    subroutine kernel7(A, B, C, x, y, i1, s1)
    real, intent(in) :: A(0:,0:), B(0:,0:)
    real, intent(out) :: C(0:,0:)
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
    do i3 = 0, n-1, s3
    do i2 = 0, n-1, s2
    do i1 = 0, n-1, s1
        do x = i2, i2+s2-1, 6
        do y = i3, i3+s3-1, 16
            call kernel6(A, B, C, x, y, i1, s1)
        end do
        end do
    end do
    end do
    end do
    end subroutine

end module


program main
use cpu_matmul_02, only: matmul1, matmul7
implicit none

integer, parameter :: dp = kind(0.d0)
integer :: n, iter, i
real(dp) :: t1, t2, t, GHz, fma_clock, freq, measured, percent_peak
real, allocatable :: A(:,:), B(:,:), C(:,:), C2(:,:)
real :: err

! Use n=960 for a good benchmark
n = 96
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
print *, "matmul7:"
call cpu_time(t1)
do i = 1, iter
    call matmul7(A, B, C2)
end do
call cpu_time(t2)
err = maxval(abs(C-C2))
print *, "Error:", err
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
if (err > 1e-4) error stop

print *
print *, "matmul1:"
call cpu_time(t1)
do i = 1, iter
    call matmul1(A, B, C2)
end do
call cpu_time(t2)
err = maxval(abs(C-C2))
print *, "Error:", err
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
if (err > 1e-4) error stop

end program
