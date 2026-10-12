      program fixed_form_semicolon_01
      implicit none
      integer :: i, j, k, n, s; real :: x(3,3), y(3)
      character(len=10) :: t
      n = 3
      do i = 1, n
      do j = 1, n
      x(i,j) = 1.0
      end do; end do
      if (abs(sum(x) - 9.0) > 1e-6) error stop 1
      do i = 1, n
      do j = 1, n
      x(i,j) = 2.0
      enddo; enddo
      if (abs(sum(x) - 18.0) > 1e-6) error stop 2
      do i = 1, n
      y(i) = 0.0
      end do; y(1) = 1.0
      if (abs(sum(y) - 1.0) > 1e-6) error stop 3
      do i = 1, n
      y(i) = 3.0
      end do;
      if (abs(sum(y) - 9.0) > 1e-6) error stop 4
      do i = 1, n
      y(i) = 4.0
      ;end do
      if (abs(sum(y) - 12.0) > 1e-6) error stop 12
      do i = 1, n; y(i) = real(i); end do
      if (abs(sum(y) - 6.0) > 1e-6) error stop 5
      t = 'ab;c d'
      k = 0
      do i = 1, 9; if (t(i+1:i+1) .eq. ' ') exit; end do
      if (i /= 4) error stop 6
      s = 0
      do i = 1, n; do j = 1, n; s = s + 1; end do; end do
      if (s /= 9) error stop 7
      i = 0
      do while (i < 5); i = i + 1; end do
      if (i /= 5) error stop 8
      if (n == 3) then; k = 1; else; k = 2; end if
      if (k /= 1) error stop 9
      select case (n); case (3); k = 10
      case default; k = 20; end select
      if (k /= 10) error stop 10
      if (n == 3) then
      k = 11; end if; k = k + 1
      if (k /= 12) error stop 11
      k = 0
      do 10 i = 1, n
   10 k = k + 1; k = k + 100
      if (k /= 103) error stop 13
      k = 0
      do 20 i = 1, n
      k = k + 1
   20 continue; k = k + 100
      if (k /= 103) error stop 14
      y = 1.0
      where (y > 0.5); y = 2.0; end where
      if (abs(sum(y) - 6.0) > 1e-6) error stop 15
      k = 0
      if (n == 1) then; k = 1; else if (n == 3) then; k = 7; end if
      if (k /= 7) error stop 16
      y = 1.0; y(2) = 0.0
      where (y > 0.5); y = 2.0
      elsewhere; y = 3.0
      end where
      if (abs(sum(y) - 7.0) > 1e-6) error stop 17
      where (y > 2.5)
      y = 4.0
      elsewhere; y = 1.0; end where; y(1) = 5.0
      if (abs(sum(y) - 10.0) > 1e-6) error stop 18
      k = 0
      do 30 i = 1, n
   30 continue;; k = k + 1
      if (k /= 1) error stop 19
      ;
      ;;k = k + 1
      if (k /= 2) error stop 20
      print *, 'ok;', s; print *, k
      end program
