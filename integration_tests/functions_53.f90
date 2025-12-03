program functions_53
    use iso_fortran_env
    implicit none
    INTEGER(int64) I,J,K,L,M,P5(145)
    integer(int64) :: temp
    integer :: loc

    DO, I=1,145
      P5(I)=I*I*I*I*I
    ! print ('(i5, i20, z20)'), i, p5(i), p5(i)
    end do

i_loop: DO, I = 1, 145
      if (mod (i, 10) == 0) print *, 'I iteration:', i
      DO, J = 1, I
        DO, K = 1, I
          DO, L = 1, I
            temp = -(P5(J) + P5(K) + P5(L) - p5(i))
#if 1
! Try various search functions
            ! loc = findloc (p5, temp, dim=1)
            loc = binsearch (p5, temp)
            if (loc /= 0) then
              m = loc
              exit i_loop
            end if
#else
! Inline search loop
            DO, M = 1, I
              if (temp == p5(m)) exit i_loop
            end do
#endif
          end do
        end do
      end do
    end do i_loop
    if (i <= 145) then
      PRINT 777
      PRINT 999, J,K,L,M,I
      IF(J /= 27 .AND. K /= 84 .AND. L /= 110 .AND. M /= 133 .AND. I /= 144) ERROR STOP
    end if

777 FORMAT('EULER CONJECTURE DISPROVED.')
999 FORMAT(5I10)

contains

!   Based on https://grokipedia.com/page/Binary_search_algorithm
pure integer function binsearch (a, val) result (res)
  integer(int64), intent(in) :: a(0:), val
  integer :: low, high, mid

  low = 0
  high = size (a) - 1
  do while (low <= high)
    mid = (low + high)/2 ! use floor ((low + high)/2) for type real
! Compare select case vs ifs
#if 1
    select case (a(mid) - val)
    case (:-1)
      low = mid + 1
    case (1:)
      high = mid - 1
    case default
      res = mid + 1
      return
    end select
#else
    if (val == a(mid)) then
      res = mid + 1
      return
    else if (val < a(mid)) then
      high = mid - 1
    else
      low = mid + 1
    end if
#endif
  end do
  res = 0

end function

END program 
