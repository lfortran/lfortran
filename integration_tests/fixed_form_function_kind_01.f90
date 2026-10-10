      real(8) function f1()
      f1 = 1.5_8
      end function

      real(kind=8) function f2(x)
      real(8), intent(in) :: x
      f2 = 2*x
      end function

      integer(4) recursive function fact(n) result(r)
      integer, intent(in) :: n
      if (n <= 1) then
         r = 1
      else
         r = n*fact(n - 1)
      end if
      end function

      integer*4 recursive function fact4(n) result(r)
      integer, intent(in) :: n
      if (n <= 1) then
         r = 1
      else
         r = n*fact4(n - 1)
      end if
      end function

      character(32) function greet()
      greet = 'hello'
      end function

      pure logical(kind=4) function is_pos(n)
      integer, intent(in) :: n
      is_pos = n > 0
      end function

      program fixed_form_function_kind_01
      implicit none
      real(8), external :: f1, f2
      integer(4), external :: fact
      integer*4, external :: fact4
      character(32), external :: greet
      logical(4), external :: is_pos
      print *, f1(), f2(3.0_8), fact(5), trim(greet()), is_pos(3)
      if (abs(f1() - 1.5_8) > 1e-12_8) error stop
      if (abs(f2(3.0_8) - 6.0_8) > 1e-12_8) error stop
      if (fact(5) /= 120) error stop
      if (fact4(6) /= 720) error stop
      if (greet() /= 'hello') error stop
      if (.not. is_pos(3)) error stop
      if (is_pos(-1)) error stop
      end program
