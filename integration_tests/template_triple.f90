module Math_integer_m

    implicit none
    private

  contains

    pure function zero_integer() result(result)
      integer :: result
      result = 0
    end function

    pure function one_integer() result(result)
      integer :: result
      result = 1
    end function

  end module

  module Math_real_m

    implicit none
    private

  contains

    pure function zero_real() result(result)
      real :: result
      result = 0.0
    end function

    pure function one_real() result(result)
      real :: result
      result = 1.0
    end function

  end module

  module triple_m

    implicit none
    private
    public :: triple_tmpl

    requirement magma_r{T, plus_T}
      deferred type :: T

      pure function plus_T(l, r) result(total)
        type(T), intent(in) :: l, r
        type(T) :: total
      end function
    end requirement

    template triple_tmpl{T, plus_T}
      require :: magma_r{T, plus_T}
      private
      public :: triple_l, triple_r
    contains
      pure function triple_l(t) result(result)
        type(T), intent(in) :: t
        type(T) :: result
        result = plus_T(plus_T(t, t), t)
      end function

      pure function triple_r(t) result(result)
        type(T), intent(in) :: t
        type(T) :: result
        result = plus_T(t, plus_T(t, t))
      end function
    end template

  end module

  module use_triple_m

    use Math_integer_m
    use Math_real_m
    use triple_m

  contains

    subroutine test_add_triples()
      instantiate triple_tmpl{integer, operator(+)}, &
        only: triple_add_l => triple_l, &
              triple_add_r => triple_r
      integer :: tal, tar
      tal = triple_add_l(7)
      tar = triple_add_r(7)
      print *, "tal = ", tal, " tar = ", tar
    end subroutine

    subroutine test_minus_triples()
      instantiate triple_tmpl{real, operator(-)}, &
        only: triple_minus_l => triple_l, &
              triple_minus_r => triple_r
      real :: tml, tmr
      tml = triple_minus_l(7.0)
      tmr = triple_minus_r(7.0)
      print *, "tml = ", tml, " tmr = ", tmr
    end subroutine

    subroutine test_max_triples()
      instantiate triple_tmpl{real, max}, &
        only: triple_max_l => triple_l, &
              triple_max_r => triple_r
      real :: tmaxl, tmaxr
      tmaxl = triple_max_l(7.0)
      tmaxr = triple_max_r(7.0)
      print *, "tmaxl =", tmaxl, " tmaxr =", tmaxr
    end subroutine

  end module

  program template_triple
  use use_triple_m

  call test_add_triples()
  call test_minus_triples()
  call test_max_triples()

  end program template_triple
