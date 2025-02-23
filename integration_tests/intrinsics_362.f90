program intrinsics_362
    implicit none
    integer :: a(10)
    integer :: b(5, 3)
    a = [1,2,3,4,5,6,7,8,9,10]
    print *, is_contiguous(b(1:, 1:))
    if (is_contiguous(b(1:, 1:)) .neqv. .true.) error stop
    print *, is_contiguous(a(::2))
    if (is_contiguous(a(::2)) .neqv. .false.) error stop
    print *, is_contiguous(b(1:2, 1:2))
    if (is_contiguous(b(1:2, 1:2)) .neqv. .false.) error stop
    print *, is_contiguous(b(1:2, 1:2:2))
    if (is_contiguous(b(1:2, 1:2:2)) .neqv. .false.) error stop
    print *, is_contiguous(b)
    if (is_contiguous(b) .neqv. .true.) error stop
    call sub(a(::2))
    call sub(a)
    call sub(a(::1))
    call sub(a(::-1))
    call sub(a(1:9))
  contains
    subroutine sub (x)
      integer :: x(:)
      if (is_contiguous (x)) then
        print *, 'X is contiguous'
      else
        print *, 'X is not contiguous'
      end if
    end subroutine sub
end program