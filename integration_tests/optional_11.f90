program optional_11
  implicit none
  integer :: r

  ! Test 1: absent optional forwarded through a block
  call outer(r=r)
  if (r /= 0) error stop "Test 1 failed: present(x) should be false"

  ! Test 2: present optional forwarded through a block
  call outer(42, r)
  if (r /= 1) error stop "Test 2 failed: present(x) should be true"

  ! Test 3: absent optional forwarded through nested blocks
  call outer_nested(r=r)
  if (r /= 0) error stop "Test 3 failed: present(x) should be false (nested)"

  ! Test 4: present optional forwarded through nested blocks
  call outer_nested(7, r)
  if (r /= 1) error stop "Test 4 failed: present(x) should be true (nested)"

  print *, "All tests passed"
contains
  subroutine inner(x, r)
    integer, intent(in), optional :: x
    integer, intent(out) :: r
    if (present(x)) then
      r = 1
    else
      r = 0
    end if
  end subroutine

  subroutine outer(x, r)
    integer, intent(in), optional :: x
    integer, intent(out) :: r
    block
      call inner(x, r)
    end block
  end subroutine

  subroutine outer_nested(x, r)
    integer, intent(in), optional :: x
    integer, intent(out) :: r
    block
      block
        call inner(x, r)
      end block
    end block
  end subroutine
end program
