! Type-bound non-elemental assignment(=) with scalar dummies (F2023):
!   - array / section / 2-D / scalar-to-array: intrinsic (assign_* not called)
!   - scalar: defined assignment
!   - extended type array: parent component via defined assignment + extension copy
!
! Free (module) interface assignment(=) is covered separately in
! defined_assignment_free_interface_01.f90 (cannot share a module with
! type-bound assignment(=) without breaking resolution).

module defined_assignment_01_mod
   implicit none

   type :: t
      integer :: v = 0
      logical :: hit = .false.
      integer, pointer :: leaf => null()
   contains
      procedure :: assign_t
      generic :: assignment(=) => assign_t
   end type t

   type :: parent_t
      integer :: v = 0
      logical :: hit = .false.
   contains
      procedure :: assign_parent
      generic :: assignment(=) => assign_parent
   end type parent_t

   type, extends(parent_t) :: child_t
      integer :: tag = 0
   end type child_t

contains

   subroutine assign_t(lhs, rhs)
      class(t), intent(inout) :: lhs
      class(t), intent(in)    :: rhs
      lhs%v = rhs%v
      lhs%hit = .true.
      lhs%leaf => rhs%leaf
   end subroutine assign_t

   subroutine assign_parent(lhs, rhs)
      class(parent_t), intent(inout) :: lhs
      class(parent_t), intent(in)    :: rhs
      lhs%v = rhs%v
      lhs%hit = .true.
   end subroutine assign_parent

end module defined_assignment_01_mod

program defined_assignment_01
   use defined_assignment_01_mod
   implicit none

   call test_plain_array()
   call test_plain_section_and_source()
   call test_rank2()
   call test_parent_child()
   call test_scalar_defined()

contains

   subroutine test_plain_array()
      type(t) :: a(2), b(2)
      integer :: i

      do i = 1, 2
         a(i)%v = i * 10
         a(i)%hit = .false.
         b(i)%hit = .false.
         nullify(b(i)%leaf)
      end do

      b = a
      if (b(1)%v /= 10 .or. b(2)%v /= 20) error stop 101
      if (any(b%hit)) error stop 102
   end subroutine test_plain_array

   subroutine test_plain_section_and_source()
      type(t), allocatable :: arr(:), copy(:), whole(:)
      type(t) :: scalar
      integer :: i

      allocate(arr(3))
      do i = 1, 3
         allocate(arr(i)%leaf)
         arr(i)%leaf = i * 100
         arr(i)%v = i
         arr(i)%hit = .false.
      end do

      ! Section assignment then source= of the result
      block
         type(t), allocatable :: out(:)
         allocate(out(size(arr)))
         out(1:size(arr)) = arr(1:size(arr))
         do i = 1, 3
            if (out(i)%v /= i) error stop 201
            if (out(i)%hit) error stop 202
            if (.not. associated(out(i)%leaf, arr(i)%leaf)) error stop 203
         end do
         allocate(copy, source=out)
      end block

      do i = 1, 3
         if (copy(i)%v /= i) error stop 204
         if (copy(i)%hit) error stop 205
         if (.not. associated(copy(i)%leaf, arr(i)%leaf)) error stop 206
      end do

      allocate(whole(3))
      do i = 1, 3
         whole(i)%hit = .true.
      end do
      whole = arr
      do i = 1, 3
         if (whole(i)%v /= i) error stop 207
         if (whole(i)%hit) error stop 208
         if (.not. associated(whole(i)%leaf, arr(i)%leaf)) error stop 209
      end do

      allocate(scalar%leaf)
      scalar%leaf = 42
      scalar%v = 42
      scalar%hit = .false.
      whole = scalar
      do i = 1, 3
         if (whole(i)%v /= 42) error stop 210
         if (whole(i)%hit) error stop 211
         if (.not. associated(whole(i)%leaf, scalar%leaf)) error stop 212
      end do

      do i = 1, 3
         deallocate(arr(i)%leaf)
      end do
      deallocate(scalar%leaf)
      deallocate(arr, copy, whole)
   end subroutine test_plain_section_and_source

   subroutine test_rank2()
      type(t) :: src(2, 3), dst(2, 3), scalar
      integer :: i, j

      do j = 1, 3
         do i = 1, 2
            src(i, j)%v = 10 * i + j
            src(i, j)%hit = .false.
            dst(i, j)%hit = .true.
         end do
      end do

      dst = src
      do j = 1, 3
         do i = 1, 2
            if (dst(i, j)%v /= 10 * i + j) error stop 301
            if (dst(i, j)%hit) error stop 302
         end do
      end do

      do j = 1, 3
         do i = 1, 2
            dst(i, j)%hit = .true.
         end do
      end do
      dst(1:2, 1:3) = src(1:2, 1:3)
      do j = 1, 3
         do i = 1, 2
            if (dst(i, j)%v /= 10 * i + j) error stop 303
            if (dst(i, j)%hit) error stop 304
         end do
      end do

      scalar%v = 99
      scalar%hit = .false.
      dst = scalar
      do j = 1, 3
         do i = 1, 2
            if (dst(i, j)%v /= 99) error stop 305
            if (dst(i, j)%hit) error stop 306
         end do
      end do
   end subroutine test_rank2

   subroutine test_parent_child()
      type(child_t) :: a(2), b(2)
      integer :: i

      do i = 1, 2
         a(i)%v = i * 10
         a(i)%hit = .false.
         a(i)%tag = i
         b(i)%hit = .false.
         b(i)%tag = -1
      end do

      ! Array: parent component via defined assignment; tag copied
      b = a
      do i = 1, 2
         if (b(i)%v /= i * 10) error stop 401
         if (.not. b(i)%hit) error stop 402
         if (b(i)%tag /= i) error stop 403
      end do

      do i = 1, 2
         b(i)%hit = .false.
         b(i)%tag = -1
      end do
      b(1:2) = a(1:2)
      do i = 1, 2
         if (b(i)%v /= i * 10) error stop 404
         if (.not. b(i)%hit) error stop 405
         if (b(i)%tag /= i) error stop 406
      end do

      ! Scalar: defined assignment only (tag untouched)
      b(1)%hit = .false.
      b(1)%tag = -99
      b(1) = a(2)
      if (b(1)%v /= 20) error stop 407
      if (.not. b(1)%hit) error stop 408
      if (b(1)%tag /= -99) error stop 409
   end subroutine test_parent_child

   subroutine test_scalar_defined()
      type(t) :: x, y
      allocate(y%leaf)
      y%leaf = 7
      y%v = 7
      y%hit = .false.
      x%hit = .false.
      nullify(x%leaf)

      x = y
      if (x%v /= 7) error stop 501
      if (.not. x%hit) error stop 502
      if (.not. associated(x%leaf, y%leaf)) error stop 503
      deallocate(y%leaf)
   end subroutine test_scalar_defined

end program defined_assignment_01
