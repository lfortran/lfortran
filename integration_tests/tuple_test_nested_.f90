program test_tuple_nested

   implicit none
   call tuple_nested()

contains
   subroutine tuple_nested()

      _lfortran_tuple(integer, integer)                                                 :: t1
      _lfortran_tuple(integer, integer)                                                 :: t2
      _lfortran_tuple(_lfortran_tuple(integer, integer),_lfortran_tuple(integer, integer),  &
         _lfortran_tuple(integer, integer),_lfortran_tuple(integer, integer))           :: t3
      _lfortran_tuple(integer, real, character(len=:))                                  :: t4
      _lfortran_tuple(_lfortran_tuple(integer, real, character(len=:)), integer)        :: t5
      _lfortran_tuple(_lfortran_tuple(_lfortran_tuple(integer,                              &
         real, character(len=:)), integer), real)                                       :: t6
      _lfortran_tuple(_lfortran_tuple(integer, integer),                                    &
         _lfortran_tuple(_lfortran_list(integer), _lfortran_list(character(len=:))))    :: t7
      _lfortran_list(_lfortran_tuple(integer, real, _lfortran_tuple(real, integer)))    :: l1
      integer:: i
      character(len=:):: s
      t1 = _lfortran_tuple_constant(-1, -2)
      t2 = _lfortran_tuple_constant(-3, -4)
      t3 = _lfortran_tuple_constant(t1, t2, t1, t2)

      if ( .not. (_lfortran_eq(_lfortran_get_item(t3, 0), t1) .and. _lfortran_eq(_lfortran_get_item(t3, 1), t2))) error stop
      if ( .not. (_lfortran_eq(_lfortran_get_item(t3, 2), t1) .and. _lfortran_eq(_lfortran_get_item(t3, 3), t2))) error stop

      t4 = _lfortran_tuple_constant(1, 2.0, "abc")
      t5 = _lfortran_tuple_constant(t4, 3)
      if ( .not. _lfortran_eq(_lfortran_tuple_constant(_lfortran_tuple_constant(1, 2.0, "abc"), 3), t5) ) error stop
      t6 = _lfortran_tuple_constant(t5, 4.0)
      if ( .not. _lfortran_eq(_lfortran_tuple_constant(_lfortran_tuple_constant(t4, 3), 4.0), t6) ) error stop

      do i=0, 4
         call _lfortran_list_append(l1, _lfortran_tuple_constant(i, real(i+1), _lfortran_tuple_constant(real(i+2), i+3)))
      end do

      do i=0, 4
         if ( .not. _lfortran_eq(_lfortran_get_item(l1, i), &
            _lfortran_tuple_constant(i, real(i+1), _lfortran_tuple_constant(real(i+2), i+3))) ) error stop
      end do

      i = 3
      s = "a"
      t7 = _lfortran_tuple_constant(t1, _lfortran_tuple_constant(_lfortran_list_constant(i, i+1, i+2), &
         _lfortran_list_constant(s, repeat(s, 2), repeat(s, 3))))
      if ( .not. _lfortran_eq(t7, _lfortran_tuple_constant(_lfortran_tuple_constant(-1, -2), _lfortran_tuple_constant( &
         _lfortran_list_constant(3, 4, 5), _lfortran_list_constant('a', 'aa', 'aaa')))) ) error stop
   end subroutine


end program test_tuple_nested

