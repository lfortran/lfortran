program list_of_tuples_test

   implicit none
   _lfortran_list(_lfortran_tuple(character(len=:), integer, real)) :: students

   call _lfortran_list_append(students, _lfortran_tuple_constant("Alice", 10, 99.3))
   call _lfortran_list_append(students, _lfortran_tuple_constant("Bob", 7, 95.3))
   call _lfortran_list_append(students, _lfortran_tuple_constant("Charlie", 9, 96.3))

   if ( _lfortran_get_item(_lfortran_get_item(students, 1), 0) /= "Bob" ) error stop
end program list_of_tuples_test

