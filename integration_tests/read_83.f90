program read_83
   implicit none

   character(len=32) :: empty_read
   integer :: unit_num

   call get_command_argument(1, empty_read)

   if (trim(empty_read) == '--empty-read') then
      print *, "Starting empty read. All input to stdin will be discarded."
      read *
      print *, "Empty read 1 completed successfully. Starting empty read 2 with unit=5 (stdin), fmt='(I10)'."
      read (5, "(I10)")
      print *, "Empty read 2 completed successfully. Starting empty read 3 with unit=*, fmt=*."
      read (*, *)
      print *, "Empty read 3 completed successfully. Starting empty read 4 with format label."
      36  format(I10)
      read 36
      print *, "Empty read 4 completed successfully. Starting empty read 5 with unit passed through variable."
      unit_num = 5
      read (unit_num, "(I10)")
      print *, "Empty read 5 completed successfully. All empty reads completed without error."
   end if
end program
