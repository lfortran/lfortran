program character_01
   CHARACTER A*4, B*4, C(2)*3
   CHARACTER, parameter :: D*3 = "ape"
   A = 'ABCD'
   B = 'EFGH'
   C(1) = 'IJK'
   C(2) = 'LMN'

   print *, A, B, C(1), C(2)
   print *, D
end program
