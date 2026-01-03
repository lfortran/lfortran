      program functions_24
      
      character a
      a = characterfunc()
      
      contains 
      
      character*1 function characterfunc()
      characterfunc = 'A'
      end function characterfunc
    
      end program
