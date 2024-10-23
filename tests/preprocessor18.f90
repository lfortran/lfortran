program preprocessor18
#if (defined __flang__ && __flang_major__ <= 19)
integer,parameter :: x = 0
#else
integer,parameter :: y = 0
#endif
end program