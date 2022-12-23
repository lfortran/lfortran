subroutine xerror(mess)
      character*(*) mess !This is string whose length is decided at compile time
      print *, mess(1)
      return
END
