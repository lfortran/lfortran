      program fixed_form_io_keyword_01
      integer printp, readi, writej, closek
      common /rkcom3/ printp
      save /rkcom3/
      printp = 1
      readi = 2
      writej = 3
      closek = 4
      if (printp /= 1) error stop
      if (readi /= 2) error stop
      if (writej /= 3) error stop
      if (closek /= 4) error stop
      print *, printp, readi, writej, closek
      end
