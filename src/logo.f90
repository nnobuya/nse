subroutine logo

  implicit none

    write(*,*)
    write(*,*) &
         & '========================================&
         &====================================='
    write(*,*)
    write(*,'( 20x, "    N       N     S S S     E E E" )')
    write(*,'( 20x, "   N N     N     S         E     " )')
    write(*,'( 20x, "  N   N   N       S       E E E  " )')
    write(*,'( 20x, " N     N N         S     E       " )')
    write(*,'( 20x, "N       N     S S S     E E E    " )')
    write(*,*)
    write(*,'( 50x, "Coded by Nobuya NISHIMURA" )')
    write(*,*) &
         & '========================================&
         &====================================='

  return

end subroutine logo
