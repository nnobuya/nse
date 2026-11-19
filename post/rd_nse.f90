program rd_nse

  implicit none

  !..param.
  integer, parameter:: nset  = 6

  !..isotopes
  integer:: nel
  integer  , allocatable:: na(:), nz(:)
  real*8   , allocatable:: x(:)
  character, allocatable:: name(:)*14

  integer:: n_out
  integer, allocatable:: num_out(:)

  real*8 :: time, ye, rho, t9, ye_in, rho_in, t9_in, r_out

  integer:: i, ier


  !..open
  open(50, file = '../res/nse.dat', form = 'unformatted', action = 'read')
  open(60, file = './res/part.dat'  , action = 'write')
  open(61, file = './res/graph.dat' , action = 'write')
  open(62, file = './res/rhotye.dat', action = 'write')


  if     ( nset == 1 ) then
     n_out =  6  !! Timmes graph #1 *
  else if( nset == 2 ) then
     n_out = 10  !! Timmes graph #2 *
  else if( nset == 3 ) then
     n_out =  6  !! Odrzywolek FIG.3 *
  else if( nset == 4 ) then
     n_out =  6  !! Odrzywolek FIG.4 
  else if( nset == 5 ) then
     n_out = 10  !! Odrzywolek FIG.5 *
  else if( nset == 6 ) then
     n_out =  4  !! Seitenzahl FIG.3-7 *
  else if( nset == 7 ) then
     n_out = 17  !! Seitenzahl2008 FIG.2
  else if( nset == 8 ) then
     n_out = 18  !! Seitenzahl2008 FIG.3
  else
     stop 'error: You must set n_out!'
  end if

  allocate ( num_out(1:n_out), stat = ier )
  if( ier /= 0 ) stop 'error: allocation'


  read(50) nel

  allocate (na(nel), nz(nel), x(nel), name(nel), stat = ier)
  read(50) na(1:nel), nz(1:nel), name(1:nel)

  do i = 1, nel
     write(60,'(3i7,a14)') i, nz(i), na(i), adjustr(name(i))
  end do

  num_out(1:n_out) = 0

  call elem_set(n_out, nel, nset, name(:), num_out(:))
  !   in: n_out, nel, nset, name
  !  out: num_out


  if( minval(num_out(1:n_out)) == 0 ) stop 'error: no data'

  do
     read(50,iostat=ier) time, ye, rho, t9, ye_in, rho_in, t9_in, x(1:nel)
     if(ier /= 0) exit

     write(62,'(1p3e14.5)') rho, t9, ye


     if( nset == 1 ) then
        r_out = t9
     else if( nset == 2 .or. nset == 4 .or. nset == 5 &
          & .or. nset == 7 .or. nset == 8 ) then
        r_out = ye
     else if( nset == 3 ) then
        r_out = t9 /11.6045d0
     else if( nset == 6 ) then
        r_out = t9
     else
        stop 'error: no output'
     end if

     write(61,'(1p100e14.5)') r_out, ( x(num_out(i)), i = 1, n_out )

  end do


  close(50)


  stop 'rd_nse: normal termination'

end program rd_nse




subroutine elem_set(n_out, nel, nset, name, num_out)

  implicit none

  !..io
  integer  , intent(in) :: n_out, nel, nset
  character, intent(in) :: name(nel)*14
  integer  , intent(out):: num_out(n_out)

  !..local
  integer:: i


  do i = 1, nel
     if( nset == 1 ) then
        !! Timmes 1
        if( adjustl(name(i)) == 'n'    ) num_out(1)  = i
        if( adjustl(name(i)) == 'p'    ) num_out(2)  = i
        if( adjustl(name(i)) == 'He4'  ) num_out(3)  = i
        if( adjustl(name(i)) == 'Fe54' ) num_out(4)  = i
        if( adjustl(name(i)) == 'Fe56' ) num_out(5)  = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(6)  = i
     else if( nset == 2 ) then
        !! Timmes 2
        if( adjustl(name(i)) == 'Ti50' ) num_out(1)  = i
        if( adjustl(name(i)) == 'Cr52' ) num_out(2)  = i
        if( adjustl(name(i)) == 'Cr54' ) num_out(3)  = i
        if( adjustl(name(i)) == 'Fe54' ) num_out(4)  = i
        if( adjustl(name(i)) == 'Fe55' ) num_out(5)  = i
        if( adjustl(name(i)) == 'Fe56' ) num_out(6)  = i
        if( adjustl(name(i)) == 'Fe58' ) num_out(7)  = i
        if( adjustl(name(i)) == 'Co55' ) num_out(8)  = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(9)  = i
        if( adjustl(name(i)) == 'Ni58' ) num_out(10) = i
     else if( nset == 3 ) then
        !! Odrzywolek FIG.3
        if( adjustl(name(i)) == 'n'    ) num_out(1)  = i
        if( adjustl(name(i)) == 'p'    ) num_out(2)  = i
        if( adjustl(name(i)) == 'He4'  ) num_out(3)  = i
        if( adjustl(name(i)) == 'Ti50' ) num_out(4)  = i
        if( adjustl(name(i)) == 'Cr54' ) num_out(5)  = i
        if( adjustl(name(i)) == 'Fe58' ) num_out(6)  = i
     else if( nset == 4 ) then
        !! Odrzywolek FIG.4
        if( adjustl(name(i)) == 'n'    ) num_out(1)   = i
        if( adjustl(name(i)) == 'p'    ) num_out(2)   = i
        if( adjustl(name(i)) == 'Fe54' ) num_out(3)   = i
        if( adjustl(name(i)) == 'Co55' ) num_out(4)   = i
        if( adjustl(name(i)) == 'Fe56' ) num_out(5)   = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(6)   = i
        if( adjustl(name(i)) == 'Ni78' ) num_out(7)   = i
        if( adjustl(name(i)) == 'Zn80' ) num_out(8)   = i
        if( adjustl(name(i)) == 'Ge82' ) num_out(9)   = i
     else if( nset == 5 ) then
        !! Odrzywolek FIG.5
        if( adjustl(name(i)) == 'Cr54' ) num_out(1)  = i
        if( adjustl(name(i)) == 'Fe54' ) num_out(2)  = i
        if( adjustl(name(i)) == 'Co55' ) num_out(3)  = i
        if( adjustl(name(i)) == 'Fe56' ) num_out(4)  = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(5)  = i
        if( adjustl(name(i)) == 'Fe58' ) num_out(6)  = i
        if( adjustl(name(i)) == 'Fe60' ) num_out(7)  = i
        if( adjustl(name(i)) == 'Ni66' ) num_out(8)  = i
        if( adjustl(name(i)) == 'Ni68' ) num_out(9)  = i
        if( adjustl(name(i)) == 'Ge80' ) num_out(10) = i
     else if( nset == 6 ) then
        !! Seitenzahl
        if( adjustl(name(i)) == 'p'    ) num_out(1)  = i
        if( adjustl(name(i)) == 'Fe55' ) num_out(2)  = i
        if( adjustl(name(i)) == 'Co55' ) num_out(3)  = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(4)  = i
     else if( nset == 7 ) then
        !! Seitenzahl 2008
        if( adjustl(name(i)) == 'p'    ) num_out(1)   = i
        if( adjustl(name(i)) == 'n'    ) num_out(2)   = i
        if( adjustl(name(i)) == 'He4'  ) num_out(3)   = i
        if( adjustl(name(i)) == 'Si28' ) num_out(4)   = i
        if( adjustl(name(i)) == 'S32'  ) num_out(5)   = i
        if( adjustl(name(i)) == 'Cr52' ) num_out(6)   = i
        if( adjustl(name(i)) == 'Fe52' ) num_out(7)   = i
        if( adjustl(name(i)) == 'Fe54' ) num_out(8)   = i
        if( adjustl(name(i)) == 'Fe56' ) num_out(9)   = i
        if( adjustl(name(i)) == 'Fe58' ) num_out(10)  = i
        if( adjustl(name(i)) == 'Co55' ) num_out(11)  = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(12)  = i
        if( adjustl(name(i)) == 'Ni58' ) num_out(13)  = i
        if( adjustl(name(i)) == 'Ni64' ) num_out(14)  = i
        if( adjustl(name(i)) == 'Ni66' ) num_out(15)  = i
        if( adjustl(name(i)) == 'Ni68' ) num_out(16)  = i
        if( adjustl(name(i)) == 'Ni70' ) num_out(17)  = i
     else if( nset == 8 ) then
        !! Seitenzahl 2008
        if( adjustl(name(i)) == 'p'    ) num_out(1)   = i
        if( adjustl(name(i)) == 'n'    ) num_out(2)   = i
        if( adjustl(name(i)) == 'He4'  ) num_out(3)   = i
        if( adjustl(name(i)) == 'Cr52' ) num_out(4)   = i
        if( adjustl(name(i)) == 'Fe52' ) num_out(5)   = i
        if( adjustl(name(i)) == 'Fe54' ) num_out(6)   = i
        if( adjustl(name(i)) == 'Fe56' ) num_out(7)   = i
        if( adjustl(name(i)) == 'Fe58' ) num_out(8)   = i
        if( adjustl(name(i)) == 'Co53' ) num_out(9)   = i
        if( adjustl(name(i)) == 'Co55' ) num_out(10)  = i
        if( adjustl(name(i)) == 'Ni55' ) num_out(11)  = i
        if( adjustl(name(i)) == 'Ni56' ) num_out(12)  = i
        if( adjustl(name(i)) == 'Ni58' ) num_out(13)  = i
        if( adjustl(name(i)) == 'Ni64' ) num_out(14)  = i
        if( adjustl(name(i)) == 'Ni66' ) num_out(15)  = i
        if( adjustl(name(i)) == 'Ni68' ) num_out(16)  = i
        if( adjustl(name(i)) == 'Ni70' ) num_out(17)  = i
        if( adjustl(name(i)) == 'Cu57' ) num_out(18)  = i
     else
        stop 'error: bad elem set'
     end if
  end do


  return

end subroutine elem_set
