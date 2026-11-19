program ch_nse

  implicit none

  integer:: nel
  integer, allocatable:: na(:), nz(:)
  real*8 , allocatable:: x(:)

  real*8 :: time, ye, rho, t9, ye_in, rho_in, t9_in
  integer:: nout = 0

  !..tmp
  integer:: i, ierr


  !..open
  open(50, file = '../res/nse.dat', form = 'unformatted', action = 'read')

  open(60, file = './out.dat', action = 'write')


  read(50) nel

  allocate (na(nel), nz(nel), x(nel), stat = ierr)
  if(ierr /= 0) stop 'error: allocation'

  read(50) na(1:nel), nz(1:nel)

  do

     nout = nout + 1

     read(50,iostat=ierr) time, ye, rho, t9, ye_in, rho_in, t9_in, x(1:nel)

     if(ierr /= 0) exit

     !..output

     do i = 1, nel
        write(60,'(3i5,1p2e15.5)') &
             & na(i), nz(i), na(i) - nz(i), x(i), x(i) /dble(na(i))
     end do
     write(60,*)
     write(60,*)


  end do

  close(50)

  write(*,*) nout


  stop

end program ch_nse
