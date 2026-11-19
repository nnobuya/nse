program wr_xa

  implicit none

  integer:: nel, n_ye, n_rho, n_t9
  integer, allocatable:: na(:), nz(:)
  real*8 , allocatable:: x(:)

  real*8 :: time, ye, rho, t9, ye_in, rho_in, t9_in
  integer:: nout = 0

  !..tmp
  integer:: i, j, k, ierr


  !..open
  open(50, file = '../res/nse.dat'    , form = 'unformatted', action = 'read')
  open(51, file = '../in/table_in.dat', action = 'read')

  open(60, file = './xa.dat', action = 'write')


  read(51,*)
  read(51,*) n_ye, n_rho, n_t9
  read(51,*)
  close(51)

  read(50) nel
  allocate (na(1:nel), nz(1:nel), x(1:nel), stat = ierr)
  if(ierr /= 0) stop 'error: allocation'

  read(50) na(1:nel), nz(1:nel)


  ye_lp: do k = 1, n_ye
     rho_lp: do j = 1, n_rho
        t9_lp: do i = 1, n_t9
           read(50) time, ye, rho, t9, ye_in, rho_in, t9_in, x(1:nel)

           write(60,'(1p10e14.5)') &
                & ye, rho, t9, log10(1.d0 - x(1) - x(2) - x(6))

        end do t9_lp

        write(60,*)
        write(60,*)

     end do rho_lp
  end do ye_lp


  close(50)


  stop

end program wr_xa
