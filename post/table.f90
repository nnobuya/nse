program table

  implicit none


  integer:: n_ye, n_temp, n_dens
  real*8 :: ye, t9, dens, z, a, q, de_nu, de_nu_bar, dye_p, dye_m
  real*8 :: ye0 = 0.d0

  integer:: i, j, k
  character:: ofile*50

  open( 50, file = '../res/nse.table', action = 'read' )


  read(50,*)
  read(50,*) n_ye, n_temp, n_dens
  read(50,*)


  ye_lp: do k = 1, n_ye
     write(*,*) k, n_ye

     temp_lp: do j = 1, n_temp
        dens_lp: do i = 1, n_dens
           read(50,*) &
                & ye, t9, dens, z, a, q, de_nu, de_nu_bar, dye_p, dye_m

           if( ye /= ye0 ) then

              close(60)

              write(ofile,'("./res/table_cut/nse.ye",f6.3,".table")') ye
              open(60, file = ofile, status ='replace', action ='write')

              write(60,'("#",4x,"ye =",f10.3)') ye

              ye0 = ye
           end if

           write(60,'(1p9e13.5)') &
                & log10(t9) + 9.d0, &
                & dens, z, a, q, de_nu, de_nu_bar, dye_p, dye_m

        end do dens_lp
        write(60,*)
     end do temp_lp
  end do ye_lp

  close(50)


  stop 'normal termination'

end program table
