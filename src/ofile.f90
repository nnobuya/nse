subroutine ofile

  use mod_set, only: set_init, n_in

  implicit none

  character(len=:), allocatable :: input_dir, output_dir
  logical :: local_input

  ! Allow execution from either the project root or the src directory.
  inquire(file='in/nse.in', exist=local_input)
  if (local_input) then
     input_dir = 'in/'
     output_dir = 'res/'
  else
     input_dir = '../in/'
     output_dir = '../res/'
  end if

  ! ------------------------------------------------------------------ !
  !     input                                                          !
  ! ------------------------------------------------------------------ !

  open(15,file=input_dir//'nse.in', action = 'read')
  call set_init(15)

  !open(40,file=input_dir//'dat/part.fz471' , action = 'read')
  open(40,file=input_dir//'dat/part.ame.fz4421', action = 'read')

  !!e- e+ capture rate
  !!T9(0.01-100), lrho(1-11)
  !open(42,file=input_dir//'ntwk/ecap.z219', action='read')
  !open(43,file=input_dir//'ntwk/pcap.z219', action='read')

  open(42,file=input_dir//'ntwk/ecap.z3859', action='read')
  open(43,file=input_dir//'ntwk/pcap.z3859', action='read')


  !! rhot table
  if( n_in == 0 ) then
     open(51,file=input_dir//'hydro.in', action = 'read')
  else if( n_in == 1 ) then
     open(51,file=input_dir//'table.in', action = 'read')
  else
     stop 'ofile(): error bad n_in'
  end if


  open(52, file = input_dir//'pns.neu.dat', action = 'read')

  !     input                                                          !
  ! ------------------------------------------------------------------ !


  ! ------------------------------------------------------------------ !
  !     output                                                         !
  ! ------------------------------------------------------------------ !

  open(60,file=output_dir//'nse.log'  , action = 'write')
  open(61,file=output_dir//'hydro.out', action = 'write')
  open(62,file=output_dir//'abund.out', action = 'write')
  open(63,file=output_dir//'flow.out' , action = 'write')
  open(64,file=output_dir//'x_end.dat', action = 'write')
  open(65,file=output_dir//'nse.tbl'  , action = 'write')
  open(66,file=output_dir//'nse.dat'  , action = 'write', form = 'unformatted' )

  !..check
  !open(70,file=output_dir//'converge.log', form = 'unformatted', action = 'write')
  open(70,file=output_dir//'converge.log', action = 'write')
  open(71,file=output_dir//'no_converge.log', action = 'write')

  !     output                                                         !
  ! ------------------------------------------------------------------ !

  return

end subroutine ofile
