program dump_nto
  implicit none
  integer :: i,j, istate

  print *,  'Phi_S'
  do i=2,N_states
    print *,  i, Phi_S(i)
  enddo

  do istate=2,N_states
    do j=1,mo_tot_num
      print *,  'MO: ', j, 'State:', istate, 'Eig:', one_body_dm_mo_diff_eigvalues(j,istate)
      do i=1,ao_num
        print *,  i, transition_natorb(i,j,istate)
      enddo
    enddo
  enddo

end
