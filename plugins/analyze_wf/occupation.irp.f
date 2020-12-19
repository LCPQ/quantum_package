subroutine get_occupation_from_dets(istate,occupation)
  implicit none
  double precision, intent(out)  :: occupation(mo_tot_num)
  integer, intent(in)            :: istate
  BEGIN_DOC
  ! Returns the average occupation of the MOs
  END_DOC
  integer                        :: i,j, ispin
  integer                        :: list(N_int*bit_kind_size,2)
  integer                        :: n_elements(2)
  double precision               :: c, norm_2
  ASSERT (istate > 0) 
  ASSERT (istate <= N_states) 
  
  occupation = 0.d0
  double precision, external :: u_dot_u

  norm_2 = 1.d0/u_dot_u(psi_coef(1,istate),N_det)

  do i=1,N_det
    c = psi_coef(i,istate)*psi_coef(i,istate)*norm_2
    call bitstring_to_list_ab(psi_det(1,1,i), list, n_elements, N_int)
    do ispin=1,2
      do j=1,n_elements(ispin)
        ASSERT ( list(j,ispin) < mo_tot_num ) 
        occupation( list(j,ispin) ) += c
      enddo
    enddo
  enddo
end
