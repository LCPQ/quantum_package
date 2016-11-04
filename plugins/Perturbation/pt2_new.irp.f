subroutine i_H_psi_pert_new_minilist(key,keys,idx_key,N_minilist,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array,coef_pert)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate,idx_key(Ndet), N_minilist
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  double precision, intent(in)   :: coef(Ndet_max,Nstate)
  double precision, intent(out)  :: i_H_psi_array(Nstate)
  double precision, intent(out)  :: coef_pert

  integer                        :: idx(0:Ndet)
  
  integer                        :: i, ii,j, i_in_key, i_in_coef
  double precision               :: phase
  integer                        :: exc(0:2,2,2)
  double precision               :: hij
  double precision :: delta_e_final
  double precision :: hjj
  BEGIN_DOC
! Computes <i|H|Psi> = \sum_J c_J <i|H|J>.
!
! Uses filter_connected_i_H_psi0 to get all the |J> to which |i>
! is connected. The |J> are searched in short pre-computed lists.
  END_DOC
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  coef_pert = 0.d0
  
  call filter_connected_i_H_psi0(keys,key,Nint,N_minilist,idx)
  double precision :: coef_array(Nstate)
  if (Nstate == 1) then

    do ii=1,idx(0)
      i_in_key = idx(ii)
      i_in_coef = idx_key(idx(ii))
      !DIR$ FORCEINLINE
      call i_H_j(keys(1,1,i_in_key),key,Nint,hij)
      i_H_psi_array(1) = i_H_psi_array(1) + coef(i_in_coef,1)*hij
      do i = 1, Nstate
       coef_array(i) = coef(i_in_coef,i)
      enddo
      call get_delta_e_dyall(keys(1,1,i_in_key),key,coef_array,hij,delta_e_final)
       
      coef_pert +=  coef(i_in_coef,1)*hij / delta_e_final
    enddo
    if     (coef_pert * i_H_psi_array(1) > 0.d0)then
      print*, coef_pert * i_H_psi_array(1)
    endif

  else

    do ii=1,idx(0)
      i_in_key = idx(ii)
      i_in_coef = idx_key(idx(ii))
      !DIR$ FORCEINLINE
      call i_H_j(keys(1,1,i_in_key),key,Nint,hij)
      i_H_psi_array(1) = i_H_psi_array(1) + coef(i_in_coef,1)*hij
      do j = 1, Nstate
        i_H_psi_array(j) = i_H_psi_array(j) + coef(i_in_coef,j)*hij
      enddo
    enddo

  endif

end

