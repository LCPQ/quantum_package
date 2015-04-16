Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ao_mono_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L1>`_
  array of the mono electronic hamiltonian on the AOs basis
  : sum of the kinetic and nuclear electronic potential

`check_ortho <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/check_orthonormality.irp.f#L1>`_
  Undocumented

`do_print <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/check_orthonormality.irp.f#L11>`_
  Undocumented

`n_pt_max_i_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/dimensions.irp.f#L2>`_
  Undocumented

`n_pt_max_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/dimensions.irp.f#L1>`_
  Undocumented

`ao_deriv2_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L1>`_
  second derivatives matrix elements in the ao basis
  .. math::
  .br
  {\tt ao_deriv2_x} = \langle \chi_i(x,y,z) \frac{\partial^2}{\partial x^2} |\chi_j (x,y,z) \rangle

`ao_deriv2_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L2>`_
  second derivatives matrix elements in the ao basis
  .. math::
  .br
  {\tt ao_deriv2_x} = \langle \chi_i(x,y,z) \frac{\partial^2}{\partial x^2} |\chi_j (x,y,z) \rangle

`ao_deriv2_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L3>`_
  second derivatives matrix elements in the ao basis
  .. math::
  .br
  {\tt ao_deriv2_x} = \langle \chi_i(x,y,z) \frac{\partial^2}{\partial x^2} |\chi_j (x,y,z) \rangle

`ao_kinetic_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L125>`_
  array of the priminitve basis kinetic integrals
  \langle \chi_i |\hat{T}| \chi_j \rangle

`mo_kinetic_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_mo_ints.irp.f#L1>`_
  Undocumented

`mo_mono_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/mo_mono_ints.irp.f#L1>`_
  array of the mono electronic hamiltonian on the MOs basis
  : sum of the kinetic and nuclear electronic potential

`a_coef <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L252>`_
  Undocumented

`b_coef <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L257>`_
  Undocumented

`ddfact2 <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L243>`_
  Undocumented

`erf0 <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L105>`_
  Undocumented

`gammln <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L271>`_
  Undocumented

`gammp <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L133>`_
  Undocumented

`gcf <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L211>`_
  Undocumented

`gser <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L167>`_
  Undocumented

`rinteg <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L47>`_
  Undocumented

`rintgauss <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L31>`_
  Undocumented

`sabpartial <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/need.irp.f#L2>`_
  Undocumented

`orthonormalize_mos <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/orthonormalize.irp.f#L1>`_
  Undocumented

`ao_nucl_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L1>`_
  interaction nuclear electron

`ao_nucl_elec_integral_per_atom <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L199>`_
  ao_nucl_elec_integral_per_atom(i,j,k) = -<AO(i)|1/|r-Rk|AO(j)>
  where Rk is the geometry of the kth atom

`give_polynom_mult_center_mono_elec <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L346>`_
  Undocumented

`i_x1_pol_mult_mono_elec <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L474>`_
  Undocumented

`i_x2_pol_mult_mono_elec <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L545>`_
  Undocumented

`int_gaus_pol <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L616>`_
  Undocumented

`nai_pol_mult <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L267>`_
  Undocumented

`v_e_n <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L597>`_
  Undocumented

`v_phi <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L661>`_
  Undocumented

`v_r <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L645>`_
  Undocumented

`v_theta <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L674>`_
  Undocumented

`wallis <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L690>`_
  Undocumented

`mo_nucl_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_mo_ints.irp.f#L1>`_
  interaction nuclear electron on the MO basis

`mo_nucl_elec_integral_per_atom <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_mo_ints.irp.f#L30>`_
  mo_nucl_elec_integral_per_atom(i,j,k) = -<MO(i)|1/|r-Rk|MO(j)>
  where Rk is the geometry of the kth atom

`save_ortho_mos <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/save_ortho_mos.irp.f#L1>`_
  Undocumented

`ao_deriv_1_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L148>`_
  array of the integrals of AO_i * d/dx  AO_j
  array of the integrals of AO_i * d/dy  AO_j
  array of the integrals of AO_i * d/dz  AO_j

`ao_deriv_1_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L149>`_
  array of the integrals of AO_i * d/dx  AO_j
  array of the integrals of AO_i * d/dy  AO_j
  array of the integrals of AO_i * d/dz  AO_j

`ao_deriv_1_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L150>`_
  array of the integrals of AO_i * d/dx  AO_j
  array of the integrals of AO_i * d/dy  AO_j
  array of the integrals of AO_i * d/dz  AO_j

`ao_dipole_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L75>`_
  array of the integrals of AO_i * x AO_j
  array of the integrals of AO_i * y AO_j
  array of the integrals of AO_i * z AO_j

`ao_dipole_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L76>`_
  array of the integrals of AO_i * x AO_j
  array of the integrals of AO_i * y AO_j
  array of the integrals of AO_i * z AO_j

`ao_dipole_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L77>`_
  array of the integrals of AO_i * x AO_j
  array of the integrals of AO_i * y AO_j
  array of the integrals of AO_i * z AO_j

`ao_spread_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L1>`_
  array of the integrals of AO_i * x^2 AO_j
  array of the integrals of AO_i * y^2 AO_j
  array of the integrals of AO_i * z^2 AO_j

`ao_spread_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L2>`_
  array of the integrals of AO_i * x^2 AO_j
  array of the integrals of AO_i * y^2 AO_j
  array of the integrals of AO_i * z^2 AO_j

`ao_spread_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L3>`_
  array of the integrals of AO_i * x^2 AO_j
  array of the integrals of AO_i * y^2 AO_j
  array of the integrals of AO_i * z^2 AO_j

`overlap_bourrin_deriv_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L365>`_
  Undocumented

`overlap_bourrin_dipole <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L318>`_
  Undocumented

`overlap_bourrin_spread <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L265>`_
  Undocumented

`overlap_bourrin_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L380>`_
  Undocumented

`overlap_bourrin_x_abs <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L226>`_
  Undocumented

`power <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_ao.irp.f#L310>`_
  Undocumented

`mo_deriv_1_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L69>`_
  array of the integrals of MO_i * d/dx  MO_j
  array of the integrals of MO_i * d/dy  MO_j
  array of the integrals of MO_i * d/dz  MO_j

`mo_deriv_1_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L70>`_
  array of the integrals of MO_i * d/dx  MO_j
  array of the integrals of MO_i * d/dy  MO_j
  array of the integrals of MO_i * d/dz  MO_j

`mo_deriv_1_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L71>`_
  array of the integrals of MO_i * d/dx  MO_j
  array of the integrals of MO_i * d/dy  MO_j
  array of the integrals of MO_i * d/dz  MO_j

`mo_dipole_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L1>`_
  array of the integrals of MO_i * x MO_j
  array of the integrals of MO_i * y MO_j
  array of the integrals of MO_i * z MO_j

`mo_dipole_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L2>`_
  array of the integrals of MO_i * x MO_j
  array of the integrals of MO_i * y MO_j
  array of the integrals of MO_i * z MO_j

`mo_dipole_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L3>`_
  array of the integrals of MO_i * x MO_j
  array of the integrals of MO_i * y MO_j
  array of the integrals of MO_i * z MO_j

`mo_spread_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L36>`_
  array of the integrals of MO_i * x^2 MO_j
  array of the integrals of MO_i * y^2 MO_j
  array of the integrals of MO_i * z^2 MO_j

`mo_spread_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L37>`_
  array of the integrals of MO_i * x^2 MO_j
  array of the integrals of MO_i * y^2 MO_j
  array of the integrals of MO_i * z^2 MO_j

`mo_spread_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/spread_dipole_mo.irp.f#L38>`_
  array of the integrals of MO_i * x^2 MO_j
  array of the integrals of MO_i * y^2 MO_j
  array of the integrals of MO_i * z^2 MO_j

`compute_integrals_pseudo <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/test_michel.irp.f#L58>`_
  Undocumented



