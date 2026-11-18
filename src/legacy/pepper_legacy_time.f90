!> The legacy interface's routines of the subsystem time (21 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_time

   use pepper_instance_module, only: pepper_context, pepper_zzcvpool => zzcvpool, &
      pepper_zzutcpm => zzutcpm, pepper_zzvststr => zzvststr, pepper_zzvsbstr => zzvsbstr, &
      pepper_zzprscor => zzprscor, pepper_zzvalcor => zzvalcor, pepper_zzcorepc => zzcorepc
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: mxcomp

   implicit none

   private
   public :: badkpv, prsint, convrt, zzcvpool, zzutcpm, zzvststr, zzvsbstr, dpfmt, dpstrf, repmf, &
      jul2gr, gr2jul, etcal, ttrans, timdef, unitim, str2et, zzprscor, zzvalcor, zzcorepc, &
      zzvstrng

contains

      !> Determine if a kernel pool variable is present and if so
      !> that it has the correct size and type.
      !>
      !> Legacy interface: calls ctx%badkpv with the legacy context.
      logical function badkpv ( caller, name,  comp, &
                                size,   divby, type )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: caller  !! Name of the routine calling this routine.
      character(*) :: name  !! Name of a kernel pool variable.
      character(*) :: comp  !! Comparison operator.
      integer(ip) :: size  !! Expected size of the kernel pool variable.
      integer(ip) :: divby  !! A divisor of the size of the kernel pool variable.
      character(*) :: type  !! Expected type of the kernel pool variable.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      badkpv = ctx%badkpv ( caller, name, comp, size, divby, type )
      end function badkpv

      !> Parse a string as an integer, encapsulating error handling.
      !>
      !> Legacy interface: calls ctx%prsint with the legacy context.
      subroutine prsint ( string, intval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String representing a numeric value.
      integer(ip) :: intval  !! Integer value obtained by parsing STRING.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%prsint ( string, intval )
      end subroutine prsint

      !> Take a measurement X, the units associated with
      !> X, and units to which X should be converted; return Y ---
      !> the value of the measurement in the output units.
      !>
      !> Legacy interface: calls ctx%convrt with the legacy context.
      subroutine convrt ( x, in, out, y )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpr
      implicit none
      real(wp) :: x  !! Number representing a measurement in some units.
      character(*) :: in  !! The units in which X is measured.
      character(*) :: out  !! Desired units for the measurement.
      real(wp) :: y  !! The measurement in the desired units.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%convrt ( x, in, out, y )
      end subroutine convrt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcvpool with the legacy context.
      subroutine zzcvpool ( agent, usrctr, update )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      character(*) :: agent  !! Name of the agent to check for notices
      integer(ip) :: usrctr    ( ctrsiz )  !! POOL state counter tracked by the caller
      logical :: update  !! .TRUE. if variables for AGENT have been updated
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzcvpool ( ctx, agent, usrctr, update )
      end subroutine zzcvpool

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzutcpm with the legacy context.
      subroutine zzutcpm ( string, start, hoff, moff, last, succes )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! is a string containing a substring ::UTC+HR:MN
      integer(ip) :: start  !! is the location in the string to start parsing
      real(wp) :: hoff  !! is the d.p. value associated with HR.
      real(wp) :: moff  !! is the d.p. value associated with MN
      integer(ip) :: last  !! is the end of the time zone substring.
      logical :: succes  !! indicates that a time zone was parsed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzutcpm ( ctx, string, start, hoff, moff, last, succes )
      end subroutine zzutcpm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvststr with the legacy context.
      subroutine zzvststr ( x, fill, expont )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! double precision number to needing a virtual string
      character(1) :: fill  !! leading character for virtual string.
      integer(ip) :: expont  !! The exponent associated with X.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzvststr ( ctx, x, fill, expont )
      end subroutine zzvststr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvsbstr with the legacy context.
      subroutine zzvsbstr ( from, to, rnd, substr, did )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: from  !! the index of the first character to retrieve
      integer(ip) :: to  !! the index of the last character to retrieve
      logical :: rnd  !! treat the virtual string as rounded string.
      character(*) :: substr  !! Contents of virtual string from FROM to TO.
      logical :: did  !! is a leading zero a result of rounding.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzvsbstr ( ctx, from, to, rnd, substr, did )
      end subroutine zzvsbstr

      !> Create a formatted string that represents a double precision
      !> number, using a format picture.
      !>
      !> Legacy interface: calls ctx%dpfmt with the legacy context.
      subroutine dpfmt ( x, pictur, str )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! a double precision number.
      character(*) :: pictur  !! a string describing the appearance of the output
      character(*) :: str  !! a string representing X as prescribed by PICTUR
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: pictur is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dpfmt ( x, (pictur), str )
      end subroutine dpfmt

      !> Take a double precision number and convert it to an
      !> equivalent formatted character string representation (base 10).
      !>
      !> Legacy interface: calls ctx%dpstrf with the legacy context.
      subroutine dpstrf ( x, sigdig, format, string )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! A double precision number
      integer(ip) :: sigdig  !! The number of significant digits saved for output
      character(1) :: format  !! 'E' for scientific, 'F' for floating point.
      character(*) :: string  !! A character string representation of X
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: format is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dpstrf ( x, sigdig, (format), string )
      end subroutine dpstrf

      !> Replace a marker in a string with a formatted double precision
      !> value.
      !>
      !> Legacy interface: calls ctx%repmf with the legacy context.
      subroutine repmf ( in, marker, value, sigdig, format, out )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: marker  !! Marker to be replaced.
      real(wp) :: value  !! Replacement value.
      integer(ip) :: sigdig  !! Significant digits in replacement text.
      character(*) :: format  !! Format: 'E' or 'F'.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, marker, format are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%repmf ( (in), (marker), value, sigdig, (format), out )
      end subroutine repmf

      !> Convert Year Month and Day on the Julian Calendar
      !> to the Gregorian Calendar
      !>
      !> Legacy interface: calls ctx%jul2gr with the legacy context.
      subroutine jul2gr ( year, month, day, doy )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: year  !! Year  of Julian Calendar/Gregorian Calendar
      integer(ip) :: month  !! Month of Julian Calendar/Gregorian Calendar
      integer(ip) :: day  !! Day of Month in Julian Calendar/Gregorian Calendar
      integer(ip) :: doy  !! Day of Year in Gregorian Calendar
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%jul2gr ( year, month, day, doy )
      end subroutine jul2gr

      !> Convert Year Month and Day on the  Gregorian Calendar
      !> to the Julian Calendar
      !>
      !> Legacy interface: calls ctx%gr2jul with the legacy context.
      subroutine gr2jul ( year, month, day, doy )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: year  !! Year  of Gregorian Calendar/Julian Calendar
      integer(ip) :: month  !! Month of Gregorian Calendar/Julian Calendar
      integer(ip) :: day  !! Day of Month in Gregorian Calendar/Julian Calendar
      integer(ip) :: doy  !! Day of Year in Julian Calendar
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gr2jul ( year, month, day, doy )
      end subroutine gr2jul

      !> Convert from an ephemeris epoch measured in seconds past
      !> the epoch of J2000 to a calendar string format using a
      !> formal calendar free of leapseconds.
      !>
      !> Legacy interface: calls ctx%etcal with the legacy context.
      subroutine etcal ( et, calstr )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Ephemeris time measured in seconds past J2000.
      character(*) :: calstr  !! A standard calendar representation of ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%etcal ( et, calstr )
      end subroutine etcal

      !> Transform a time vector from one representation and system
      !> to another.
      !>
      !> Legacy interface: calls ctx%ttrans with the legacy context.
      subroutine ttrans ( from, to, tvec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: from  !! description of a time vector.
      character(*) :: to  !! description of a time vector.
      real(wp) :: tvec ( mxcomp )  !! time vector representing an epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ttrans ( from, to, tvec )
      end subroutine ttrans

      !> Set and retrieve the defaults associated with calendar
      !> input strings.
      !>
      !> Legacy interface: calls ctx%timdef with the legacy context.
      subroutine timdef ( action, item, value )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: action  !! Kind of action to take 'SET' or 'GET'.
      character(*) :: item  !! Default item of interest.
      character(*) :: value  !! Value associated with the default item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: action, item are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%timdef ( (action), (item), value )
      end subroutine timdef

      !> Transform time from one uniform scale to another. The uniform
      !> time scales are TAI, GPS, TT, TDT, TDB, ET, JED, JDTDB, JDTDT.
      !>
      !> Legacy interface: calls ctx%unitim with the legacy context.
      real(wp) function unitim ( epoch, insys, outsys )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      real(wp) :: epoch  !! An epoch.
      character(*) :: insys  !! The time scale associated with the input EPOCH.
      character(*) :: outsys  !! The time scale associated with the function value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      unitim = ctx%unitim ( epoch, insys, outsys )
      end function unitim

      !> Convert a string representing an epoch to a double precision
      !> value representing the number of TDB seconds past the J2000
      !> epoch corresponding to the input epoch.
      !>
      !> Legacy interface: calls ctx%str2et with the legacy context.
      subroutine str2et ( timstr, et )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: timstr  !! A string representing an epoch.
      real(wp) :: et  !! The equivalent value in seconds past J2000, TDB.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%str2et ( timstr, et )
      end subroutine str2et

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzprscor with the legacy context.
      subroutine zzprscor ( abcorr, attblk )
      use pepper_kinds, only: ip
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: abcorr  !! Aberration correction string.
      logical :: attblk ( * )  !! Aberration correction attribute block.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzprscor ( ctx, abcorr, attblk )
      end subroutine zzprscor

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvalcor with the legacy context.
      subroutine zzvalcor ( abcorr, attblk )
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: abcorr  !! Aberration correction string.
      logical :: attblk ( * )  !! Aberration correction attribute block.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzvalcor ( ctx, abcorr, attblk )
      end subroutine zzvalcor

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcorepc with the legacy context.
      subroutine zzcorepc ( abcorr, et, lt, etcorr )
      use pepper_kinds, only: wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: abcorr  !! Aberration correction string.
      real(wp) :: et  !! Ephemeris time, seconds past J2000.
      real(wp) :: lt  !! Light time.
      real(wp) :: etcorr  !! Light time-corrected epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzcorepc ( ctx, abcorr, et, lt, etcorr )
      end subroutine zzcorepc

      !> Umbrella of the ZZVSTRNG routines: it does nothing, as in SPICELIB.
      subroutine zzvstrng ( x, fill, from, to, rnd, expont, substr, did )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x
      character(1) :: fill
      integer(ip) :: from
      integer(ip) :: to
      logical :: rnd
      integer(ip) :: expont
      character(*) :: substr
      logical :: did
      end subroutine zzvstrng

end module pepper_legacy_time
