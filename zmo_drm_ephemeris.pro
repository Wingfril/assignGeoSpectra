;+
; drm_ephemeris
;
; PURPOSE :
;
;  To input into the header ephemeris data (distance from earth, distance
;  from sun, heliocentric latitude and longitude, and relative velocity).
;
; CALLING SEQUENCE :
;
;       header = drm_ephemeris(header,cancelevent)
;
;
; ARGUMENTS
;  INPUT : header - The FITS header to be modified.
;
;  OUTPUT : cancelevent - A variable which contains whether or not
;                         the cancel button was hit.  If the cancel button
;                         is hit then cancelevent is 'Yes' otherwise, its 'No'
;                         If cancel is hit, then every change made to the
;                         header up till that point is lost and control
;                         is returned to DRM.
;
; KEYWORDS
;  INPUT : auto - do not prompt the user with ephemeris data before
;                 continuing.
;
;  OUTPUT : NONE
;
;
; RETURN : header - the updated FITS header
;
;
; EXAMPLE :
;
;  header = drm_ephemeris(header,cancelevent)
;
;
; RESTRICTIONS:
;
;  This program needs a link to the file JPLEPH in the directory this
;  program is run off.  In addition, the program writes three temporary
;  files to your the DRM temp directory and erases them afterwards.
;
; PROCEDURES USED:
;       Functions: sxpar, drm_access_temp_path, drm_acc_FITS_COMMENT
;       Procedures: drm_putfits,dirt,silt,qutoec,flotsam,juldate,drm_util
;
;
; KNOWN BUGS : NONE
;
; ORIGINAL AUTHOR : Darrel K. Robertson; 7/94
;
; UPDATE HISTORY : 7/94 - Widgets added by T. Kanamori
;                  10/94 - changed temp dir. locations; J. Spitale
;                  11/94 - changed to use drm_ephtest; J. Spitale
;		   2/99 - added FITS four-digit year compliance
; 2009/11 BMF removed references to 'juldate'
;
;-
;=============================================================================
function zmo_drm_ephemeris, header,cancelevent, auto=auto, specfun=specfun, $
                        ephem_header=ephem_header,ephem_data=ephem_data, $
                        no_old=no_old

 newheader=header

 cancelevent='No'

 if(NOT keyword_set(specfun)) then specfun=0

 if(drm_exists()) then FITS_COMMENT=drm_acc_FITS_COMMENT(junk) $
 else FITS_COMMENT=''


;-----get date and time from header and convert to yy/mm/dd/hh:mm-----

 date=drm_getfits(newheader, 'DATE-OBS', specfun=specfun,status=status)
 time=drm_getfits(newheader, 'TIME-OBS', specfun=specfun,status=status)
 if(status NE 0) then begin
 date=drm_getfits(newheader, 'DATE_OBS')
 time=drm_getfits(newheader, 'TIME_OBS')
 endif
 date = strtrim(date,2)
 time = strtrim(time,2)
typ = datetype(date)
; print,typ,date
case typ of
    0 : begin  ; date is 'dd/mm/yy'
        check = strtrim(date,2)
        p1 = strpos(check,'/',0)
        day = (strmid(check,0,p1))
        p2 = strpos(check,'/',p1+1)
        month = (strmid(check,p1+1,p2-(p1+1)))
        year = strtrim(string(fix(strmid(check,p2+1,strlen(check)-(p2+1)))$
		+1900),2)
 	sec=strmid(time,6,5)
 	mi=float(strmid(time,3,2)) + (sec/60.)
        if mi lt 10 then min='0'+string(mi,format='(f4.2)') $
	else min = string(mi,format='(f5.2)') 
 	hour=strmid(time,0,2)
    end
    1 : begin ; CCYY-MM-DD
        year = (strmid(date,0,4))
        month = (strmid(date,5,2))
        day = (strmid(date,8,2))
 	sec=strmid(time,6,5)
	mi=float(strmid(time,3,2)) + (sec/60.) 
        if mi lt 10 then min='0'+string(mi,format='(f4.2)') $
	else min = string(mi,format='(f5.2)') 
	hour=strmid(time,0,2)
    end
    2 : begin   ;  CCYY-MM-DDThh:mm:ss[.sss...]
                ; ignor time string for this case
        year = (strmid(date,0,4))
        month = (strmid(date,5,2))
        day = (strmid(date,8,2))
        pos = strpos(date,'T') + 1
        hh = fix(strmid(date,pos(0),2))
        mm = fix(strmid(date,pos(0)+3,2))
        ss = float(strmid(date,pos(0)+6,strlen(date)))
	hour = hh
	mi = float(mm) + ss/60.
        if mi lt 10 then min='0'+string(mi,format='(f4.2)') $
	else min = string(mi,format='(f5.2)') 
	min = strtrim(string(float(mm) + sec/60.),2)
        time = (hh+(mm+(ss)/60.D)/60.D)/24.D
    end
    else : return,-1
endcase
; sec=strmid(time,6,5)
; min=strtrim( float(strmid(time,3,2)) + (sec/60.) , 2)
; hour=strmid(time,0,2)

 dates=[year+'/'+month+'/'+day+'/'+hour+':'+min]
;print,dates
;help,dates
;print,size(dates)

;---------------------get ephem for target-----------------------

 target=drm_getfits(newheader, 'OBJECT')
 get_eph, dates, target, data,ephem_header=ephem_header,ephem_data=ephem_data


;---------------------get ephem for sun-----------------------

; get_eph, dates, 'Sun', earth_data


;-----------Allow user to update data if desired-------------

  fields=[strtrim(data(7,0),2), $
          strtrim(data(6,0),2), $
          strtrim(data(3,0),2), $
          strtrim(data(1,0),2), $
          strtrim(data(0,0),2), $
          strtrim(data(8,0),2), $
          strtrim(data(9,0),2)]
            
  
  ;if(NOT keyword_set(auto)) then $
   ;begin
    ;cancelled = 0
    ;labels=['Heliocentric Ecliptic Latitude (HLAT) in degrees:  ', $
    ;        'Heliocentric Ecliptic Longitude (HLON) in degrees: ', $
    ;        'Radial Velocity (RVEL) in km/s:                    ', $
    ;        'Distance from Earth (DISTOBJ) in AU:               ', $
    ;        'Distance from Sun (HDIST) in AU:                   ', $
    ;        'Right Ascension (RA) in degrees:                   ', $
    ;        'Declination (DEC) in degrees:                      ']


;    newfields=drm_update_box('Please change the appropriate fields.', $
;                labels, fields, title='Planetary Ephemeris Data', $
;                cancelled=cancelled)
;
;    if(cancelled) then $
;     begin
;      cancelevent='Yes'
;      return, header
;     end
;  
;   end $
    newfields=fields


;----------------Update any changed fields in header---------------
 
  drm_putfits, newheader, 'HLAT', double(newfields(0)),$
      'Heliocentric Latitude, Degrees '+FITS_COMMENT, specfun=specfun

  drm_putfits, newheader, 'HLON', double(newfields(1)),$
      'Heliocentric Longitude, Degrees '+FITS_COMMENT, specfun=specfun

; RADIAL VELOCITY SHOULD BE DEFINED POSITIVE WHEN JUPITER APPROACHES EARTH!
; ajf, 12-14-95
  drm_putfits, newheader, 'RVEL', -1.*double(newfields(2)),$
      'Radial velocity, km/s'+ FITS_COMMENT, specfun=specfun

;  drm_putfits, newheader, 'RVEL', double(newfields(2)),$
;     'Radial velocity, km/s'+ FITS_COMMENT, specfun=specfun


  drm_putfits, newheader, 'DISTOBJ', double(newfields(3)),$
      'Distance from Earth, AU   '+FITS_COMMENT, specfun=specfun

  drm_putfits, newheader, 'HDIST', double(newfields(4)),$
       'Distance from Sun, AU '+FITS_COMMENT, specfun=specfun


  ;--------------------------------------------------------
  ;  If RA and DEC are already in the header, then copy
  ;  to the fields _OLDRA and _OLDDEC and then write
  ;  the new values.
  ;--------------------------------------------------------
  if(NOT keyword_set(no_old)) then begin
    RA=drm_getfits(newheader, 'RA', status=stat)
    if(stat EQ 0) then $
    drm_putfits, newheader, '_OLDRA', RA, 'Original telescope RA field.'
  endif
  
  drm_putfits, newheader, 'RA', double(newfields(5)),$
	' Ephemeris RA '+FITS_COMMENT, specfun=specfun

  if(NOT keyword_set(no_old)) then begin
    DEC=drm_getfits(newheader, 'DEC', status=stat)
    if(stat EQ 0) then $
    drm_putfits, newheader, '_OLDDEC', DEC, 'Original telescope DEC field.'
  endif
  
  drm_putfits, newheader, 'DEC', double(newfields(6)), $
	' Ephemeris DEC '+FITS_COMMENT,specfun=specfun

  return, newheader
 end
;=============================================================================
