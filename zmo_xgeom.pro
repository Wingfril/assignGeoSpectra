; NAME:
;     xgeom
;
; PURPOSE:
;     Assign slit position on guide image, return location of slit
;
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     xgeom, image, HEADER=header,LAT_PTR=lat_ptr,LON_PTR=lon_ptr,$
;            N_SLTPIX=n_sltpix,SLITH_PIX=slith_pix,$
;            SLITW_ARC=slitw_arc,GROUP_LEADER=group_leader
;
; INPUTS:
;     image	- A guide image associated with the spectra
;
; KEYWORD PARAMETERS:
;     HEADER	   - Header of the image
;     LAT_PTR	   - Pointer to latitude
;     LON_PTR	   - Pointer to Longitude
;     N_SLTPIX	   - Number of pixels on the slit in the guide image
;     SLITH_PIX    - Number of pixels on the spectral image (height)
;     SLITW_ARC	   - Number of arc seconds of slit width on spectral image
;     GROUP_LEADER - ID of spextool base
;
; REVISION HISTORY:
;     Created by Greg Farquhar in 2013. Comments written by Sam Hedges in 2014
;     in an attempt to understand how the program works and aid future students.
;

pro xgeom_startup,GROUP_LEADER=group_leader

    common xgeom_state,state

    mc_getfonts,buttonfont,textfont

    w = {xgeom_base:0L,$
         button_base:0L,$
         plotwin:0L}
    p = {plotwin_size:[512,512],$
         plotwin_wid:0L}
    d = {img:ptr_new(2),$
         x:0L,$
         y:0L,$
         rot:0L,$
         outputPath:'', $
         n_sltpix:0L,$
         slith_pix:0L,$
         slitw_arc:0.,$
         pxsc:0.,$
         lats:ptr_new(1),$
         lons:ptr_new(1),$
         header:ptr_new(1),$
         slit_info:ptr_new(1)}

    state = {w:w,p:p,d:d}
    state.w.xgeom_base = widget_base(TITLE='Xgeom',$
                                        /COLUMN,$
                                        GROUP_LEADER=group_leader,$
                                        /TLB_SIZE_EVENTS)
       state.w.button_base = widget_base(state.w.xgeom_base,$
                                         /ROW,$
                                         /BASE_ALIGN_CENTER,$
                                         MAP=1)
          button = widget_button(state.w.button_base,$
                                 FONT=buttonfont,$
                                 VALUE='Done',$
                                 EVENT_PRO='xgeom_finish',$
                                 UVALUE='Done')

       plot_base = widget_base(state.w.xgeom_base,$
                               /COLUMN)
             
          state.w.plotwin = widget_draw(plot_base,$
                                        /ALIGN_CENTER,$
                                        XSIZE=state.p.plotwin_size[0],$
                                        YSIZE=state.p.plotwin_size[1],$
                                        EVENT_PRO='xgeom_plotwin_event',$
                                        /KEYBOARD_EVENTS,$
                                        /BUTTON_EVENTS)
    cgcentertlb,state.w.xgeom_base     
    widget_control, state.w.xgeom_base, /REALIZE

    widget_control, state.w.plotwin, GET_VALUE=x
    state.p.plotwin_wid=x
    XManager, 'xgeom', $
      state.w.xgeom_base, $
      EVENT_HANDLER='xgeom_resize',$
      /NO_BLOCK,$
      CLEANUP='xgeom_cleanup'

end

pro xgeom_loadimg
common xgeom_state
wset,state.p.plotwin_wid
tvscl,hist_equal(*state.d.img)

end

pro xgeom_resize
common xgeom_state

end

pro xgeom_plotwin_event,event
common xgeom_state

;mouse click to set slit location
if event.type eq 1 and event.release eq 1 then begin
  state.d.x = event.x
  state.d.y = event.y
  ;print,state.d.x,state.d.y
endif
;button presses to shift slit
if (event.type eq 6) and (event.press eq 1) then begin
  case event.key of
    5 : state.d.x -= 1
    6 : state.d.x += 1
    7 : state.d.y += 1
    8 : state.d.y -= 1
  endcase
endif

xgeom_update
end

pro xgeom_update
common xgeom_state
wset,state.p.plotwin_wid
;erase
tvscl,hist_equal(*state.d.img)
;converts rotation from degrees to rad
rad = state.d.rot*!pi/180.
x1=state.d.x
y1=state.d.y
;nslt_pix is the height of the slit in pixels
n_sltpix = state.d.n_sltpix
;default rad is pi/2, so x1=x2, y1 is top of slit, y2 is bottom
x2=x1-state.d.n_sltpix*cos(rad)
y2=y1-state.d.n_sltpix*sin(rad)
xs = fltarr(5)
ys = fltarr(5)
;w = width of slit in pixels
w = ((state.d.slitw_arc/state.d.pxsc)/2. + 1) > 2

xs[0]=x1-w*sin(rad)
ys[0]=y1+w*cos(rad)
xs[1]=x1+w*sin(rad)
ys[1]=y1-w*cos(rad)
xs[2]=x2+w*sin(rad)
ys[2]=y2-w*cos(rad)
xs[3]=x2-w*sin(rad)
ys[3]=y2+w*cos(rad)
xs[4]=xs[0]
ys[4]=ys[0]

;added by Sam to store the top center of slit, slitwidth, slitheight in the header to
;reconstruct slit in analysis programs
slitinfo=fltarr(4)
slitinfo[0]=x1
slitinfo[1]=y1
slitinfo[2]=2*w
slitinfo[3]=state.d.n_sltpix

*state.d.slit_info=slitinfo
print, 'slitinfo'
print, *state.d.slit_info
plots,xs,ys,/device,color='0000FF'x

end

pro xgeom_cleanup,event
common xgeom_state
ptr_free,state.d.img
;ptr_free,state.d.lats
;ptr_free,state.d.lons
ptr_free,state.d.header

end

pro xgeom_finish,event
    common xgeom_state
    rad = state.d.rot*!pi/180.
    x1=state.d.x
    y1=state.d.y
    n_sltpix = state.d.n_sltpix
    slith_pix = state.d.slith_pix
    x2=x1-state.d.n_sltpix*cos(rad)
    y2=y1-state.d.n_sltpix*sin(rad)
    xloc=x1 + (x2-x1)*findgen(slith_pix)/(slith_pix - 1)
    yloc=y1 + (y2-y1)*findgen(slith_pix)/(slith_pix - 1)
    assign_geom,*state.d.img,*state.d.header,xloc,yloc,slith_pix,lon,lat

    *state.d.lons = lon
    *state.d.lats = lat
    openw, 1, state.d.outputPath, /APPEND
    printf, 1, format='(A,",", A, ",", A)', 'lat', 'lon', 'slit_info'
    for i = 0, N_ELEMENTS(lat)-1 do begin

        if i lt 4 then begin
            slit_info = *state.d.slit_info
            printf, 1, format='(A,",", A, ",", A)', lat[i], lon[i], slit_info[i]   
        endif else begin
            printf, 1, format='(A,",", A)', lat[i], lon[i]
        endelse    

    endfor
    close, 1

widget_control,event.top,/destroy

end

pro zmo_xgeom, img, outputPath, HEADER=header, LAT_PTR=lat_ptr, LON_PTR=lon_ptr, $
          N_SLTPIX=n_sltpix, SLITH_PIX=slith_pix, SLITW_ARC=slitw_arc, $
          SLIT_PTR=slit_ptr, GROUP_LEADER=group_leader
    print, 'in zmo_xgeom'
    common xgeom_state
    cancel = 0
    xgeom_startup, GROUP_LEADER=group_leader
    state.d.outputPath = outputPath
    *state.d.img=img
    state.d.rot=90.
    state.d.pxsc=fxpar(header,'PLATE_SC')
    state.d.n_sltpix = n_sltpix
    state.d.slith_pix = slith_pix

    w = fxpar(header,'SLIT')
    s = strsplit(w,'x',/extract)
    state.d.slitw_arc = float(s[0])
    *state.d.header = header
    *state.d.lats=dblarr(n_sltpix)

    *state.d.lons=dblarr(n_sltpix)
    lat_ptr=state.d.lats
    lon_ptr=state.d.lons

    *state.d.slit_info=fltarr(4)
    slit_ptr=state.d.slit_info

    xgeom_loadimg

end
