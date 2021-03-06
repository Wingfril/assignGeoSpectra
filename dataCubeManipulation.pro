;------------------------------------------------------------------------------
; Given a data cube, it return an 2d array. For each aperture, it has an 
; associated flux, and the array looks at the apertures flux at each wavelength
;------------------------------------------------------------------------------
pro collapsingCube, datacube, wavemax, temparr = temparr, sumArray = sumArray
size = size(datacube, /DIMENSIONS)
sumArray = FLTARR(size[2])
temparr = FLTARR(size[2], wavemax)
help, datacube
for k = 0, size[2]-1 do begin
		sum = 0		
		for i = 0, size[0]-1 do begin
			; The middle stays constant because it decides type of spectra/info
		    temparr[k, i] = datacube[i,1,k]
            sumArray[k] += datacube[i, 0, k] * datacube[i, 1, k]
		endfor
endfor

end

;------------------------------------------------------------------------------
; In case the third dimension is uneven, find the largest one to accomadate
; also find the largest wavelength number in case it differs.
;------------------------------------------------------------------------------
pro getMaxSize, files, numFiles, argument, maxsize = maxsize, $
                wavemax = wavemax
maxNum = 0
total = 0
for i = 0, numFiles - 1 do begin
	data = readfits(files[i], /SILENT)
	dimension = size(data, /DIMENSION)
    wavemax = dimension[0]    
    total += dimension[argument]
	if maxNum lt dimension[argument] then begin
		maxNum = dimension[argument]	
	endif
    if wavemax lt dimension[0] then begin
	    wavemax = dimension[0]	
	endif
endfor
print, 'MAXNUM'
maxsize = maxNum
end

;------------------------------------------------------------------------------
; Takes in several spectra, and calls collapseCube function 
; to sum up the spectrum. All we have now is a 1 pix by 
; however many pixel of image. We put this data into
; a 2d array to form an image.  
;------------------------------------------------------------------------------
pro create2D, event

    common curState, state
   
    ; state.spectraPath contains the directory where all the spectra are stored
    ; and viceversa for everything else 
    state.spectraPath = '/home/mziyan/TestData/17May31.1/proc/'
    state.rawSpectraPath = '/home/mziyan/TestData/17May31.1/spectra/'
    state.outputPath = '/home/mziyan/TestData/17May31.1/maps/'
    state.guidePath = '/home/mziyan/TestData/17May31.1/guideimg/'
    state.calPath = '/home/mziyan/TestData/17May31.1/cal/'
    if state.spectraPath eq '' or state.outputPath eq '' or $
       state.guidePath eq '' or state.calPath eq '' then begin
        print, 'Please select directory paths for ALL fields'       
        goto, ENDEVENT
    endif
    widget_control, state.widgetBase, /DESTROY
    print, 'Processing guide image files'
    guidefiles = file_search(state.guidePath+'*')
    numGuideFiles = size(guidefiles, /DIMENSIONS)
    for i = 0, numGuideFiles[0] - 1 do begin
        print, guideFiles[i]
        if STRMATCH(guideFiles[i], '*reftable*') eq 0 then begin
            drm_cmap_sam, guideFiles[i]
        endif   
    endfor    

    print, 'Finished processing guide images'
    print, 'Begin creating reduced spectra files'
    ; Create the spectra   
    ; Parsing out ....maps/ part of state.outputpath. 
    outputPath = STRMID(state.outputPath, 0, STRLEN(state.outputpath) - 5)
    spexDriver, state.guidePath, state.rawSpectraPath, outputPath, state.calPath
    
    print, 'All spectra has been reduced'
    files = file_search(outputPath+'/proc2/spectra00*.fits')

    print, files
    help, files
    siz = size(files, /DIMENSIONS)

    array = STRARR(siz)

    ; We need maxsize to tell us the largest number of apertures; wavemax
    ; is the maximum wavelength (for example, in May 20th, 2017 data, the 
    ; wavemax is around 811. 
    ; total tells us the total amount of apertures
    getMaxSize, files, siz[0], 2, maxsize = maxsize, wavemax = wavemax

    ; Initializing arrays. 
    ; positionArray to keep track where one pixel becomes the next pixel
    latArray = []
    lonArray = []
    muArray = []
    mu0Array = []
    summedArray = FLTARR(siz[0],maxSize)
    positionArray = FLTARR(siz[0])
    lastGuide = ''
    guideCounter = 0
    guideArray = STRARR(siz[0])
    spectraCounter = -1
;------------------------------------------------------------------------------
    ; Debugging purposes
    oriLonArray = []
    oriLatArray = []
;------------------------------------------------------------------------------

    ; Use a counter to separate between lat/lon of each pixel
    lonlatCounter = 1000

    ; Create a final array that will place spectra side by side like |||||
    finalArray = FLTARR(siz[0], maxSize, wavemax)

    ; Loop through every file. 
    for i = 0, siz[0]-1 do begin
	    datacube = readfits(files[i], header, /SILENT)

        ; Getting list of lat and lon, and also getting which guide image
        ; each spectra used
        lat = strsplit(fxpar(header,'LATS'),',',/extract)
        lon = strsplit(fxpar(header,'LONS'),',',/extract)

        guidePath = fxpar(header,'GUIDE')
        spectraNumStr = STRMID(files[i], STRLEN(files[i])-10, 5)     
        
        ; We use a guideArray to keep track which spectra corresponded
        ; to which array. 
        ; We have a spectra counter that always increments; 
        ; when there is a change in guide path, get the spectra that last used
        ; the previous guide image by subtracting 2 from cur spectra number,
        ; and then the lower boundary is the spectracounter * 2 subtracted from 
        ; the last spectra, since, the spectra numbering increase by 2 every 
        ; time. Finally, we put it all in a string. 
        ; Since we do not know how many guides are used, we use a guide counter
        ; to keep track where we ended. 
        ; Reset spectra counter = -1 because spectra230 to 230 does not need to 
        ; increment spectra counter.  
        if lastGuide ne guidePath and i ne 0 then begin
            
            higherNum = FIX(spectraNumStr) - 2
            lowerNum = higherNum - spectraCounter*2
            guideStr = lastGuide + ','+ STRING(lowerNum) + $
                       ',' + STRING(higherNum)
            guideArray[guideCounter] = guideStr
            guideCounter += 1
            spectraCounter = -1
        endif
        lastGuide = guidePath
        spectraCounter += 1
        lonSize = SIZE(lon, /DIMENSIONS)        
        print, 'on file: ' + files[i]

        lonlatCounter = FIX(spectraNumStr)+10000  
        templonarr = FLTARR(lonSize[0])
        templatarr = FLTARR(lonSize[0])
        tempmuarr  = FLTARR(lonSize[0])
        tempmu0arr = FLTARR(lonSize[0])

;------------------------------------------------------------------------------
        ; Debugging        
        lonn = FLTARR(lonSize[0])
        latt = FLTARR(lonSize[0])
;------------------------------------------------------------------------------
        
        ; Converting strings into floats and correcting a wierd error that
        ; causes longitude to reach like 600. 
        for a = 0, lonSize[0]-1 do begin
            templonarr[a] = FLOAT(lon[a])
            templatarr[a] = FLOAT(lat[a])
            lonn[a] = FLOAT(lon[a])
            latt[a] = FLOAT(lat[a])
            if FLOAT(lat[a]) gt 90 then begin
                templatarr[a] -= 180    
            endif
            if FLOAT(lat[a]) lt -90 then begin
                templatarr[a] += 180    
            endif
            if FLOAT(lon[a]) gt 360 then begin
                templonarr[a] -= 360
            endif
            if FLOAT(lon[a]) lt -360 then begin
                templonarr[a] += 360
            endif
        endfor

        ; Retrieve information about emission angle and solar incidence angle
        getMuMu0, templonarr, templatarr, guidePath, muArray = tempmuarr, $
                  ;mu0Array = tempmu0arr

        ; We append a counter to differentiate between each pixel. 
        ; The last two lines in this block appends the array of lon 
        ; for this spectra to a large array that will contain longitude
        ; information regarding all spectra. 
        temparr = [lonlatCounter, templonarr]
        templonarr = temparr
        temparr = [lonArray, templonarr]
        lonArray = temparr

        ; Same thing as above but for latitude
        temparr = [lonlatCounter, templatarr]
        templatarr = temparr
        temparr = [latArray, templatarr]
        latArray = temparr

        ; Same thing as above, but for mu (emission angle)
        temparr = [lonlatCounter, tempmuarr]
        tempmuarr = temparr
        temparr = [muArray, tempmuarr]
        muArray = temparr        
        
        ; Same thing as above, but for mu0 (solar incidence)
        ; I think there is a easier way to append... but I'm lazy
         ; ¯\_(ツ)_/¯
        temparr = [lonlatCounter, tempmu0arr]
        tempmu0arr = temparr
        temparr = [mu0Array, tempmu0arr]
        mu0Array = temparr  

        ; Getting each (wavelength) layer of spectra
	    collapsingCube, datacube, wavemax, temparr = temparr, sumArray = sumArray
	    sizetemp = size(temparr, /DIMENSIONS)
	    for j = 0, sizetemp[0]-1 do begin
            for k = 0, sizetemp[1]-1 do begin
		        finalArray[i,j,k] = temparr[j,k]
            endfor
            summedArray[i,j] = sumArray[j]
	    endfor
    endfor


    help, finalArray

    rotatedFinalArray = FLTARR(siz[0],maxSize, wavemax)
    sumFinalArray = FLTARR(siz[0],maxSize)
    for k = 0, wavemax - 1 do begin
        for i = 0, siz[0]-1 do begin
	        for j = 0, maxSize-1 do begin
		        rotatedFinalArray[i,j,k] = finalArray[siz[0]-1-i,maxSize-1-j,k]
                if k eq 0 then begin
                    sumFinalArray[i,j] = summedArray[siz[0]-1-i,maxSize-1-j]
                endif

	        endfor
        endfor

    endfor
help, rotatedFinalArray
    ; Outputs all maps at each wavelengths. 
    ; It also creates a summed up image as well. 
    for k = 0, wavemax - 1 do begin
        ; We format it like 003, 012, 123 because file_search is being a little
        ; bitch
        numbering = string(k, format = '(i3.3)')
        writefits, state.outputPath + 'map'+ numbering + '.fits', $
                   rotatedFinalArray[*, *, k]
    endfor
    
    ; Output the 
    writefits, state.outputPath + 'sum.fits', sumFinalArray

    ;tempsize = size(mu0Array, /DIMENSIONS)
    geometryPath = state.outputPath + 'geometryList'
    openw, 1, geometryPath ,/APPEND
	printf, 1, format = '(A,",",A, ",", A, ",", A, ",", A, ",", A)', 'lat', $
                         '      lon','       mu','      mu0', $
                         '      original lat', '      original lon'
	for i = 0, tempsize[0] - 1 do begin
		printf, 1, format='(A,",",A, ",", A, ",", A, ",", A, ",", A)', $
				latArray[i], lonArray[i], $
				muArray[i], mu0Array[i], $
                oriLatArray[i], oriLonArray[i]
	endfor
	close, 1
    outputPath = STRMID(state.outputPath, 0, STRLEN(state.outputpath) - 5)
    spectraPath = outputPath +'proc2/'
    print, 'Writing information to header'
    writeMapHeader, state.outputPath + 'sum.fits', geometryPath, $
                    state.spectraPath, guideArray, guideCounter
    print, 'Finished updating header'
    viewingLayers, state.outputPath + 'sum.fits'
    ENDEVENT: print, 'Exiting dataCubeManipulation...'
end

;------------------------------------------------------------------------------
; Setting up the widget and starting the program; this is the main driver
;------------------------------------------------------------------------------
pro createLayers
common curState, state

	; State structure that will contain variable information and GUI widgets.
	state = {beginProcessButton:0L, $
             beginText:'', $ 
             buttonBaseAll:0L, $
             buttonBase1:0L, $
             buttonBase2:0L, $
             buttonBase3:0L, $
             buttonBase4:0L, $
             buttonBase5:0L, $
	         buttonBase6:0L, $
             calPath:'', $
             calPathText:'', $
             outputPath:'', $
             lat:0.0, $
             lon:0.0, $
             guidePath:'', $
             guidePathText:'', $
             outputPathText:'', $
             pickCalButton:0L, $
             pickGuideButton:0L, $
             pickOutputButton:0L, $
             pickSpectraButton:0L, $
	     pickRawSpectraButton:0L, $
             settingBase:0L, $
	     spectraPath:'', $
             spectraPathText:'', $	     
             rawSpectraPath:'', $
             rawSpectraPathText:'', $
             widgetBase:0L, $
             windowSize:0L}

	; Setting window size
	state.windowSize = 365

	; Title of the widget
	state.widgetBase = widget_base(TITLE = 'PLACEHOLDER TITLE', /COLUMN, $
                       /TLB_SIZE_EVENTS)
	
	; Dealing with buttons
	state.settingBase = widget_base(state.widgetBase, /ROW)
	state.buttonBaseAll = widget_base(state.settingBase, /COLUMN)
	state.buttonBase1 = widget_base(state.buttonBaseAll, /ROW)

	state.pickOutputButton = widget_button(state.buttonBase1, $
                      EVENT_PRO ='getOutputDirectory', VALUE = 'Output Path: ')
	state.outputPathText= widget_text(state.buttonBase1, XSIZE = 80)

	state.buttonBase2 = widget_base(state.buttonBaseAll, /ROW)
	state.pickRawSpectraButton = widget_button(state.buttonBase2, $
                      EVENT_PRO ='getRawSpectrImage', VALUE = 'Raw spectra: ')
	state.rawSpectraPathText = widget_text(state.buttonBase2, XSIZE = 84)
	
	state.buttonBase3 = widget_base(state.buttonBaseAll, /ROW)
	state.pickSpectraButton = widget_button(state.buttonBase3, $
                      EVENT_PRO ='getSpectrImage', VALUE = 'Output spectra: ')
	state.spectraPathText = widget_text(state.buttonBase3, XSIZE = 84)

	state.buttonBase4 = widget_base(state.buttonBaseAll, /ROW)
	state.pickGuideButton = widget_button(state.buttonBase4, $
                      EVENT_PRO ='getGuide', VALUE = 'Guide Images: ')
	state.guidePathText = widget_text(state.buttonBase4, XSIZE = 84)

	state.buttonBase5 = widget_base(state.buttonBaseAll, /ROW)
	state.pickCalButton = widget_button(state.buttonBase5, $
                      EVENT_PRO ='getCal', VALUE = 'Calibration: ')
	state.calPathText = widget_text(state.buttonBase5, XSIZE = 84)

	state.buttonBase6 = widget_base(state.buttonBaseAll)
	state.beginProcessButton = widget_button(state.buttonBase6, $
                       EVENT_PRO ='create2D', VALUE = 'Begin')

	; Once all widget set up is complete, create the widget
	widget_control, state.widgetBase, /REALIZE
	XMANAGER, 'placeholdertext', state.widgetBase, EVENT_HANDLER='resize', $
              /NO_BLOCK
end

;------------------------------------------------------------------------------
; Retrieving where the user wants the output path to be
;------------------------------------------------------------------------------
pro getOutputDirectory, event
common curState, state
	state.outputPath = DIALOG_PICKFILE(DIALOG_PARENT = state.widgetBase, $
                       /DIRECTORY, TITLE = 'Choose output directory')

	widget_control, state.outputPathText, SET_VALUE = state.outputPath
end

;------------------------------------------------------------------------------
; Retrieving the output spectra directory path
;------------------------------------------------------------------------------
pro getSpectrImage, event
    common curState, state
	state.spectraPath = DIALOG_PICKFILE(DIALOG_PARENT = state.widgetBase, $
                     /DIRECTORY, TITLE = 'Choose directory containing output spectra')

	widget_control, state.spectraPathText, SET_VALUE = state.spectraPath
end

;------------------------------------------------------------------------------
; Retrieving the rawspectra directory path
;------------------------------------------------------------------------------
pro getRawSpectrImage, event
    common curState, state
	state.rawSpectraPath = DIALOG_PICKFILE(DIALOG_PARENT = state.widgetBase, $
                     /DIRECTORY, TITLE = 'Choose directory containing raw spectra')

	widget_control, state.rawSpectraPathText, SET_VALUE = state.rawSpectraPath
end


;------------------------------------------------------------------------------
; Retrieving the latitude and longitude directory path
;------------------------------------------------------------------------------
pro getGuide, event
common curState, state
	state.guidePath = DIALOG_PICKFILE(DIALOG_PARENT = state.widgetBase, $
                      /DIRECTORY, $
                      TITLE = 'Choose directory with guide image info')
	widget_control, state.guidePathText, SET_VALUE = state.guidePath
end

;------------------------------------------------------------------------------
; Retrieving the latitude and longitude directory path
;------------------------------------------------------------------------------
pro getCal, event
common curState, state
	state.calPath = DIALOG_PICKFILE(DIALOG_PARENT = state.widgetBase, $
                      /DIRECTORY, $
                      TITLE = 'Choose directory with calibration info')
	widget_control, state.calPathText, SET_VALUE = state.calPath
end

;------------------------------------------------------------------------------
; Does nothing except resizing. 
;------------------------------------------------------------------------------
pro resize, event
common curState, state
end

