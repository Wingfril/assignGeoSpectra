;------------------------------------------------------------------------------
; Setting up the widget and starting the program to view each layer
; Arguments: image path to the sum.fits file create by dataCubeManipulation.pro
;------------------------------------------------------------------------------
pro viewingLayers, imagePath
    common curState, state

	state = {axisA:0L, $
             axisB:0L, $
             boolNewGeometry:0, $
             boolChangeAxis:1, $
             buttonBaseAll:0L, $
             buttonBase1:0L, $ 
             buttonBase2:0L, $
             buttonBase3:0L, $
             buttonBase4:0L, $
             mapsButtonBase:0L, $
             mapsButtonAll:ptr_new(0), $
             centerX:0L, $
             centerY:0L, $
             correctImageButton:0L, $
             curHeader: ptr_new(''), $
             curImage:ptr_new(''), $
             curLayer:0L, $
             curLat:0.0, $
             curLon:0.0, $
             curMu:0.0, $
             curMu0:0.0, $
             curPath:'', $
             curPathIndex:0L, $
             files:ptr_new(''), $
             fitLimbsBase:0L, $
             fitLimbsButton:0L, $
             fitLimbsDone:0L, $
             fitLimbsMoveLeft:0L, $
             fitLimbsMoveDown:0L, $
             fitLimbsMoveRight:0L, $
             fitLimbsMoveUp:0L, $
             fitLimbsIncrA:0L, $
             fitLimbsIncrB:0L, $
             fitLimbsDecrA:0L, $
             fitLimbsDecrB:0L, $
             fitLimbsLatLine:0L, $
             fitLimbsLonLine:0L, $
             fitLimbsStepButton:0L, $
             fitLimbsStep:0L, $
             fitLimbsTiltLeftButton:0L, $
             fitLimbsTiltRightButton:0L, $
             geometryListDivider:ptr_new(''), $
             getFilesButton:0L, $
             gotoSumLayerButton:0L, $
             imagesize:[0L, 0L], $
             infoBase:0L, $
             layerChangeButton:0L, $
             layerInput:'', $
             layerLabel:0L, $
             latArray:ptr_new(''), $
             lonArray:ptr_new(''), $
             lonlatLabel:'', $
             limbArray:ptr_new(2), $
             limbsTheta:0, $
             manualCorrectImageButton:0L, $
             muArray:ptr_new(''), $
             mu0Array:ptr_new(''), $
             mumu0Label:'', $
             numFiles:0, $
             plotBase:0L, $
             plotwin:0L, $
             plotwinID:0L, $
             scaledImage:ptr_new(2), $
             scale:6, $
             settingBase:0L, $
             smoothLayerButton:0L, $
             spectraPath:'', $
             spectra:ptr_new(''), $
             umLabel:'', $
             widgetBase:0L, $
             windowSize:0L, $
             XYLabel:'', $
             x:0L, $
             y:0L}
    ; Get the file so we don't have a wierd sized screen when the widget is 
    ; created ( since we initialized to imagesize[0,0])
    loadFile, imagepath

	; Title of the widget
	state.widgetBase = widget_base(TITLE = 'PLACEHOLDER TITLE', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
	state.settingBase = widget_base(state.widgetBase, /ROW)

    state.infoBase = widget_base(state.widgetBase, /ROW)
    ;state.buttonBaseAll = widget_base(state.settingBase, /COLUMN)
    
	; Dealing with buttons
    state.getFilesButton = widget_button(state.settingBase, $
                       EVENT_PRO ='getFile', VALUE = 'Get File')

    state.fitLimbsButton = widget_button(state.settingBase, $
                       EVENT_PRO ='fitLimb', VALUE = 'Fit Limb')

    state.layerInput = widget_text(state.settingBase, XSIZE = 20, /EDITABLE, $
                       VALUE = 'jump to a layer')
    state.layerChangeButton = widget_button(state.settingBase, $
                       EVENT_PRO ='changeMapEvent', VALUE = 'change layers')
    state.correctImageButton = widget_button(state.settingBase, $
                       EVENT_PRO ='correctImage', VALUE = 'Correct image')
    state.manualCorrectImageButton = widget_button(state.settingBase, $
                       EVENT_PRO ='manualCorrectImage', $
                       VALUE = 'Correct image manually')
    state.gotoSumLayerButton = widget_button(state.settingBase, $
                       EVENT_PRO ='gotoSumLayer', VALUE = 'Go to Sum Layer')
    state.smoothLayerButton = widget_button(state.settingBase, $
                       EVENT_PRO ='smoothLayer', VALUE = 'Smooth layer')

    state.layerLabel = widget_label(state.infoBase, $
                       VALUE = 'Current layer: sum')
    state.umLabel = widget_label(state.infoBase, $
                       VALUE = 'Micron meter: summed flux')
    state.XYLabel = widget_label(state.infoBase, $
                       VALUE = 'X: 000.00, Y: 000.00')
    state.lonlatLabel = widget_label(state.infoBase, $
                       VALUE = 'Lat: 000.0000, Lon: 000.0000')
    state.mumu0Label = widget_label(state.infoBase, $
                       VALUE = 'Mu: 000.0000, Mu0: 000.0000')

    ; Creating the plot area for the maps
    state.plotBase = widget_base(state.widgetBase,$
                           /COLUMN)

    state.plotwin = widget_draw(state.plotBase,$
                                XSIZE=state.imagesize[0]*state.scale,$
                                YSIZE=state.imagesize[1]*state.scale,$
                                EVENT_PRO='plotwinevent',$
                                /KEYBOARD_EVENTS,$
                                /BUTTON_EVENTS)
    
    ; Create the widget
    widget_control, state.widgetbase, /REALIZE

    ; Get the window id so we can plot to the correct place
    widget_control, state.plotwin, GET_VALUE = x
    state.plotwinID = x
    print, state.plotwinID

    ; Allow events to be tracked
    XMANAGER, 'placeholdertext', state.widgetBase, EVENT_HANDLER='resize', $
              /NO_BLOCK, CLEANUP='viewLayer_cleanup'
    plotupdate
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro getFile, event
    print, 'in getfile!'
    imagePath = DIALOG_PICKFILE(DIALOG_PARENT = state.widgetBase, $
                      TITLE = 'Choose sum.fits file')
    tempstr = strmid(imagePath, strlen(imagePath) - 8)
    if tempstr ne 'sum.fits' then begin
        print, 'please select sum.fits file'    
    endif else begin 
        loadFile, imagepath
    endelse
end

pro smoothLayer, event
    common curState, state
    if state.boolNewGeometry eq 0 then begin
        print, 'Please assign limbs'
    endif else begin   
        whereChange = 0
        ; Look at where it is in relation to center and look at limb coordinates.  
        for i = 0, state.imagesize[0]-1 do begin    
            limbArray = *state.limbArray
            limby = where(limbArray[0, *] eq i*state.scale)
            if (state.imagesize[1]-1)*state.scale gt state.centery and $
               (state.imagesize[1]-1)*state.scale gt limbArray[1, limby[0]] $
               then begin
                image = (*state.curImage)

                change = image[i, state.imagesize[1]-1]
                ; we took the sky value at the top
                whereChange = 1
            endif else if (0)*state.scale lt state.centery and $
                          (0)*state.scale lt limbArray[1, limby[0]] then begin
                image = (*state.curImage)
                change = image[i, 0]
                ; we took the sky value at the top
                whereChange = 2
            endif
            
            for j = 0, state.imagesize[1] - 1 do begin
                ;for k = 0, state.numFiles - 1 do begin
                ;endfor
                print, image[i, j], change
                image[i, j] -= change
                print, image[i, j]
            endfor
            ;print, change
        endfor
        (*state.curImage) = image
        writefits, state.curPath, image
        image = readfits (state.curPath)
        (*state.curImage) = image
        plotupdate
    endelse
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro gotoSumLayer, event
    common curState, state
    state.curPath = (*state.files)[state.numFiles - 1]
    state.curPathIndex = state.numFiles - 1
    image = readfits (state.curPath)
    (*state.curImage) = image
    plotupdate
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro correctImage, event
    common curState, state
    if state.boolNewGeometry eq 0 then begin
        print, 'Please assign limbs'
    endif else begin
        print, 'in correctImage!'
        tempimg = *state.curImage
        minim = max(tempimg)
        for i = 0, state.imageSize[0] - 1 do begin
            for j = 0, state.imageSize[1] - 1 do begin
                if tempimg[i, j] lt minim and tempimg[i, j] ne 0 then begin
                    minim = tempimg[i, j]                
                endif
            endfor
        endfor

        for i = 0, state.imageSize[0] - 1 do begin              
            for j = 0, state.imageSize[1] - 1 do begin          
                if tempimg[i, j] ne 0 then begin
                    tempimg[i, j] /= minim 
                endif
            endfor
        endfor

        onLimbArrayContent = []
        onLimbCoordX = []
        onLimbCoordY = []
        print, state.imageSize
        for i = 0, 360 do begin
            x = fix((*state.limbArray)[0, i] / state.scale)
            y = fix((*state.limbArray)[1, i] / state.scale)
            if x lt state.imageSize[0]-1 and y gt -1 and x gt -1 and y lt state.imageSize[1]-1 then begin 
                tempimgArr = [tempimg[x, y]]
                temparr = [onLimbArrayContent, tempimgArr]
                onLimbArrayContent = temparr
                
                tempCoordArr = [x]
                temparr = [onLimbCoordX, tempCoordArr]
                onLimbCoordX = temparr

                tempCoordArr = [y]
                temparr = [onLimbCoordY, tempCoordArr]
                onLimbCoordY = temparr
            endif
        endfor  

    onLimbSize = SIZE(onLimbArrayContent, /DIMENSIONS)

    ; So we use this ghetto method of detecting shifted spectra ( as in shifted
    ; in the y direction). 
    ; LOL so we get the lowest 5 value that is on the limbs, and average them
    ; And then we divide everything by that average
    ; if the resulting value is greater than like 2.5 or 3, we know that it is
    ; wierd. 
    ; so then we look at the x, y coordinate to find the corresponding x 
    ; (basically the corresponding spectra)
    ; we loop through every since damn layer there is and shift the values up. 
    ; lol this is so ghetto. ٩(⁎❛ᴗ❛⁎)۶ 
    ; I'm only kind of sorry for how redundant this is ¯\_(ツ)_/¯
    minim = [max(onLimbArrayContent), max(onLimbArrayContent), $
             max(onLimbArrayContent), max(onLimbArrayContent), $
             max(onLimbArrayContent)]
        for i = 0, onGraphSize[0] - 1 do begin
            if minim[0] lt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = minim[2] 
                minim[2] = minim[1] 
                minim[1] = minim[0] 
                minim[0] = onLimbArrayContent[i] 
            endif else if minim[1] lt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = minim[2] 
                minim[2] = minim[1] 
                minim[1] = onLimbArrayContent[i] 
            endif else if minim[2] lt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = minim[2]  
                minim[2] = onLimbArrayContent[i] 
            endif else if minim[3] lt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = onLimbArrayContent[i] 
            endif else if minim[4] lt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = onLimbArrayContent[i] 
            endif
        endfor
        lowLimbAverage = mean(minim)

        scaledOnLimbArrayContent = onLimbArrayContent / lowLimbAverage
        
        for i = 0, onGraphSize[0] - 1 do begin
            if scaledOnLimbArrayContent [i] gt 2.5 then begin
                x = onLimbCoordX[i]
                y = onLimbCoordY[i]
                shiftSpectra, x, y, tempimg, lowLimbAverage
            endif
        endfor    

    endelse
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro shiftSpectra, x, y, tempimg, lowLimbAverage
    common curState, state
    tempimg /= lowLimbAverage
    changeYNeeded = 0   
    tempimgSize = Size(tempimg, /DIMENSIONS)
    
    if y lt tempimgSize[1]/2 then begin
        start = 0
        ending = tempimg
    endif ;else begins


    for i = 0, tempimgSize[1] do begin
        
    endfor
     

    ; Shifting the spectra one by one, in the y direction in of a map. 
    ; Consider changeYNeeded and changeAtX arrays???
    for i = 0, state.numFiles do begin
        image = readfits(state.files[i])
        newimage = image
        imagesize = size(image, /DIMENSIONS)
        for j = 0, imagesize[1] do begin
            if j + changeYNeeded lt imageSize[1] and j + changeYNeeded gt 0 then begin
                newimage[x, j] = image[x, j + changeYNeeded]
            endif                    
        endfor
        writefits, state.files[i], newimage
    endfor
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro manualCorrectImage, event
    common curState, state
end
;------------------------------------------------------------------------------
; Handles initializing information. 
; It reads in the image and the header, and from the header, it gets
; the location of the geometry reference list, and reads in each respective
; latitude, longitude, mu, mu0
; It also looks for the index where one spectra ends and another one starts
; It then reads in the location of each layer of image.
;------------------------------------------------------------------------------
pro loadFile, imagepath

    common curState, state
    print, 'loading file information....'    
    
    ; I really hate pointers because deceiphering other people's work with 
    ; pointers is hell, but it seems that there are no other way of 
    ; putting unknown size of array into a structure tag.
    
    ; Loading in the image and header
    state.curPath = imagePath
    image = readfits (imagepath, curHeader)
    *state.curImage = image
    state.imagesize = SIZE(image, /DIMENSIONS)
    *state.curHeader = curHeader

    ; Retrieve geometry reference list path, read it, and put it into its
    ; respective array
    geometryPath = fxpar(curHeader,'GEOMPATH')
    state.spectraPath = fxpar(curHeader,'SPECPATH')
    spectraFile = file_search(state.spectraPath+'*')
    *state.spectra = readfits(spectraFile[0])
    print, spectraFile[0]
    geometryInfo = read_csv(geometryPath, HEADER = geometryHeader, $ 
                   N_TABLE_HEADER = 4, TABLE_HEADER = geometryTableHeader)
    *state.latArray = geometryInfo.field1
    *state.lonArray = geometryInfo.field2
    *state.muArray = geometryInfo.field3
    *state.mu0Array = geometryInfo.field4
    print, 'Read in geometric data complete'
    
    ; We set the pointer to a variable for better readability (personally...)
    muArray = *state.muArray

    ; Retrieve the index in the geometrylist where one spectra starts
    geometryListDivider = []
    geometryArraySize = SIZE(geometryInfo.field1, /DIMENSIONS)
    for i = 0, geometryArraySize[0] - 1 do begin
        if muArray[i] - 10000 gt 0 then begin
            tempGeoArr = [i]
            temparr = [geometryListDivider, tempGeoArr]
            geometryListDivider = temparr
        endif
    endfor
    print, geometryListDivider
    help, geometryListDivider
    *state.geometryListDivider = geometryListDivider
    print, 'Found all index dividers in geometric reference list'
    print, 'Finding all map layer files'

    ; This way, layer 0 is the first element, and as elements increase
    ; the layer number increases. The very last layer is the summed map. 
    files = file_search(strmid(imagePath, 0, strlen(imagePath)-8)+'map*')
    ;print, files
    state.numFiles = N_ELEMENTS(files) + 1
    print, 'state.numFiles'
    print, state.numFiles
    ; This way to append the sum.fits file to the rest of the map layers 
    temparr = STRARR(1)
    temparr[0] = imagepath
    *state.files = [files, temparr]
    state.curPathIndex = N_ELEMENTS(files)
    print, 'Loaded all layer location'
    print, 'Loading data complete'

    
end

;------------------------------------------------------------------------------
; Called when the mouse clicks on the graph or if we use the keyboard
; When the mouse clicks to select a pixel, a box with the current scale factor
; is drawn. 
; There are 6 keyboard events. 
; up/down/left/right changes the pixel you are selecting
; Event.key 9 and 10 are page up and page down, respectively. They
; change the layer that you are on. 
; Finally, this routine checks that you aren't going off the image, and 
; it will force you to stay on. (So x can never be negative)
; It then plots the limbs depending on if you have assigned them or not. 
;------------------------------------------------------------------------------
pro plotwinevent, event
    
    common curState, state
    print, 'in plotwinevent'
    changeLoc = 0
    ; erase previous 
    wset, state.plotwinID & tvscl, (*state.scaledImage), $ 
            xsize = state.imagesize[0], ysize = state.imagesize[1]

    ; This is for mouse clicks. 
    if event.type eq 1 and event.release eq 1 then begin
      
        state.x = fix(event.x/state.scale) * state.scale
        state.y = fix(event.y/state.scale) * state.scale
        changeLoc = 1
    endif
    if (event.type eq 6) and (event.press eq 1) then begin
        case event.key of
            ; Changing selection of pixel
            ; Left
            5 : begin
                    state.x -= state.scale
                    changeLoc = 1
                end
            ; Right
            6 : begin
                    state.x += state.scale
                    changeLoc = 1
                end
            ; Up
            7 : begin
                    state.y += state.scale
                    changeLoc = 1
                end
            ; Down
            8 : begin 
                    state.y -= state.scale
                    changeLoc = 1
                end
            ; Changing currently layer with page up and page down button
            9 : begin
                   if state.curPathIndex eq state.numfiles-1 then begin
                       print, 'At last file'
                   endif else begin
                       state.curPath = (*state.files)[state.curPathIndex + 1]
                       state.curPathIndex += 1
                       image = readfits (state.curPath)
                       (*state.curImage) = image
                       plotupdate 
                   endelse
                end
           ; Changing currently layer with page up and page up button
            10 : begin
                   if state.curPathIndex eq 0 then begin
                       print, 'At first file'
                   endif else begin
                       state.curPath = (*state.files)[state.curPathIndex - 1]
                       state.curPathIndex -= 1
                       image = readfits(state.curPath)
                       (*state.curImage) = image
                       plotupdate      
                   endelse
                end
        endcase
    
    endif    

    ;if state.x lt 0 then begin
    ;    state.x = 0
    ;endif 
    ;if state.x gt (state.imagesize[0]-1)*state.scale then begin
    ;    state.x = (state.imagesize[0]-1)*state.scale
    ;endif
    ;if state.y lt 0 then begin
    ;    state.y = 0
    ;endif 
    ;if state.y gt (state.imagesize[1]-1)*state.scale then begin
    ;    state.y = (state.imagesize[1]-1)*state.scale
    ;endif
    x = [state.x, state.x+state.scale, state.x+state.scale, state.x, state.x]
    y = [state.y, state.y, state.y-state.scale, state.y-state.scale, state.y]

    if state.boolNewGeometry eq 1 then begin
        drawEllipse 
    endif

    plots,x,y,/device,color='0000FF'x
    if changeLoc eq 1 then begin
          getGeometry
    endif 
    updateInfoBar
end

;------------------------------------------------------------------------------
; Displays the currently selected image, scaled by a factor. 
;------------------------------------------------------------------------------
pro plotupdate
    common curState, state
    print, 'in plotupdate'
    scalImage = CONGRID((*state.curImage), state.imagesize[0]*state.scale, state.imagesize[1]*state.scale)
    *state.scaledImage = scalImage
    wset, state.plotwinID 
    tvscl, (*state.scaledImage), $ 
            xsize = state.imagesize[0]*state.scale, ysize = state.imagesize[1]*state.scale
    ;print, scalImage
    
end


;------------------------------------------------------------------------------
; Updates the displayed lat, lon, mu, mu0
;------------------------------------------------------------------------------
pro updateInfoBar
    print, 'in updateinfoBar'
    common curState, state
    if state.curPathIndex eq state.numFiles - 1 then begin
        curUM = 'Micron meter: sum'
    endif else begin
        spectra = *state.spectra 
        curUM = 'Micron meter: ' + strtrim(spectra[state.curPathIndex, 0, 0], 2)
    endelse
    
    val = 'X: '+strtrim(string(state.x,FORMAT='(f7.2)'),2)+ $
        ',  Y: '+strtrim(string(state.y,FORMAT='(f7.2)'),2)
    geomVal1 = 'Lat: ' + strtrim(state.curLat, 2) + $
              ', Lon: ' + strtrim(state.curLon, 2)

    geomVal2 = 'Mu: ' + strtrim(state.curMu, 2) + $
              ', Mu0: ' + strtrim(state.curMu0, 2)
    layerInfo = 'Layer: ' + strmid(state.curPath, strlen(state.curPath)-8, 3)
    widget_control, state.layerLabel, SET_VALUE = layerInfo
    widget_control, state.umLabel, SET_VALUE = curUM
    widget_control, state.XYLabel, SET_VALUE = val
    widget_control, state.lonlatLabel, SET_VALUE = geomVal1
    widget_control, state.mumu0Label, SET_VALUE = geomVal2
    
    
    print, val
    print, geomVal1
    print, geomVal2
    print, layerInfo
    
end
;------------------------------------------------------------------------------
; (╯°□°）╯︵ ┻━┻ 
; pls ⌐╦╦═─ (ಥ_ಥ)  
;
;------------------------------------------------------------------------------
pro getGeometry
    common curState, state
    realX = state.x / state.scale
    realY = state.y / state.scale
    geometryListDivider = *state.geometryListDivider
    latArray = *state.latArray
    if realY lt 0 or realY gt state.imagesize[1] then begin
        state.curLat = NaN
        state.curLon = NaN
        state.curMu = NaN
        state.curMu0 = NaN
    endif else if state.boolNewGeometry eq 0 then begin
        ; Assigning derefenced pointer to a variable for easier readability
        latArray = *state.latArray
        lonArray = *state.lonArray
        muArray  = *state.muArray
        mu0Array = *state.mu0Array

        ;print, latArray
        print, 'geo list div elements'
        print, N_ELEMENTS(geometryListDivider)
        print, realX
        print, realX
        index = geometryListDivider[realY]
        index += realX + 1
        state.curLat = latArray[index]
        state.curLon = lonArray[index]
        state.curMu = muArray[index]
        state.curMu0 = mu0Array[index]
    endif else begin
        index = geometryListDivider[realY]
        spectraNum = fix(latArray[index] - 10000)
        print, spectraNum
        spectraPath = state.spectraPath + 'spectra*' + STRTRIM(spectraNum,2) +'*'
        spectraHeader = headfits(spectraPath)
        newHeader = zmo_drm_ephemeris(spectraHeader,cancelevent, auto=auto, $
                                  specfun=specfun, ephem_header=ephem_header, $
                                  ephem_data=ephem_data)

theta = state.limbsTheta * !CONST.PI / 180
        centX = state.centerX(173 / state.axisA*cos(theta) + $
                 160 / state.axisB*sin(theta)) * state.axisB/state.axisA
        centY = state.centerY(160 / state.axisB*cos(theta) + $
                 173 / state.axisA*sin(theta)) * state.axisA/state.axisA


        fxaddpar,newHeader,'CX', centX, 'center x of planettt'
        fxaddpar,newHeader,'CY', centY, 'center y of planettt'
        fxaddpar,newHeader,'CCW', -360.000, 'center y of planettt'
        newHeader = zmo_drm_lcm(newHeader)
        print, 'state.axisA and axisB '        
        print, state.axisA
        print, state.axisB
        b = 0.0  
        a = float(state.axisA / state.axisB)
        b = float(state.axisB) / float(state.axisB)
        c = float(state.axisA) / float(state.axisB)
        ; since the axis are off, we need a to adjust for that
        ;yscale = float(340.0 / (state.axisB/3.0));*49/36)
        ;xscale = float(356.0 / (state.axisA/3.0))
        print, 'axisA'
        print, state.axisA/6
        print, 'axisB'
        print, state.axisB/6
        print, 'centerx'
        print, centX
        print, 'centery'
        print, centY 
        ;print, 'boundaries'

        realX *= 6        
        realY *= 6

       
        transX = realX(173 / state.axisA*cos(theta) + $
                 160 / state.axisB*sin(theta)) * state.axisB/state.axisA
        transY = realY(160 / state.axisB*cos(theta) + $
                 173 / state.axisA*sin(theta)) * state.axisA/state.axisA
        print, 'transx'
        print, transX
        print, 'transy'
        print, transy
        status = zmo_pf_image_latlon(newHeader, transX, transY, lat, long, hit, error, a, b, c)
        if status ne 0 or hit eq 0 then begin 
            print, 'off planet'
        endif else begin
            state.curLat = lat
            state.curLon = long
            ; jupguide 00254, line 67841
            getMu0, newHeader, lat, long, mu0 = mu0
            state.curMu0 = mu0
            state.curMu = pf_get_mu( newHeader, [transX, transY], lat = lat, $
                          long = long, sub_lat = sub_lat, sub_long = sub_long,$
                          status = status) * !CONST.PI/2
       endelse
    endelse
    print, state.curLat
    print, state.curLon
    print, state.curMu
    print, state.curMu0
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro changeMapEvent, event
    common curState, state
    widget_control, state.layerInput, GET_VALUE = inputValue
    print, 'uvale'
    print, inputValue
    ; strnumber is a function in astronlib that return 1 if the string can 
    ; be changed to a numeric value, and 0 otherwise
    if strnumber(inputValue) eq 1 then begin
        print, 'valid number'
        if fix(inputValue) gt state.numFiles-1 or $
        fix(inputValue) lt 0 then begin
            print, 'please print a valid number between 0 and ' + $
            strtrim(state.numFiles-1, 2)
        endif else begin
            state.curPath = (*state.files)[fix(inputValue)]
            state.curPathIndex = fix(inputValue) - 1
            image = readfits (state.curPath)
            (*state.curImage) = image
            plotupdate 
        endelse
    endif else begin
        print, 'please print a valid number between 0 and ' + $
                strtrim(state.numFiles-1, 2)

    endelse

end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro fitLimb, event
    common curState, state
    state.fitLimbsBase = widget_base(TITLE = 'Fitting Limbs', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
    state.fitLimbsMoveLeft = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move left', UVALUE = 'left')
    state.fitLimbsMoveRight = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move right', UVALUE = 'right')
    state.fitLimbsMoveUp = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move up', UVALUE = 'up')
    state.fitLimbsMoveDown = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move down', UVALUE = 'down')
    state.fitLimbsIncrA = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Increase horizontal axis radius', $
                            UVALUE = 'IncrA')
    state.fitLimbsIncrB = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Increase vertical axis radius', $
                            UVALUE = 'IncrB')
    state.fitLimbsDecrA = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Decrease horizontal axis radius', $
                            UVALUE = 'DecrA')
    state.fitLimbsDecrB = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Decrease vertical axis radius', $
                            UVALUE = 'DecrB')
    state.fitLimbsTiltRightButton = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'tiltLimb', $
                            VALUE = 'Tilt Right', UVALUE = 'tiltRight')
    state.fitLimbsTiltLeftButton = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'tiltLimb', $
                            VALUE = 'Tilt Left' , UVALUE = 'tiltLeft')
    state.fitLimbsStepButton = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'changeStep', $
                            VALUE = 'Make Step Large' )
    state.fitLimbsDone = widget_button(state.fitLimbsBase, $
                            EVENT_PRO = 'done', $
                            VALUE = 'Done' )
    widget_control, state.fitLimbsBase, /REALIZE
    XMANAGER, 'Fitting Limbs', state.fitLimbsBase, EVENT_HANDLER='resize', $
              /NO_BLOCK
    state.centerX = state.imagesize[0]*3
    state.centerY = state.imagesize[1]*3
    state.fitLimbsStep = state.scale
    state.axisA = state.imagesize[0]*3  
    state.axisB = state.imagesize[1]*3
;cx_js = state.centerX
;cy_js = state.centerY
;status = fe_bplanetfit(cx_js=cx_js, cy_js=cy_js, data_id=data_id)
    drawEllipse 
end

pro done, event
    common curState, state
    state.boolNewGeometry = 1
    widget_control, state.fitLimbsBase, /DESTROY
end

;------------------------------------------------------------------------------
; Change the steps that moving limbs and change axis uses. We start off
; moving one pixel at a time. 
;------------------------------------------------------------------------------
pro changeStep, event
    common curState, state
    if state.fitLimbsStep eq state.scale then begin
        state.fitLimbsStep = state.scale * 5
        widget_control, state.fitLimbsStepButton, SET_VALUE = 'Make Step Small'

    endif else begin
        state.fitLimbsStep = state.scale
        widget_control, state.fitLimbsStepButton, SET_VALUE = 'Make Step Large'
    endelse
end

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------
pro ChangeLimbLocation, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    case uvalue of
        'left' : begin
            state.centerX -= state.fitLimbsStep
        end
        'right' : begin
            state.centerX += state.fitLimbsStep
        end
        'up' : begin
            state.centery += state.fitLimbsStep
        end
        'down' : begin
            state.centery -= state.fitLimbsStep
        end
    endcase
    state.boolChangeAxis = 1
    drawEllipse
end
;------------------------------------------------------------------------------
; This is called every time 
;------------------------------------------------------------------------------
pro ChangeAxis, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    case uvalue of
        'IncrA' : begin
            state.axisA += state.fitLimbsStep
        end
        'IncrB' : begin
            state.axisB += state.fitLimbsStep
        end
        'DecrA' : begin
            state.axisA -= state.fitLimbsStep
        end
        'DecrB' : begin
            state.axisB -= state.fitLimbsStep
        end
    endcase
    state.boolChangeAxis = 1
    drawEllipse
end

pro tiltLimb, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    case uvalue of
        'tiltLeft' : begin
            state.limbsTheta += state.fitLimbsStep / state.scale
        end
        'tiltRight' : begin
            state.limbsTheta -= state.fitLimbsStep / state.scale
        end
    endcase
    state.boolChangeAxis = 1
    drawEllipse
end
;------------------------------------------------------------------------------
; Every time the image scale or the center changes, drawEllipse will be called
; It will also be called after fitting limbs is complete, so you can see the 
; limbs while selecting pixel for lat/lot. 
; This routine uses 
;------------------------------------------------------------------------------
pro drawEllipse 
    common curState, state
    ; Erase previous limb fit
    wset, state.plotwinID & tvscl, (*state.scaledImage), $ 
            xsize = state.imagesize[0], ysize = state.imagesize[1]
    ; Only calculate new limbArray if a change, such as axis change or
    ; center change is made
    if state.boolChangeAxis eq 1 then begin
        limbArray = FLTARR(2, 361)    
        theta = state.limbsTheta * !CONST.PI/180
            print, 'theta'
            print, theta       
            print, state.limbsTheta 
        for i = 0, 360 do begin
            radians = i * !CONST.PI/ 180
         
            limbArray[0, i] = state.axisA * cos(radians)*cos(theta)-$
                              state.axisB * sin(radians)*sin(theta) $
                              + state.centerX
            limbArray[1, i] = state.axisA * cos(radians)*sin(theta)+$
                              state.axisB * sin(radians)*cos(theta) $
                              + state.centerY
            ;print, limbArray[0, *]
            ;print, '--'
            ;print, limbArray[1, *]
        endfor
        *state.limbArray = limbArray
        state.boolChangeAxis = 0
    endif else begin
        limbArray = *state.limbArray
    endelse
    ; Create the center cross to denote center of planet
    verticalLineX = [state.centerX-12, state.centerX + 12]
    verticalLineY = [state.centerY, state.centerY]
    horizontalLineY = [state.centerY-12, state.centerY + 12]
    horizontalLineX = [state.centerX, state.centerX]
    ; Plots the ellipse and then the center cross
    plots,limbArray[0, *],limbArray[1, *], /device, color='0000FF'x
    plots, verticalLineX, verticalLineY, /device, color='0000FF'x
    plots, horizontalLineX, horizontalLineY, /device, color='0000FF'x

end


;------------------------------------------------------------------------------
; As the name implies (ಠ⌣ಠ) Gotta clean up after yourself
;------------------------------------------------------------------------------
pro viewLayer_cleanup, base

    common curState, state

    ;ptr_free, state.mapsButton
    ptr_free, state.curHeader
    ptr_free, state.curImage
    ptr_free, state.files
    ptr_free, state.geometryListDivider
    ptr_free, state.latArray
    ptr_free, state.lonArray
    ptr_free, state.limbArray
    ptr_free, state.muArray
    ptr_free, state.mu0Array
    ptr_free, state.scaledImage
    state = 0B
end

;------------------------------------------------------------------------------
; Does nothing except resizing. 
;------------------------------------------------------------------------------
pro resize, event
common curState, state
end


