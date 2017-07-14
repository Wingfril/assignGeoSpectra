;------------------------------------------------------------------------------
; Setting up the widget and starting the program to view each layer
; Arguments: image path to the sum.fits file create by dataCubeManipulation.pro
;------------------------------------------------------------------------------
pro viewingLayers, imagePath
    common curState, state

	state = {axisA:0L, $
             axisB:0L, $
             boolNewGeometry:0, $
             boolChangeAxis:0, $
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
             geometryListDivider:ptr_new(''), $
             getFilesButton:0L, $
             gotoSumLayerButton:0L, $
             imagesize:[0L, 0L], $
             layerChangeButton:0L, $
             layerInput:'', $
             layerLabel:0L, $
             latArray:ptr_new(''), $
             lonArray:ptr_new(''), $
             limbArray:ptr_new(2), $
             muArray:ptr_new(''), $
             mu0Array:ptr_new(''), $
             numFiles:0, $
             plotBase:0L, $
             plotwin:0L, $
             plotwinID:0L, $
             scaledImage:ptr_new(2), $
             scale:6, $
             settingBase:0L, $
             spectraPath:'', $
             widgetBase:0L, $
             windowSize:0L, $
             x:0L, $
             y:0L}
    ; Get the file so we don't have a wierd sized screen when the widget is 
    ; created ( since we initialized to imagesize[0,0])
    loadFile, imagepath

	; Title of the widget
	state.widgetBase = widget_base(TITLE = 'PLACEHOLDER TITLE', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
	state.settingBase = widget_base(state.widgetBase, /ROW)
    ;state.buttonBaseAll = widget_base(state.settingBase, /COLUMN)
    
	; Dealing with buttons
    state.getFilesButton = widget_button(state.settingBase, $
                       EVENT_PRO ='getFile', VALUE = 'Get new summed map')

    state.fitLimbsButton = widget_button(state.settingBase, $
                       EVENT_PRO ='fitLimb', VALUE = 'Fit Limb')

    ;state.layerLabel = widget_label(state.settingBase, VALUE = 'summed map')
    state.layerInput = widget_text(state.settingBase, XSIZE = 20, /EDITABLE)
    state.layerChangeButton = widget_button(state.settingBase, $
                       EVENT_PRO ='changeMapEvent', VALUE = 'change layers')
    state.correctImageButton = widget_button(state.settingBase, $
                       EVENT_PRO ='correctImage', VALUE = 'Correct image')
    state.gotoSumLayerButton = widget_button(state.settingBase, $
                       EVENT_PRO ='gotoSumLayer', VALUE = 'Go to Sum Layer')

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

pro getFile, event
    print, 'in getfile!'
end
pro gotoSumLayer, event
    common curState, state
    
end
pro correctImage, event
    common curState, state
    if state.boolNewGeometry eq 0 do begin
        print, 'Please assign limbs'
    endif else begin
        print, 'in correctImage!'
    endelse
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
    *state.geometryListDivider = geometryListDivider
    print, 'Found all index dividers in geometric reference list'
    print, 'Finding all map layer files'

    ; This way, layer 0 is the first element, and as elements increase
    ; the layer number increases. The very last layer is the summed map. 
    files = file_search(strmid(imagePath, 0, strlen(imagePath)-8)+'map*')
    print, files
    state.numFiles = N_ELEMENTS(files)
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

    ; erase previous 
    wset, state.plotwinID & tvscl, (*state.scaledImage), $ 
            xsize = state.imagesize[0], ysize = state.imagesize[1]

    ; This is for mouse clicks. 
    if event.type eq 1 and event.release eq 1 then begin
      
        state.x = fix(event.x/state.scale) * state.scale
        state.y = fix(event.y/state.scale) * state.scale
    endif
    if (event.type eq 6) and (event.press eq 1) then begin
        case event.key of
            ; Changing selection of pixel
            5 : state.x -= state.scale
            6 : state.x += state.scale
            7 : state.y += state.scale
            8 : state.y -= state.scale
            ; Changing currently selected layer
            9 : begin
               if state.curPathIndex eq state.numfiles then begin
                   print, 'At last file'
               endif else begin
                   state.curPath = (*state.files)[state.curPathIndex + 1]
                   state.curPathIndex += 1
                   image = readfits (state.curPath)
                   (*state.curImage) = image
                   plotupdate 
               endelse
            end
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

    if state.x lt 0 then begin
        state.x = 0
    endif 
    if state.x gt (state.imagesize[0]-1)*state.scale then begin
        state.x = (state.imagesize[0]-1)*state.scale
    endif
    if state.y lt 0 then begin
        state.y = 0
    endif 
    if state.y gt (state.imagesize[1]-1)*state.scale then begin
        state.y = (state.imagesize[1]-1)*state.scale
    endif
    x = [state.x, state.x+state.scale, state.x+state.scale, state.x, state.x]
    y = [state.y, state.y, state.y-state.scale, state.y-state.scale, state.y]
    updateInfoBar
    plots,x,y,/device,color='0000FF'x
    if state.boolNewGeometry eq 1 then begin
        drawEllipse 
    endif
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

    getGeometry
    val = 'X: '+strtrim(string(state.x,FORMAT='(f7.2)'),2)+ $
        ',  Y: '+strtrim(string(state.y,FORMAT='(f7.2)'),2)
    geomVal1 = 'Lat: ' + strtrim(state.curLat, 2) + $
              ', Lon: ' + strtrim(state.curLon, 2)

    geomVal2 = 'Mu: ' + strtrim(state.curMu, 2) + $
              ', Mu0: ' + strtrim(state.curMu0, 2)
    layerInfo = 'Layer: ' + state.curPath
    wset, state.plotwinID
    xyouts, state.imagesize[0] * state.scale, $
        state.imagesize[1] * state.scale - 12, val, /DEVICE, COLOR = 2, $
        CHARSIZE = 10,FONT = 0, ALIGNMENT = 1
    xyouts, state.imagesize[0] * state.scale, $
        state.imagesize[1] * state.scale - 22, geomVal1, /DEVICE, COLOR = 2, $
        CHARSIZE = 10,FONT = 0, ALIGNMENT = 1
    xyouts, state.imagesize[0]*state.scale, $
        state.imagesize[1] * state.scale - 32, geomVal2, /DEVICE, COLOR = 2, $
        CHARSIZE = 10,FONT = 0, ALIGNMENT = 1
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
    if state.boolNewGeometry eq 0 then begin
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
        index = geometryListDivider[realX/2]
        index += realY + 1

        state.curLat = latArray[index]
        state.curLon = lonArray[index]
        state.curMu = muArray[index]
        state.curMu0 = mu0Array[index]
    endif else begin
        index = geometryListDivider[realX/2]
        spectraNum = fix(latArray[index] - 10000)
        spectraPath = state.spectraPath + 'spectra*' + STRTRIM(spectraNum,2) +'*'
        spectraHeader = headfits(spectraPath)
        newHeader = zmo_drm_ephemeris(spectraHeader,cancelevent, auto=auto, $
                                  specfun=specfun, ephem_header=ephem_header, $
                                  ephem_data=ephem_data)
        fxaddpar,newHeader,'CX', state.centerX, 'center x of planettt'
        fxaddpar,newHeader,'CY', state.centerY, 'center y of planettt'
        fxaddpar,newHeader,'CCW', -360.000, 'center y of planettt'
        newHeader = zmo_drm_lcm(newHeader)
        ;print, newheader        
        print, state.axisA
        print, state.axisB
        b = 0.0  
        ;a = state.axisA / state.axisB
        ;b = float(state.axisB) / float(state.axisB)
        ;c = state.axisA / state.axisB
        ; since the axis are off, we need a to adjust for that
        tempscale = state.axisB / state.axisA
        realx = tempscale * realx
        status = zmo_pf_image_latlon(newHeader, realx, realy, lat, long, hit, error, a, b, c)
        state.curLat = lat
        state.curLon = long
        
    endelse
    print, state.curLat
    print, state.curLon
    print, state.curMu
    print, state.curMu0
end

pro changeMapEvent, event
    common curState, state
    ; strnumber is a function in astronlib that return 1 if the string can 
    ; be changed to a numeric value, and 0 otherwise
    if strnumber(state.layerInput) eq 1 then begin
        print, 'valid number'
    endif else begin
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
    drawEllipse
end
;------------------------------------------------------------------------------
;
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

;------------------------------------------------------------------------------
; Every time the image scale or 
;
;------------------------------------------------------------------------------
pro drawEllipse 
    common curState, state
    ; Erase previous limb fit
    wset, state.plotwinID & tvscl, (*state.scaledImage), $ 
            xsize = state.imagesize[0], ysize = state.imagesize[1]
    if state.boolChangeAxis eq 1 then begin
        limbArray = FLTARR(2, 361)    
        for i = 0, 360 do begin
            radians = i * !CONST.PI/ 180
            limbArray[0, i] = state.axisA * cos(radians) + state.centerX
            limbArray[1, i] = state.axisB * sin(radians) + state.centerY
        endfor
        *state.limbArray = limbArray
        state.boolChangeAxis = 0
    endif else begin
        limbArray = *state.limbArray
    endelse
    verticalLineX = [state.centerX-12, state.centerX + 12]
    verticalLineY = [state.centerY, state.centerY]
    horizontalLineY = [state.centerY-12, state.centerY + 12]
    horizontalLineX = [state.centerX, state.centerX]
    plots,limbArray[0, *],limbArray[1, *], /device, color='0000FF'x
    plots, verticalLineX, verticalLineY, /device, color='0000FF'x
    plots, horizontalLineX, horizontalLineY, /device, color='0000FF'x
    print, 'centerx, centery'
    print, state.centerX
    print, state.centerY
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

