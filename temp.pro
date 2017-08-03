; need to auto fit limbs
; need to auto line up frames....so


;------------------------------------------------------------------------------
; Setting up the widget and starting the program to view each layer
; Arguments: image path to the sum.fits file create by dataCubeManipulation.pro
;------------------------------------------------------------------------------
pro viewingLayers, imagePath
    common curState, state
    data = { axisA:0L, $                    ; X-axis
             axisB:0L, $                    ; Y-axis
             boolNewGeometry:0, $           ; 1 if limbs were assigned
             boolChangeEllipse:1, $            ; 1 if ellipse is changed
             centerX:0L, $                  ; State.scale is applied
             centerY:0L, $                  ; State.scale is applied
             correctImgX:0L, $
             correctImgY:0L, $
             correctTempImg:ptr_new(''), $
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
             formatted:0, $
             geometryListDivider:ptr_new(''), $
             imagesize:[0L, 0L], $
             imageX:0L, $
             imageY:0L, $
             latArray:ptr_new(''), $
             lonArray:ptr_new(''), $
             limbArray:ptr_new(2), $
             limbsTheta:0, $
             muArray:ptr_new(''), $
             mu0Array:ptr_new(''), $
             numFiles:0, $
             orientation:'vertical', $
             scaledImage:ptr_new(2), $
             scale:6, $
             spectraPath:'', $
             spectra:ptr_new(''), $
             x:0L, $
             y:0L}
	widget = {buttonBaseAll:0L, $            
             buttonBase1:0L, $ 
             buttonBase2:0L, $
             buttonBase3:0L, $
             buttonBase4:0L, $
             mapsButtonBase:0L, $
             mapsButtonAll:ptr_new(0), $
             changeOrient:0L, $
             changeSelected1:0L, $
             changeSelected2:0L, $
             correctImageButton:0L, $
             correctImgBase:0L, $
             correctImgDone:0L, $
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
             fitLimbsStepBut:0L, $
             fitLimbsStep:0L, $
             fitLimbsTiltLeft:0L, $
             fitLimbsTiltRight:0L, $
             formatButton:0L, $
             getFilesButton:0L, $
             gotoSumLayerButton:0L, $
             infoBase:0L, $
             layerChangeButton:0L, $
             layerInput:'', $
             layerLabel:0L, $
             lonlatLabel:'', $
             manualCorrectImageButton:0L, $
             mumu0Label:'', $
             plotBase:0L, $
             plotwin:0L, $
             plotwinID:0L, $
             settingBase:0L, $
             shift1:0L, $
             shift2:0L, $
             smoothLayerButton:0L, $
             umLabel:'', $
             widgetBase:0L, $
             windowSize:0L, $
             XYLabel:''}

    state = { data:data, widget:widget}
    ; Get the file so we don't have a wierd sized screen when the widget is 
    ; created ( since we initialized to imagesize[0,0])
    loadFile, imagepath

	; Title of the widget
	state.widget.widgetBase = widget_base(TITLE = 'PLACEHOLDER TITLE', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
	state.widget.settingBase = widget_base(state.widget.widgetBase, /ROW)

    state.widget.infoBase = widget_base(state.widget.widgetBase, /ROW)
    ;state.buttonBaseAll = widget_base(state.settingBase, /COLUMN)
    
	; Dealing with buttons
    state.widget.getFilesButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='getFile', VALUE = 'Get File')

    state.widget.fitLimbsButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='fitLimb', VALUE = 'Fit Limb')
    state.widget.layerInput = widget_text(state.widget.settingBase, $
                       XSIZE = 10, /EDITABLE, VALUE = 'Layer#')
    state.widget.layerChangeButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='changeMapEvent', VALUE = 'change layers')
    state.widget.correctImageButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='correctImage', VALUE = 'Auto-Shift')
    state.widget.manualCorrectImageButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='manualCorrectImage', $
                       VALUE = 'Shift manually')
    state.widget.gotoSumLayerButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='gotoSumLayer', VALUE = 'Go to Sum Layer')
    state.widget.smoothLayerButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='smoothLayer', VALUE = 'Smooth layer')
    state.widget.formatButton =  widget_button(state.widget.settingBase, $
                       EVENT_PRO ='format', VALUE = 'Format')

    state.widget.layerLabel = widget_label(state.widget.infoBase, $
                       VALUE = 'Current layer: sum')
    state.widget.umLabel = widget_label(state.widget.infoBase, $
                       VALUE = 'Micron meter: summed flux')
    state.widget.XYLabel = widget_label(state.widget.infoBase, $
                       VALUE = 'X: 000.00, Y: 000.00')
    state.widget.lonlatLabel = widget_label(state.widget.infoBase, $
                       VALUE = 'Lat: 000.0000, Lon: 000.0000')
    state.widget.mumu0Label = widget_label(state.widget.infoBase, $
                       VALUE = 'Mu: 000.0000, Mu0: 000.0000')

    ; Creating the plot area for the maps
    state.widget.plotBase = widget_base(state.widget.widgetBase,$
                           /COLUMN)

    state.widget.plotwin = widget_draw(state.widget.plotBase,$
                              XSIZE=state.data.imagesize[0]*state.data.scale,$
                              YSIZE=state.data.imagesize[1]*state.data.scale,$
                              EVENT_PRO='plotwinevent',$
                              /KEYBOARD_EVENTS,$
                              /BUTTON_EVENTS)
    
    ; Create the widget
    widget_control, state.widget.widgetbase, /REALIZE

    ; Get the window id so we can plot to the correct place
    widget_control, state.widget.plotwin, GET_VALUE = x
    state.widget.plotwinID = x
    print, state.widget.plotwinID

    ; Allow events to be tracked
    XMANAGER, 'placeholdertext', state.widget.widgetBase, $
              EVENT_HANDLER='resize', /NO_BLOCK, CLEANUP='viewLayer_cleanup'
    plotupdate
end
;------------------------------------------------------------------------------
; Prompts user to retrieve a sum.fits file inside a maps folder
;------------------------------------------------------------------------------
pro getFile, event
    common curState, state
    imagePath = DIALOG_PICKFILE(DIALOG_PARENT = state.widget.widgetBase, $
                      TITLE = 'Choose sum.fits file')
    tempstr = strmid(imagePath, strlen(imagePath) - 8)
    if tempstr ne 'sum.fits' then begin
        print, 'please select sum.fits file'    
    endif else begin 
        loadFile, imagepath
    endelse
end

pro format, event
    common curState, state
    state.data.curPath = (*state.data.files)[0]
    *state.data.curImage = readfits (state.data.curPath)
    plotupdate


    fitLimb, 0
    ; It would take up too much memory to store all the limb data. We should
    ; create an array for each layer, and the data in the array will tell us the
    ; cente roffset. 

    ; The average scaled value should be around 20% of largest value

    offsetCenter = fltarr(state.data.numFiles)
    offsetCenter[0] = 0
    ; Starting from the last offset center, look around left/right, moving 1
    ; pixel at a time. Look at the +-5 pixels (of the original image, so 30)
    ; pixels to left and right. 
    for a = 1, state.data.numFiles do begin
        state.data.curPath = (*state.data.files)[0]
        *state.data.curImage = readfits (state.data.curPath)
        plotupdate

        tempimg = *state.data.curImage
        minim = max(tempimg)

        ; Find the smallest number within the image that is not 0
        for i = 0, state.data.imageSize[0] - 1 do begin
            for j = 0, state.data.imageSize[1] - 1 do begin
                if tempimg[i, j] lt minim and tempimg[i, j] ne 0 then begin
                    minim = tempimg[i, j]                
                endif
            endfor
        endfor    
        tempimg /= minim    
        maxim = max(tempimg)
        state.data.centerX += offsetCenter[a-1]
        averageArr = fltarr(11)
        trackingArr = FINDGEN(10, start = -5)
        counter = 0
        aveCounter = 0
        aveTotal = 0
        oriCenterX = state.data.centerX
        oriCenterY = state.data.centerY

        for z = -5, 5 do begin
            state.data.centerX = oriCenterX + z
            ;state.data.centerY = oriCenterY + z
        
            drawEllipse
            onLimbArrayContent = []
            ; onLimbArrayContent contains the values that is located at each x, y
            ; coord on the ellipse
            for i = 0, 360 do begin
                x = fix((*state.data.limbArray)[0, i] / state.data.scale)
                y = fix((*state.data.limbArray)[1, i] / state.data.scale)
                if x lt state.data.imageSize[0]-1 and y ge 0 and x ge 0 and $
                   y lt state.data.imageSize[1]-1 then begin 
                   aveCounter += 1
                   aveTotal += tempimg[x, y]
                endif
            endfor 
            averageArr[counter] = (aveTotal/aveCounter) / maxim
            counter += 1
            aveCounter = 0
            aveTotal = 0
        endfor
        
        
    endfor


end
;------------------------------------------------------------------------------
; This procedure will only run if you have already assigned limbs.
; It will only work on the current layer. 
; It looks at the very top and the very bottom of each vertical line of pixels
; and sees if it is either both below the center and the limbs, or if it is
; both above the center and the limbs. 
; If it is outside of the limbs, it must be the sky. Thus, we run a for-loop
; across the vertical line, and subtract the sky value from each of the
; pixels. 
; We have a larger loop that will loop through every x on the image. 
; Finally, we save this new image at the current path, replacing the old image
;------------------------------------------------------------------------------
pro smoothLayer, event
    common curState, state
    scale = state.data.scale

    if state.data.boolNewGeometry eq 0 then begin
        print, 'Please assign limbs'
    endif else begin   
        whereChange = 0
        ; Look at where it is in relation to center and look at limb coords.  
        for i = 0, state.data.imagesize[0]-1 do begin    
            limbArray = *state.data.limbArray
            ; Limb Y had two values since this is an ellipse
            limbY = where(limbArray[0, *] eq i*state.data.scale)

            maxY = max(limbArray[1, limbY])
            minY = min(limbArray[1, limbY])

            ; Look to see if there is sky at the top of the image
            if (state.data.imagesize[1]-1)*scale gt state.data.centery and $
               (state.data.imagesize[1]-1)*scale gt maxY $
               then begin
                image = (*state.data.curImage)
                change = image[i, state.data.imagesize[1]-1]
            ; Look to see if there is sky at the bottom of the image
            endif else if (0)*scale lt state.data.centery and $
                          (0)*scale lt minY then begin
                image = (*state.data.curImage)
                change = image[i, 0]
            endif
            
            ; Subtract this sky value from the vertical line of pixels
            for j = 0, state.data.imagesize[1] - 1 do begin
                ;for k = 0, state.numFiles - 1 do begin
                ;endfor
                image[i, j] -= change
            endfor
        endfor
        
        upperLimbCoordX = []
        upperLimbCoordY = []

        lowerLimbCoordX = []
        lowerLimbCoordY = []

        for i = 0, 360 do begin
            x = (*state.data.limbArray)[0, i] / state.data.scale
            y = (*state.data.limbArray)[1, i] / state.data.scale   

            print, 'x, y'
            print, x, y 
            if x lt state.data.imageSize[0]-1 and x ge 0 and $
               y ge state.data.imageSize[1] / 2 and $
               y le state.data.imageSize[1] then begin 
                
                tempCoordArr = [x]
                temparr = [upperLimbCoordX, tempCoordArr]
                upperLimbCoordX = temparr

                tempCoordArr = [y]
                temparr = [upperLimbCoordY, tempCoordArr]
                upperLimbCoordY = temparr
            endif

            if y lt state.data.imageSize[1] / 2 and y ge 0 and $
               x lt state.data.imageSize[0]-1 and x ge 0 then begin 
                
                tempCoordArr = [x]
                temparr = [lowerLimbCoordX, tempCoordArr]
                lowerLimbCoordX = temparr

                tempCoordArr = [y]
                temparr = [lowerLimbCoordY, tempCoordArr]
                lowerLimbCoordY = temparr
            endif
        endfor


        ; Insertion sort since the upperLimbcoordX is mostly reversed and it
        ; is the fastest out of the non-recursive sorts

        for a = 1, N_ELEMENTS(upperLimbCoordX) - 1 do begin
            value = upperLimbCoordx[a]
            j = a
            while j gt 0 and upperLimbCoordX[j-1] gt upperLimbCoordX[j] $
            do begin
                temp = upperLimbCoordX[j - 1]              
                upperLimbCoordX[j - 1] = upperLimbCoordX[j]
                upperLimbCoordX[j] = temp

                temp = upperLimbCoordY[j - 1]              
                upperLimbCoordY[j - 1] = upperLimbCoordY[j]
                upperLimbCoordY[j] = temp

                j -= 1
            endwhile
        endfor

        lowerY = fltarr(state.data.imageSize[0])
        upperY = fltarr(state.data.imageSize[0])

        average = fltarr(state.data.imageSize[0])
        for i = 0, state.data.imageSize[0] - 1 do begin
            limbArray = *state.data.LimbArray
            upperX = nearest_element(i, upperLimbCoordX, upperXindex)
            lowerX = nearest_element(i, lowerLimbCoordX, lowerXindex)

            upperY[i] = fix(upperLimbCoordY[upperXindex])
            lowerY[i] = fix(lowerLimbCoordY[lowerXindex])

            for j = lowerY[i], upperY[i] do begin
                average[i] += image[i, j]
            endfor

        endfor
        twomeanAve = 0
        fourMeanAve = 0

        ; Weight the nearest two elements heavier than the four surrounding one
        for i = 1, state.data.imageSize[0] - 2 do begin
            twomeanAve = (average[i-1] + average[i+1])/2
            if i ge 2 and i le state.data.imageSize[0] - 3 then begin
                fourmeanAve = (average[i-1] + average[i+1] + average[i+2] + $
                                   average[i-2])/4
            endif else if i eq 1 then begin
                fourmeanAve = (average[i-1] + average[i+1] + average[i+2] + $
                                   average[i+3])/4      
            endif else if i eq state.data.imageSize[0] - 2 then begin
                fourmeanAve = (average[i-1] + average[i+1] + average[i-3] + $
                                   average[i-2])/4   
            endif else begin
                fourMeanAve = twoMeanAve
            endelse
            totalAve = (twoMeanAve + fourMeanAve) / 2
            change = totalAve - average[i]
            change /= upperY[i] - lowerY[i]
            for j = lowerY[i], upperY[i] do begin

                image[i,j] += change
            endfor
        endfor
        ; Update the image, overwrite the original file.
        (*state.data.curImage) = image


        ; Soooooooo the way I did it with load file is that it takes everything
        ; that exists in /maps folder... so if we did write fits and then 
        ; reload viewingLayers on /maps... uhhh bad things happen
        ;writefits, state.data.curPath+'smooth', image
        ;(*state.data.files)[state.data.curPathIndex] = $
        ;                                         state.data.curPath + 'smooth'
        plotupdate
    endelse
end
;------------------------------------------------------------------------------
; We know that sum layer must be located at the very end of the state.files 
; array, so we jump there to see the summed flux across wavelength for each
; pixel. 
; It retrieves path from state.files, and changes the respective pathindex
; as well as current image.
; Finally ya plot the damn thing. 
;------------------------------------------------------------------------------
pro gotoSumLayer, event
    common curState, state
    state.data.curPath = (*state.data.files)[state.data.numFiles - 1]
    state.data.curPathIndex = state.data.numFiles - 1
    image = readfits (state.data.curPath)
    (*state.data.curImage) = image
    plotupdate
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro correctImage, event
    common curState, state
    if state.data.boolNewGeometry eq 0 then begin
        print, 'Please assign limbs'
    endif else begin
        print, 'in correctImage!'
        tempimg = *state.data.curImage
        minim = max(tempimg)

        ; Find the smallest number within the image that is not 0
        for i = 0, state.data.imageSize[0] - 1 do begin
            for j = 0, state.data.imageSize[1] - 1 do begin
                if tempimg[i, j] lt minim and tempimg[i, j] ne 0 then begin
                    minim = tempimg[i, j]                
                endif
            endfor
        endfor

        ; Scale the image by dividing everything by that smallest number within
        ; the image
        for i = 0, state.data.imageSize[0] - 1 do begin              
            for j = 0, state.data.imageSize[1] - 1 do begin          
                if tempimg[i, j] ne 0 then begin
                    tempimg[i, j] /= minim 
                endif
            endfor
        endfor

        onLimbArrayContent = []
        onLimbCoordX = []
        onLimbCoordY = []

        ; onLimbArrayContent contains the values that is located at each x, y
        ; coord on the ellipse
        for i = 0, 360 do begin
            x = fix((*state.data.limbArray)[0, i] / state.data.scale)
            y = fix((*state.data.limbArray)[1, i] / state.data.scale)
            if x lt state.data.imageSize[0]-1 and y gt -1 and x gt -1 and $
               y lt state.data.imageSize[1]-1 then begin 
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
        for i = 0, onLimbSize[0] - 1 do begin
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
        badLimbs = INTARR(N_ELEMENTS(onLimbArrayContent))
        for i = 0, onLimbSize[0] - 1 do begin
            if scaledOnLimbArrayContent [i] gt 2.5 then begin
                badLimbs[i] = 1
            endif else begin
                badLimbs[i] = 0
            endelse
        endfor    
        shiftSpectra, badLimbs, onLimbCoordX, onLimbCoordY, tempimg, $
                      lowLimbAverage
     endelse
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro shiftSpectra, badlimbs, x, y, tempimg, lowLimbAverage
    common curState, state
    ; Now tempimg is the by the factors of lowLimbAverage, and 
    tempimg /= lowLimbAverage
    changeYNeeded = 0   
    tempimgSize = Size(tempimg, /DIMENSIONS)


    newimage = tempimg

    ; To see if the limb of planet is actually visible in the pictureeeee
    flag1 = 0
    flag2 = 0

    realtopY = 'this is such'
    realbottomY = 'BS'
    y1 = 'random strings'
    y2 = 'lol
    
    factorArr = STRARR(state.imagesize[1])
    finishedX = []
    
    for i = 0, N_ELEMENTS(badlimbs) do begin
        if badlimbs[i] eq 1 and where(finishedX eq x[i]) eq -1 then begin
            for a = 0, state.imagesize[1] - 1 do begin
                if tempimg[x[i], a] gt 2 then begin
                    factorArr[a] = 'planet'
                                            
                endif else if tempimg[x[i], a] lt 1 then begin
                    factorArr[a] = 'sky'
                endif else begin
                    factorArr[a] = 'possible Limb'
                endelse
                tempXArr = [x[i]]
                temparr = [finishedX, tempXArr]
                finishedX = temparr
            endfor

            ; THe other way is to search for the other y and see if there are
            ; any...
            allY = where(y eq y[i])

            bottomY = min(y[allY])
            topY = max(y[allY])
            
            for a = 0, state.imagesize[1] - 1 do begin
                if factorArr[a-1] eq 'sky' and $
                   factorArr[a] eq 'possible Limb' and flag1 eq 0 then begin
                    flag1 = 1
                    realBottomY = a
                endif else if (factorArr[a-1] eq 'possible Limb' and $
                              factorArr[a] eq 'sky') and flag2 eq 0 then begin
                    flag2 = 1
                    realTopY = a-1
                endif
            endfor
            
            if realTopY ne 'this is such' then begin
       
                changeYNeeded = topY - realTopY  

            endif else if realBottomY ne 'BS' then begin
                changeYNeeded = bottomY - realBottomY
            endif
            
            for j = 0, imagesize[1] - 1 do begin
                if j - changeYNeeded lt imageSize[1] and $
                j - changeYNeeded gt 0 then begin
                    newimage[x, j] = tempimg[x, j - changeYNeeded]
                endif                    
            endfor

        endif

        realTopY = 'this is such'
        realBottomY = 'BS'
        topY = 'random strings'
        bottomY = 'lol'
        flag1 = 0
        flag2 = 0
    endfor   

    ; Shifting the spectra one by one, in the y direction in of a map. 
    ; Consider changeYNeeded and changeAtX arrays???
    for i = 0, state.data.numFiles do begin
        image = readfits(state.data.files[i])
        newimage = image
        imagesize = size(image, /DIMENSIONS)
        ;writefits, state.data.files[i], newimage
    endfor
end
;------------------------------------------------------------------------------
; Allows user to manually move a line of pixels. 
;------------------------------------------------------------------------------
pro manualCorrectImage, event
    common curState, state
    state.data.correctImgX = 0
    state.data.correctImgY = 0

    correctTempImg = fltarr(state.data.imageSize[0]*3, $
                            state.data.imageSize[1]*3)
    help, correctTempImg
    image = *state.data.curImage

    for i = 0, state.data.imageSize[0] - 1 do begin
        for j = 0, state.data.imageSize[1] - 1 do begin
            correctTempImg[i+state.data.imageSize[0], $
                           j+state.data.imageSize[1]] = image[i, j]
            print, correctTempImg[i+state.data.imageSize[0], $
                           j+state.data.imageSize[1]]
        endfor
    endfor
    *state.data.correctTempImg = correctTempImg

    ;print, *state.data.correctTempImg

    state.widget.correctImgBase = widget_base(TITLE = 'Manual Corrections', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
    state.widget.changeOrient = widget_button(state.widget.correctImgBase, $
                            EVENT_PRO = 'changeOrientation', $
                            VALUE = 'Vertical', UVALUE = 'vertical')
    state.widget.changeSelected1 = widget_button(state.widget.correctImgBase, $
                            EVENT_PRO = 'changeSelectionLocation', $
                            VALUE = 'Move Left', UVALUE = 'left')
    state.widget.changeSelected2 = widget_button(state.widget.correctImgBase, $
                            EVENT_PRO = 'changeSelectionLocation', $
                            VALUE = 'Move Right', UVALUE = 'right')
    state.widget.shift1 = widget_button(state.widget.correctImgBase, $
                            EVENT_PRO = 'shiftPixels', $
                            VALUE = 'Shift Up', $
                            UVALUE = 'shiftUp') 
    state.widget.shift2 = widget_button(state.widget.correctImgBase, $
                            EVENT_PRO = 'shiftPixels', $
                            VALUE = 'Shift Down', $
                            UVALUE = 'shiftDown') 
    state.widget.correctImgDone = widget_button(state.widget.correctImgBase, $
                            EVENT_PRO = 'correctImageDone', $
                            VALUE = 'Done' )

        ; Create the widget
    widget_control, state.widget.correctImgBase, /REALIZE

    XMANAGER, 'Manual image correction', state.widget.correctImgBase, $
              EVENT_HANDLER='resize', /NO_BLOCK
    updateSelection
end

pro changeOrientation, event
    common curState, state

    widget_control, event.id, GET_UVALUE = uvalue
    if uvalue eq 'vertical' then begin
        widget_control, state.widget.changeOrient, $
                        SET_UVALUE = 'horizontal' 
                        SET_VALUE = 'Horizontal' 
        widget_control, state.widget.changeSelected1, $
                        SET_UVALUE = 'up', $
                        SET_VALUE = 'Move Up'
        widget_control, state.widget.changeSelected2, $
                        SET_UVALUE = 'down', $
                        SET_VALUE = 'Move Down'
        widget_control, state.widget.shift1, $
                        SET_UVALUE = 'shiftLeft', $
                        SET_VALUE = 'Shift Left'
        widget_control, state.widget.shift2, $
                        SET_UVALUE = 'shiftRight', $
                        SET_VALUE = 'Shift Right'
        state.data.orientation = 'horizontal'
        updateSelection
    endif else begin
        widget_control, state.widget.changeOrient, $
                        SET_UVALUE = 'vertical', $
                        SET_VALUE = 'Vertical'       
        widget_control, state.widget.changeSelected1, $
                        SET_UVALUE = 'left', $
                        SET_VALUE = 'Move Left'
        widget_control, state.widget.changeSelected2, $
                        SET_UVALUE = 'right', $
                        SET_VALUE = 'Move Right'
        widget_control, state.widget.shift1, $
                        SET_UVALUE = 'shiftUp', $
                        SET_VALUE = 'Shift Up'
        widget_control, state.widget.shift2, $
                        SET_UVALUE = 'shiftDown', $
                        SET_VALUE = 'Shift Down'
        state.data.orientation = 'vertical'
        updateSelection
    endelse
end

pro changeSelectionLocation, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    case uvalue of 
       'left' : begin
            state.data.imageX -= state.data.scale
        end
       'right' : begin
            state.data.imageX += state.data.scale
        end
       'up' : begin
            state.data.imageY += state.data.scale
        end
       'down' : begin
            state.data.imageY -= state.data.scale
        end    
    endcase 
    updateSelection

end

pro shiftPixels, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    correctTempImg = *state.data.correctTempImg
    ;print, correctTempImg
    case uvalue of 
       'shiftRight' : begin
            for i = state.data.imagesize[0]*3-1, 1, -1 do begin
                correctTempImg[i, state.data.imageY/state.data.scale + $
                state.data.imageSize[1]] $
                = correctTempImg[i-1, state.data.imageY/state.data.scale + $
                  state.data.imageSize[1]]
                ;print, correctTempImg[i, state.data.imageY/state.data.scale]
            endfor
        end
       'shiftLeft' : begin
            for i = 0, state.data.imagesize[0]*3 - 2 do begin
               help, correctTempImg
                print, 'state.data.imageY/state.data.scale+state.data.imageSize[1]'
                print, state.data.imageY/state.data.scale+state.data.imageSize[1]
                print, 'i'
                print, i
                correctTempImg[i, $
                state.data.imageY/state.data.scale+state.data.imageSize[1]] $
                = correctTempImg[i+1, $
                  state.data.imageY/state.data.scale + state.data.imageSize[1]]
            endfor
        end
       'shiftDown' : begin
            for i = 0, state.data.imagesize[1]*3 - 2 do begin
                correctTempImg[state.data.imageX/state.data.scale + $
                state.data.imageSize[0], i] $
                = correctTempImg[state.data.imageY/state.data.scale + $
                  state.data.imageSize[0], i+1]
            print, correctTempImg[state.data.imageX/state.data.scale, i]
            endfor
        end
       'shiftUp' : begin
            for i = state.data.imagesize[1]*3-1, 1, -1 do begin
               help, correctTempImg
                print, 'state.data.imageX/state.data.scale+state.data.imageSize[0]'
                print, state.data.imageX/state.data.scale+state.data.imageSize[0]
                print, 'i
                print, i
                correctTempImg[state.data.imageX/state.data.scale + $
                state.data.imageSize[0], i] $
                = correctTempImg[state.data.imageX/state.data.scale + $
                  state.data.imageSize[0], i-1]
                

            endfor
        end    
    endcase

    image = fltarr(state.data.imageSize[0], state.data.imageSize[1])

    *state.data.correctTempImg = correctTempImg
    for i = 0, state.data.imageSize[0] - 1 do begin
        for j = 0, state.data.imageSize[1] - 1 do begin
            image[i, j] = correctTempImg[i+state.data.imageSize[0], $
                          j+state.data.imageSize[1]]
        endfor
    endfor
    *state.data.curimage = image   
    plotupdate
    updateSelection
end

pro correctImageDone, event
    common curState, state

end

pro updateSelection
    common curState, state
    ; Erase previous limb fit
    wset, state.widget.plotwinID & tvscl, (*state.data.scaledImage), $ 
            xsize = state.data.imagesize[0], ysize = state.data.imagesize[1]
    if state.data.orientation eq 'horizontal' then begin
        x = [0, (state.data.imagesize[0] - 1)*state.data.scale, $
             (state.data.imagesize[0] - 1)*state.data.scale, 0, 0]
        y = [state.data.imageY, state.data.imageY, $
             state.data.imageY + state.data.scale, $
             state.data.imageY + state.data.scale, $
             state.data.imageY]
    endif else begin
        y = [0, 0, (state.data.imagesize[1] - 1)*state.data.scale, $
             (state.data.imagesize[1] - 1)*state.data.scale, 0]
        x = [state.data.imageX, state.data.imageX + state.data.scale, $
             state.data.imageX + state.data.scale, state.data.imageX, $
             state.data.imageX]
    endelse
        plots, x, y, /device, color='00FF00'x
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
    state.data.curPath = imagePath
    image = readfits (imagepath, curHeader)
    *state.data.curImage = image
    state.data.imagesize = SIZE(image, /DIMENSIONS)
    *state.data.curHeader = curHeader

    ; Retrieve geometry reference list path, read it, and put it into its
    ; respective array
    geometryPath = fxpar(curHeader,'GEOMPATH')
    state.data.spectraPath = fxpar(curHeader,'SPECPATH')
    spectraFile = file_search(state.data.spectraPath+'*')
    *state.data.spectra = readfits(spectraFile[0])
    print, spectraFile[0]
    geometryInfo = read_csv(geometryPath, HEADER = geometryHeader, $ 
                   N_TABLE_HEADER = 2, TABLE_HEADER = geometryTableHeader)
    *state.data.latArray = geometryInfo.field1
    *state.data.lonArray = geometryInfo.field2
    *state.data.muArray = geometryInfo.field3
    *state.data.mu0Array = geometryInfo.field4
    print, 'Read in geometric data complete'
    
    ; We set the pointer to a variable for better readability (personally...)
    muArray = *state.data.muArray

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
    *state.data.geometryListDivider = geometryListDivider
    print, 'Found all index dividers in geometric reference list'
    print, 'Finding all map layer files'

    ; This way, layer 0 is the first element, and as elements increase
    ; the layer number increases. The very last layer is the summed map. 
    files = file_search(strmid(imagePath, 0, strlen(imagePath)-8)+'map*')
    ;print, files
    state.data.numFiles = N_ELEMENTS(files) + 1
    print, 'state.numFiles'
    print, state.data.numFiles
    ; This way to append the sum.fits file to the rest of the map layers 
    temparr = STRARR(1)
    temparr[0] = imagepath
    *state.data.files = [files, temparr]
    state.data.curPathIndex = N_ELEMENTS(files)
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
    wset, state.widget.plotwinID & tvscl, (*state.data.scaledImage), $ 
            xsize = state.data.imagesize[0], ysize = state.data.imagesize[1]

    ; This is for mouse clicks. 
    if event.type eq 1 and event.release eq 1 then begin
      
        state.data.x = fix(event.x/state.data.scale) * state.data.scale
        state.data.y = fix(event.y/state.data.scale) * state.data.scale
        changeLoc = 1
    endif
    if (event.type eq 6) and (event.press eq 1) then begin
        case event.key of
            ; Changing selection of pixel
            ; Left
            5 : begin
                    state.data.x -= state.data.scale
                    changeLoc = 1
                end
            ; Right
            6 : begin
                    state.data.x += state.data.scale
                    changeLoc = 1
                end
            ; Up
            7 : begin
                    state.data.y += state.data.scale
                    changeLoc = 1
                end
            ; Down
            8 : begin 
                    state.data.y -= state.data.scale
                    changeLoc = 1
                end
            ; Changing currently layer with page up and page down button
            9 : begin
                   if state.data.curPathIndex eq state.data.numfiles-1 $
                      then begin
                       print, 'At last file'
                   endif else begin
                       state.data.curPath = $
                               (*state.data.files)[state.data.curPathIndex+1]
                       state.data.curPathIndex += 1
                       image = readfits (state.data.curPath)
                       (*state.data.curImage) = image
                       plotupdate 
                   endelse
                end
           ; Changing currently layer with page up and page up button
            10 : begin
                   if state.data.curPathIndex eq 0 then begin
                       print, 'At first file'
                   endif else begin
                       state.data.curPath = $
                                (*state.data.files)[state.data.curPathIndex-1]
                       state.data.curPathIndex -= 1
                       image = readfits(state.data.curPath)
                       (*state.data.curImage) = image
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
    x = [state.data.x, state.data.x+state.data.scale, $
         state.data.x+state.data.scale, state.data.x, state.data.x]
    y = [state.data.y, state.data.y, state.data.y-state.data.scale, $
         state.data.y-state.data.scale, state.data.y]

    if state.data.boolNewGeometry eq 1 then begin
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
    ; Scale the original image to however large state.data.scale is. 
    scalImage = CONGRID((*state.data.curImage), state.data.imagesize[0] * $
                 state.data.scale, state.data.imagesize[1]*state.data.scale)
    *state.data.scaledImage = scalImage
    wset, state.widget.plotwinID 
    tvscl, (*state.data.scaledImage), $ 
            xsize = state.data.imagesize[0]*state.data.scale, $
            ysize = state.data.imagesize[1]*state.data.scale
end

;------------------------------------------------------------------------------
; Updates the displayed lat, lon, mu, mu0
;------------------------------------------------------------------------------
pro updateInfoBar
    print, 'in updateinfoBar'
    common curState, state
    if state.data.curPathIndex eq state.data.numFiles - 1 then begin
        curUM = 'Micron meter: sum'
    endif else begin
        spectra = *state.data.spectra 
        curUM = 'Micron meter: ' + $
                strtrim(spectra[state.data.curPathIndex, 0, 0], 2)
    endelse
    
    val = 'X: '+strtrim(string(state.data.x,FORMAT='(f7.2)'),2)+ $
        ',  Y: '+strtrim(string(state.data.y,FORMAT='(f7.2)'),2)
    geomVal1 = 'Lat: ' + strtrim(state.data.curLat, 2) + $
             ', Lon: ' + strtrim(state.data.curLon, 2)

    geomVal2 = 'Mu: ' + strtrim(state.data.curMu, 2) + $
             ', Mu0: ' + strtrim(state.data.curMu0, 2)
    layerInfo = 'Layer: ' + strmid(state.data.curPath, $
                strlen(state.data.curPath)-8, 3)
    widget_control, state.widget.layerLabel, SET_VALUE = layerInfo
    widget_control, state.widget.umLabel, SET_VALUE = curUM
    widget_control, state.widget.XYLabel, SET_VALUE = val
    widget_control, state.widget.lonlatLabel, SET_VALUE = geomVal1
    widget_control, state.widget.mumu0Label, SET_VALUE = geomVal2
    
    
    print, val
    print, geomVal1
    print, geomVal2
    print, layerInfo
    
end
;------------------------------------------------------------------------------
; (╯°□°）╯︵ ┻━┻ 
; pls ⌐╦╦═─ (ಥ_ಥ)  
; Right now, we transform all the coordinates the elongated map 
; into a circle, and then we call the code from DRM to get mu, lat, and lon.
; We also used portions Sam Hedges' code, drm_cmap_sam.pro, to get mu0.  
;------------------------------------------------------------------------------
pro getGeometry
    common curState, state
    realX = state.data.x / state.data.scale
    realY = state.data.y / state.data.scale
    geometryListDivider = *state.data.geometryListDivider
    latArray = *state.data.latArray

    ; If we are outside of the boundariessss
    if realY lt 0 or realY gt state.data.imagesize[1] then begin
        state.data.curLat = NaN
        state.data.curLon = NaN
        state.data.curMu = NaN
        state.data.curMu0 = NaN
    endif else if state.data.boolNewGeometry eq 0 then begin
        ; Assigning derefenced pointer to a variable for easier readability
        latArray = *state.data.latArray
        lonArray = *state.data.lonArray
        muArray  = *state.data.muArray
        mu0Array = *state.data.mu0Array

        index = geometryListDivider[realY]
        index += realX + 1
        state.data.curLat = latArray[index]
        state.data.curLon = lonArray[index]
        state.data.curMu = muArray[index]
        state.data.curMu0 = mu0Array[index]
    endif else begin
        index = geometryListDivider[realY]
        spectraNum = fix(latArray[index] - 10000)

        spectraPath = state.data.spectraPath + 'spectra*' + $
                      STRTRIM(spectraNum,2) +'*'
        spectraHeader = headfits(spectraPath)

        ; To get the lat/lon/mu/mu0, we need certain ephemeris data that drm
        ; can get for us. 
        ; The version below disables pop-ups. 
        newHeader = zmo_drm_ephemeris(spectraHeader,cancelevent, auto=auto, $
                                  specfun=specfun, ephem_header=ephem_header, $
                                  ephem_data=ephem_data)

        ; God damn radians
        theta = state.data.limbsTheta * !CONST.PI / 180

        axisA = state.data.axisA
        axisB = state.data.axisB 

        ; Transform the center coordinates
        centX = state.data.centerX(173 / axisA*cos(theta) + $
                 160 / axisB*sin(theta)) * axisB/axisA
        centY = state.data.centerY(160 / axisB*cos(theta) + $
                 173 / axisA*sin(theta)) * axisA/axisA

        ; we need the center information to be in the temporary header
        fxaddpar,newHeader,'CX', centX, 'center x of planettt'
        fxaddpar,newHeader,'CY', centY, 'center y of planettt'
        fxaddpar,newHeader,'CCW', -360.000, 'center y of planettt'
        newHeader = zmo_drm_lcm(newHeader)

        realX *= state.data.scale        
        realY *= state.data.scale
 

        ; So these numbers are based on the length and width, pixel-wise,
        ; of guide images....   
        ; Credit to Kenny Duran (another summer intern) for the 
        ; transformation formula
        transX = realX(173 / axisA*cos(theta) + $
                 160 / axisB*sin(theta)) * axisB / axisA
        transY = realY(160 / axisB*cos(theta) + $
                 173 / axisA*sin(theta)) * axisA / axisA

        ; Actually get the lat/long 
        status = zmo_pf_image_latlon(newHeader, transX, transY, lat, long, $
                                     hit, error, a, b, c)
        ; If errored out of if it did not hit the planet... NaN
        if status ne 0 or hit eq 0 then begin 
            state.data.curLat = NaN
            state.data.curLon = NaN
            state.data.curMu = NaN
            state.data.curMu0 = NaN
        endif else begin
            state.data.curLat = lat
            state.data.curLon = long
            ; jupguide 00254, line 67841
            state.data.curMu = pf_get_mu( newHeader, [transX, transY], $
                          lat = lat, long = long, sub_lat = sub_lat, $
                          sub_long = sub_long, status = status) * !CONST.PI/2
            
            getMu0, newHeader, lat, long, mu0 = mu0
            state.data.curMu0 = mu0
        
       endelse
       ;xvspec, spectraPath 
    endelse
end
;------------------------------------------------------------------------------
; Allows user to enter in a number in the box, and if it is a valid layer, 
; we will jump to that layer and load all respective data. 
;------------------------------------------------------------------------------
pro changeMapEvent, event
    common curState, state
    widget_control, state.widget.layerInput, GET_VALUE = inputValue

    ; strnumber is a function in astronlib that return 1 if the string can 
    ; be changed to a numeric value, and 0 otherwise
    if strnumber(inputValue) eq 1 then begin
        print, 'valid number'
        if fix(inputValue) gt state.data.numFiles-1 or $
        fix(inputValue) lt 0 then begin
            print, 'please print a valid number between 0 and ' + $
            strtrim(state.data.numFiles-1, 2)
        endif else begin
            state.data.curPath = (*state.data.files)[fix(inputValue)]
            state.data.curPathIndex = fix(inputValue) - 1
            image = readfits (state.data.curPath)
            (*state.data.curImage) = image
            plotupdate 
        endelse
    endif else begin
        print, 'please print a valid number between 0 and ' + $
                strtrim(state.data.numFiles-1, 2)
    endelse
end

;------------------------------------------------------------------------------
; This sets up the widget base for fitting limbs
;
;
;------------------------------------------------------------------------------
pro fitLimb, event
    common curState, state
    state.widget.fitLimbsBase = widget_base(TITLE = 'Fitting Limbs', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
    state.widget.fitLimbsMoveLeft = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move left', UVALUE = 'left')
    state.widget.fitLimbsMoveRight = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move right', UVALUE = 'right')
    state.widget.fitLimbsMoveUp = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move up', UVALUE = 'up')
    state.widget.fitLimbsMoveDown = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeLimbLocation', $
                            VALUE = 'Move down', UVALUE = 'down')
    state.widget.fitLimbsIncrA = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Increase horizontal axis radius', $
                            UVALUE = 'IncrA')
    state.widget.fitLimbsIncrB = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Increase vertical axis radius', $
                            UVALUE = 'IncrB')
    state.widget.fitLimbsDecrA = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Decrease horizontal axis radius', $
                            UVALUE = 'DecrA')
    state.widget.fitLimbsDecrB = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeAxis', $
                            VALUE = 'Decrease vertical axis radius', $
                            UVALUE = 'DecrB')
    state.widget.fitLimbsTiltRight = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'tiltLimb', $
                            VALUE = 'Tilt Right', UVALUE = 'tiltRight')
    state.widget.fitLimbsTiltLeft = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'tiltLimb', $
                            VALUE = 'Tilt Left' , UVALUE = 'tiltLeft')
    state.widget.fitLimbsStepBut = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'changeStep', $
                            VALUE = 'Make Step Large' )
    state.widget.fitLimbsDone = widget_button(state.widget.fitLimbsBase, $
                            EVENT_PRO = 'done', $
                            VALUE = 'Done' )
    widget_control, state.widget.fitLimbsBase, /REALIZE
    XMANAGER, 'Fitting Limbs', state.widget.fitLimbsBase, $
              EVENT_HANDLER='resize', /NO_BLOCK
    state.data.centerX = state.data.imagesize[0]*3
    state.data.centerY = state.data.imagesize[1]*3
    state.widget.fitLimbsStep = state.data.scale
    state.data.axisA = state.data.imagesize[0]*3  
    state.data.axisB = state.data.imagesize[1]*3
    drawEllipse 
end

pro done, event
    common curState, state
    state.data.boolNewGeometry = 1
    widget_control, state.widget.fitLimbsBase, /DESTROY
end

;------------------------------------------------------------------------------
; Change the steps that moving limbs and change axis uses. We start off
; moving one pixel at a time. 
;------------------------------------------------------------------------------
pro changeStep, event
    common curState, state
    if state.widget.fitLimbsStep eq state.data.scale then begin
        state.widget.fitLimbsStep = state.data.scale * 5
        widget_control, state.widget.fitLimbsStepBut, $
                        SET_VALUE = 'Make Step Small'

    endif else begin
        state.widget.fitLimbsStep = state.data.scale
        widget_control, state.widget.fitLimbsStepBut, $
                        SET_VALUE = 'Make Step Large'
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
            state.data.centerX -= state.widget.fitLimbsStep
        end
        'right' : begin
            state.data.centerX += state.widget.fitLimbsStep
        end
        'up' : begin
            state.data.centery += state.widget.fitLimbsStep
        end
        'down' : begin
            state.data.centery -= state.widget.fitLimbsStep
        end
    endcase
    state.data.boolChangeEllipse = 1
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
            state.data.axisA += state.widget.fitLimbsStep
        end
        'IncrB' : begin
            state.data.axisB += state.widget.fitLimbsStep
        end
        'DecrA' : begin
            state.data.axisA -= state.widget.fitLimbsStep
        end
        'DecrB' : begin
            state.data.axisB -= state.widget.fitLimbsStep
        end
    endcase
    state.data.boolChangeEllipse = 1
    drawEllipse
end

pro tiltLimb, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    step = state.widget.fitLimbsStep
    case uvalue of
        'tiltLeft' : begin
            state.data.limbsTheta += step / state.data.scale
        end
        'tiltRight' : begin
            state.data.limbsTheta -= step / state.data.scale
        end
    endcase
    state.data.boolChangeEllipse = 1
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
    wset, state.widget.plotwinID & tvscl, (*state.data.scaledImage), $ 
            xsize = state.data.imagesize[0], ysize = state.data.imagesize[1]
    ; Only calculate new limbArray if a change, such as axis change or
    ; center change is made
    if state.data.boolChangeEllipse eq 1 then begin
        limbArray = FLTARR(2, 361)    
        theta = state.data.limbsTheta * !CONST.PI/180
        for i = 0, 360 do begin
            radians = i * !CONST.PI/ 180
         
            limbArray[0, i] = state.data.axisA * cos(radians)*cos(theta)-$
                              state.data.axisB * sin(radians)*sin(theta) $
                              + state.data.centerX
            limbArray[1, i] = state.data.axisA * cos(radians)*sin(theta)+$
                              state.data.axisB * sin(radians)*cos(theta) $
                              + state.data.centerY
            ;print, limbArray[0, *]
            ;print, '--'
            ;print, limbArray[1, *]
        endfor
        *state.data.limbArray = limbArray
        state.data.boolChangeEllipse = 0
    endif else begin
        limbArray = *state.data.limbArray
    endelse
    ; Create the center cross to denote center of planet
    verticalLineX = [state.data.centerX-12, state.data.centerX + 12]
    verticalLineY = [state.data.centerY, state.data.centerY]
    horizontalLineY = [state.data.centerY-12, state.data.centerY + 12]
    horizontalLineX = [state.data.centerX, state.data.centerX]
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
    ptr_free, state.data.curHeader
    ptr_free, state.data.curImage
    ptr_free, state.data.files
    ptr_free, state.data.geometryListDivider
    ptr_free, state.data.latArray
    ptr_free, state.data.lonArray
    ptr_free, state.data.limbArray
    ptr_free, state.data.muArray
    ptr_free, state.data.mu0Array
    ptr_free, state.data.scaledImage
    state = 0B
end

;------------------------------------------------------------------------------
; Does nothing except resizing. 
;------------------------------------------------------------------------------
pro resize, event
common curState, state
end


