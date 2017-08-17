; want each file to be a 3 ds array; fist layer the map, second layer, lat, 
; third, lon, 4th, mu, 5th mu0

;------------------------------------------------------------------------------
; Setting up the widget and starting the program to view each layer
; Arguments: image path to the sum.fits file create by dataCubeManipulation.pro
;------------------------------------------------------------------------------
pro viewingLayers, imagePath
    common curState, state
    data = { axisA:0L, $                    ; X-axis; state.scale is applied
             axisB:0L, $                    ; Y-axis; state.scale is applied
             boolNewGeometry:0, $           ; 1 if limbs were assigned
             boolChangeEllipse:1, $            ; 1 if ellipse is changed
             boolGetScale:0, $
             centerX:0L, $                  ; State.scale is applied
             centerY:0L, $                  ; State.scale is applied
             correctImgX:0L, $              
             correctImgY:0L, $
             correctTempImg:ptr_new(''), $  ; 9x imagesize, ori img in center
             curHeader: ptr_new(''), $  
             curImage:ptr_new(''), $        ; State.scale *NOT* applied
             curLat:0.0, $
             curLon:0.0, $
             curMu:0.0, $
             curMu0:0.0, $
             curPath:'', $
             curPathIndex:0L, $
             files:ptr_new(''), $
             formatted:0, $
             geometryListDivider:ptr_new(''), $
             imagesize:[0L, 0L], $          ; State.scale *NOT* applied
             imageX:0L, $                   ; State.scale is applied
             imageY:0L, $                   ; State.scale is applied
             latArray:ptr_new(''), $
             lonArray:ptr_new(''), $
             limbArray:ptr_new(2), $
             limbsTheta:0, $
             muArray:ptr_new(''), $
             mu0Array:ptr_new(''), $
             numFiles:0, $
             orientation:'vertical', $
             refFiles:ptr_new(''), $
             saveBool:0l, $
             saveCounter:0, $
             saveData:ptr_new(2), $
             scaledImage:ptr_new(2), $
             scale:6, $
             spectraPath:'', $
             spectra:ptr_new(''), $
             x:0L, $
             y:0L, $
             xscale:50.0, $ 
             yscale:183.0}

    ; All that lovely stuff about widget... idk if I actually need to keep 
    ; track of all of this buttttt ¯\_(ツ)_/¯
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
             shiftDown: 0L, $
             shiftLeft: 0L, $
             shiftRight: 0L, $
             shiftUp: 0L, $
             shiftLayerDone: 0L, $
             shiftLayerBase: 0L, $
             shiftLayerButton: 0L, $
             smoothLayerButton:0L, $
             umLabel:'', $
             widgetBase:0L, $
             windowSize:0L, $
             XYLabel:''}

    ; State is our list of what is basically global vars. 
    state = {data:data, widget:widget}

    ; Get the file so we don't have a wierd sized screen when the widget is 
    ; created ( since we initialized to imagesize[0,0])
    loadFile, imagepath

	; Title of the widget
	state.widget.widgetBase = widget_base(TITLE = 'PLACEHOLDER TITLE', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
    
	; Dealing with buttons
	state.widget.settingBase = widget_base(state.widget.widgetBase, /ROW)
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
    state.widget.shiftLayerButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='shiftLayerControl', $
                       VALUE = 'shift Layer')
    state.widget.gotoSumLayerButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='gotoSumLayer', VALUE = 'Go to Sum Layer')
    state.widget.smoothLayerButton = widget_button(state.widget.settingBase, $
                       EVENT_PRO ='smoothLayer', VALUE = 'Smooth layer')
    state.widget.formatButton =  widget_button(state.widget.settingBase, $
                       EVENT_PRO ='format', VALUE = 'Format')

    ; Information displayed on the second row. 
    state.widget.infoBase = widget_base(state.widget.widgetBase, /ROW)
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
                           /COLUMN, /ALIGN_CENTER)
    state.widget.plotwin = widget_draw(state.widget.plotBase,$
                              XSIZE=state.data.imagesize[0]*state.data.scale,$
                              YSIZE=state.data.imagesize[1]*state.data.scale,$
                              EVENT_PRO='plotwinevent',$
                              /KEYBOARD_EVENTS,$
                              /BUTTON_EVENTS)
    print, 'xsize y size'
    print, state.data.imagesize[0]*state.data.scale
    print, state.data.imagesize[1]*state.data.scale


    ; Create the widget
    widget_control, state.widget.widgetbase, /REALIZE

    ; Get the window id so we can plot to the correct place
    widget_control, state.widget.plotwin, GET_VALUE = x
    state.widget.plotwinID = x

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

    ; It will only take files ending in sum.fits
    if tempstr ne 'sum.fits' then begin
        print, 'please select sum.fits file'    
    endif else begin 
        loadFile, imagepath
    endelse
end

;------------------------------------------------------------------------------
; This *attempts* to fit limbs for each layer, given the inital fit for layer 0 
; It will save the location of the center x, since center y and the semi major
; axis shouldn't change. 
; It first requires the user to fit a few frames for reference. 
; This routine will then call another routine which will make a huge set of 
; images where it places each image on where it thinks it is on the planet, 
; using cx. 
;------------------------------------------------------------------------------
pro format, event
    common curState, state

    ; When you first click format, it turns on saving, which will record
    ; the layer as well as the information regarding the fitted limbs
    if state.data.saveBool eq 0 then begin
        state.data.saveBool = 1
        fittedLimbInfo = intarr(fix(state.data.numfiles/100)+1, 5)
        (*state.data.saveData) = fittedLimbInfo
        print, 'Please select ' + strtrim(fix(state.data.numfiles/100)+1, 2) + $ 
               ' layers and fit their limbs'

        goto, ENDEVENT
    endif

    ; If no limbs were ever fitted, and the current image isn't layer 0, 
    ; this program will fail...
    if state.data.curPath ne (*state.data.files)[0] or $
    state.data.boolNewGeometry ne 1 or state.data.saveBool ne 2 then begin
        print, 'Please fit limbs to layer 0'
        goto, ENDEVENT
    endif

    moveAverage = fltarr(fix(state.data.numFiles/100))
    axisA = mean((*state.data.saveData)[*, 3])
    axisB = mean((*state.data.saveData)[*, 4])
    centerX = mean((*state.data.saveData)[*, 1])
    changeLayers = (*state.data.saveData)[*, 0]
    moveAveCounter = -1    

    print, 'axisA'
    print, axisA
    print, 'axisB'
    print, axisB
    print, 'centerX'
    print, centerX
    print, 'changeLayers'
    print, changeLayers
    print, '(*state.data.saveData)[*, 0]'
    print, (*state.data.saveData)[*, 0]
    print, '(*state.data.saveData)[*, 2]'
    print, (*state.data.saveData)[*, 2]

    for i  = 1,  N_ELEMENTS(moveAverage)-1 do begin

        moveAverage[i] = (*state.data.saveData)[i, 1] - $
                         (*state.data.saveData)[i-1, 1]

        ; How much change divided by the change in layers
        ; This is now how much it should change per layer butttttttttt idk...
        moveAverage[i] /= (*state.data.saveData)[i, 0] - $
                         (*state.data.saveData)[i-1, 0]

    endfor
    print, moveAverage[i]
    ; It would take up too much memory to store all the limb data. We should
    ; create an array for each layer, and the data in the array will tell us 
    ;the center offset. 

    ; The average scaled value should be around 20% of largest value

    ; Create an array with values as the centerX for each layer. 
    ; THe first layer must be fitted before anything else continues
    offsetCenter = fltarr(state.data.numFiles)
    offsetCenter[0] = (*state.data.saveData)[0, 1]
    totalmoved = 0


    ; Starting from the last offset center, look around left/right, moving 1
    ; pixel at a time. Look at the +-5 pixels (of the original image, so 30)
    ; pixels to left and right. 
    for a = 1, state.data.numFiles-1 do begin
        state.data.curPath = (*state.data.files)[a]
        *state.data.curImage = readfits (state.data.curPath)
        plotupdate

        tempimg = *state.data.curImage
        minim = max(tempimg)
    
        ; Change moveAveCounter, which decides expected shift based on curlayer
        if changeLayers[moveAveCounter] le i then begin
            moveAveCounter += 1
        endif

        ; Find the smallest number within the image that is not 0
        for i = 0, state.data.imageSize[0] - 1 do begin
            for j = 0, state.data.imageSize[1] - 1 do begin
                if tempimg[i, j] lt minim and tempimg[i, j] ne 0 then begin
                    minim = tempimg[i, j]                
                endif
            endfor
        endfor    

        ; In case the minim is less than 0, first make the minimum positive, 
        ; then adjust and increase everything, and finally divide by minimum
        if minim lt 0 then begin   
            minim = -minim
            tempimg += minim
            tempimg /= minim
        endif else begin
            tempimg /= minim 
        endelse

        ; Find the highest number in the image, since we will need to for 
        ; comparison between that and the limbs later. 
        maxim = max(tempimg)

        ; The current center is for the last layer  
        state.data.centerY = offsetCenter[a-1]
        
        ; Average arr is ratio of the average limb value divided by the max
        ; value of the image. Tracking arr keeps track of the index
        averageArr = fltarr(13)
        trackingArr = FINDGEN(13, start = -6)
        counter = 0
        aveCounter = 0
        aveTotal = 0
        oriCenterX = state.data.centerX
        oriCenterY = state.data.centerY

        ; We check 6 pixels in either way lol. 
        for z = -6, 6 do begin

            ; We reset centerX everytime, and we need to make boolchangeEllipse
            ; true so things would be recalculated
            state.data.centerY = oriCenterY + z
            state.data.boolChangeEllipse = 1
            drawEllipse, silent = 1
            onLimbArrayContent = []

            ; For each value that is on the ellipse, check if it is located
            ; on our image first. If it is, then we increment average counter, 
            ; and add that number to our total.
            ; we also add that number into onLimbArrayContent array. 
            for i = 0, 720 do begin
                x = fix((*state.data.limbArray)[0, i] / state.data.scale)
                y = fix((*state.data.limbArray)[1, i] / state.data.scale)
                if x lt state.data.imageSize[0]-1 and y ge 0 and x ge 0 and $
                   y lt state.data.imageSize[1]-1 then begin 
                    aveCounter += 1
                    aveTotal += tempimg[x, y]

                    tempimgArr = [tempimg[x, y]]
                    temparr = [onLimbArrayContent, tempimgArr]
                    onLimbArrayContent = temparr
                endif
            endfor 

            ; We use a real ghetto method to find the lowest 5 non-zero 
            ; numbers lol
            minim = [max(onLimbArrayContent), max(onLimbArrayContent), $
                     max(onLimbArrayContent), max(onLimbArrayContent), $
                     max(onLimbArrayContent)]
            for i = 0, N_ELEMENTS(onLimbArrayContent) - 1 do begin
                if minim[0] gt onLimbArrayContent[i] and $
                onLimbArrayContent[i] gt 0 then begin
                    minim[4] = minim[3] 
                    minim[3] = minim[2] 
                    minim[2] = minim[1] 
                    minim[1] = minim[0] 
                    minim[0] = onLimbArrayContent[i] 
                endif else if minim[1] gt onLimbArrayContent[i] and $
                onLimbArrayContent[i] gt 0 then begin
                    minim[4] = minim[3] 
                    minim[3] = minim[2] 
                    minim[2] = minim[1] 
                    minim[1] = onLimbArrayContent[i] 
                endif else if minim[2] gt onLimbArrayContent[i] and $
                onLimbArrayContent[i] gt 0 then begin
                    minim[4] = minim[3] 
                    minim[3] = minim[2]  
                    minim[2] = onLimbArrayContent[i] 
                endif else if minim[3] gt onLimbArrayContent[i] and $
                onLimbArrayContent[i] gt 0 then begin

                    minim[4] = minim[3] 
                    minim[3] = onLimbArrayContent[i] 
                endif else if minim[4] gt onLimbArrayContent[i] and $
                onLimbArrayContent[i] gt 0 then begin
                    minim[4] = onLimbArrayContent[i] 
                endif
            endfor
            lowLimbAverage = mean(minim)
            
            ; Now everything should be in ratios of what is theroetically
            ; on the limbs
            scaledOnLimbArrayContent = onLimbArrayContent / lowLimbAverage
        
            ; If the ratio is too high on the limbs, we are probably actually
            ; on the planets
            error = 0.0
            for s  = 0, N_ELEMENTS(scaledOnLimbArrayContent) - 1 do begin
                if scaledOnLimbArrayContent[s] gt 10 then begin
                    error += 1
                endif
            endfor

            ; Get percentage of error
            error /= N_ELEMENTS(scaledOnLimbArrayContent)

            ; If the error ratio is too high, this is probably a bad fit 
            ; despite the average
            if error gt .2 then begin
                averageArr[counter] = 99999
            endif else begin
                averageArr[counter] = (aveTotal/aveCounter) / maxim
            endelse

            counter += 1
            aveCounter = 0
            aveTotal = 0
        endfor
        
        ; Find the nearest of how much we are expected to move
        ; We will then compare it to the best fit. 
        move = moveAverage[moveAveCounter]*a - totalmoved
        expectedMove = nearest_element(moveAverage[moveAveCounter], $
                       TrackingArr, expectedMoveIndex)

        ; Insertion sort since the upperLimbcoordX is mostly reversed and it
        ; is the fastest out of the non-recursive sorts
        ; 
        for z = 1, N_ELEMENTS(averageArr) - 1 do begin
            value = averageArr[z]
            j = z
            while j gt 0 and averageArr[j-1] gt averageArr[j] $
            do begin
                temp = averageArr[j - 1]              
                averageArr[j - 1] = averageArr[j]
                averageArr[j] = temp

                temp = trackingArr[j - 1]              
                trackingArr[j - 1] = trackingArr[j]
                trackingArr[j] = temp

                j -= 1
            endwhile
        endfor
    print, 'averageArr'
    print, averageArr

    ; We are using a simple way of finding the nearest. There should be
    ; some kind of weighing mechanism that encourages the program to move
    ; the way they did previously, and to stay constant where they are. 
    bestFit = nearest_element(.17, averageArr, bestFitIndex)
    print, 'bestfit'
    print, bestFit

    print, 'bestFitIndex'
    print, bestFitIndex

    print, 'expectedMove'
    print, expectedMove 
    
    print, 'total move'
    print, totalmoved

    print, 'moveAverage[moveAveCounter]'
    print, moveAverage[moveAveCounter]

    print, 'layer'
    print, a


    ; Put the best fitted limb into our array of center X's. 
    ; Tracking arr is the position it has moved from the last center
    offsetCenter[a] = trackingArr[expectedMoveIndex] + oriCenterX

    totalmoved += trackingArr[expectedMoveIndex]

    state.data.centerX = offsetCenter[a]
    state.data.boolChangeEllipse = 1
    drawEllipse, silent = 0
    print, 'Y center'
    print, offsetCenter[a]
    ;printf, 1, format='(A,",", A)', offsetCenter[a], a
    ;wait, 3
    endfor

print, 'state.data.axisA, state.data.axisB'
print, state.data.axisA, state.data.axisB

print, 'state.data.centerX, state.data.centerY'
print, state.data.centerX, state.data.centerY

;close, 1

saveFormattedImage, offsetCenter
print, 'Formatting data finished...'
ENDEVENT: print, 'Exiting formatting data routine...'
end


;------------------------------------------------------------------------------
; After format finishes, this routine is called. The array of center x's
; is passed on to to saveFormattImage. This function will place each layer
; somewhere on new map. This map will place each layer based on where it lies 
; limb. Since when you look through all the layer, jupiter/whatever planet 
; appears to be shifting side ways. With the autofitted limbs, we place 
; each image where it would be on the limbs. This means that this 
; new image will be very long horizontally, and it will mostly be just black 
; spaces with the original image (not stretched or modded in anyway)
; placed somewhere in there. 
;------------------------------------------------------------------------------
pro saveFormattedImage, offsetCenter
    common curstate, state
    ; The yaxis is the same, but since the image is stretched in the x
    ; direction, we need to make sure the size is large enough. 
    ; SIZE DOES MATTER hhehehehe (☞ﾟ∀ﾟ)☞
    xAxis = state.data.imagesize[1]*2 + state.data.axisB/state.data.scale
    yAxis = state.data.imagesize[0]

    ; All files need to be modified
    for i = 0, state.data.numFiles-1 do begin
        ; Create the new image with the new size
        totalImage = fltarr(xAxis, yAxis)
    
        ; Change the current path and get the current image so we 
        ; can place it somewhere in the new image
        state.data.curPath = (*state.data.files)[i]
        curImage = readfits (state.data.curPath)
        minim = min(curImage)
        
        ; We use the centerX to figure out where the left and right
        ; boundaries of the original image should be. 
        ; xAxis/2 is the center X of the ALL formatted images
        leftbound = YAxis/2 - offsetCenter[i]/state.data.scale
        rightbound = leftbound + state.data.imagesize[1]

        
        ; If the y axis does get to it, place the value into the new image
        ; otherwise, it should be black (aka the minimum on an image). 
        for a = 0, yAxis-1 do begin 
            for b = 0, xAxis-1 do begin
                if a gt leftbound  and a lt rightbound then begin
                    totalImage[b,a] = curImage[b, a-leftbound]
                endif else begin
                    totalImage[b,a] = minim
                endelse                
            endfor             
        endfor

        ; Go up one directory from the one we were on, and go into 
        ; formattedMaps. 
        ; Ex: we start off as '/home/mziyan/TestData/May31.1/maps/map001.fits
        ; Getting an array of string without the '/'
        ; Now we have an array of 'home', 'mziyan', TestData', 'May31.1', 'map'
        ; 'map001.fits'
	    pathList = strsplit(state.data.curPath, '/', /EXTRACT)
        ; Combine the part you need together.
        ; Now it's like '/home/mziyan/TestData/May31.1'
	    path = '/' + strjoin(pathList[0:N_ELEMENTS(pathList)-3], '/')
        directoryType = '/formattedMaps/'
        
        ; And add it together...
        ; '/home/mziyan/TestData/May31.1' + '/formattedMaps/' + 'map001.fits'
        outputPath = path + directoryType + pathList[N_ELEMENTS(pathList)-1]
        writefits, outputPath, totalimage  
        (*state.data.files)[i] = outputPath
        
        ; Adding references to previous paths and new paths
        finalHeader = headfits(outputPath)
        fxaddpar,finalHeader,'OLDPATH', (*state.data.refFiles)[i]
        modfits, outputPath, 0, finalHeader
        
        finalHeader = headfits((*state.data.refFiles)[i])
        fxaddpar,finalHeader,'NEWPATH', outputPath
        modfits, (*state.data.refFiles)[i], 0, finalHeader
        
    endfor
    pathList = strsplit(state.data.curPath, '/', /EXTRACT)
    ; Combine the part you need together.
    ; Now it's like '/home/mziyan/TestData/May31.1'
    path = '/' + strjoin(pathList[0:N_ELEMENTS(pathList)-3], '/')
    directoryType = '/formattedMaps/'

    outputPath = path + directoryType + 'info'
    openw, 1, outputPath ,/APPEND
    printf, 1, state.data.axisA/state.data.scale
    printf, 1, state.data.axisB/state.data.scale
    printf, 1, state.data.centerX/state.data.scale
    printf, 1, state.data.centerY/state.data.scale
    close, 1

    ; Change the sum.fits files in the original map folder.
    ; When we load a file, it will automatically redirect to formattedMaps
    ; folder as opposed to the maps folder. 
    finalHeader = headfits((*state.data.refFiles)[state.data.numfiles-1])
    fxaddpar,finalHeader,'FORMAT', 'true'
    modfits, (*state.data.refFiles)[state.data.numfiles-1], 0, finalHeader
    
end

;------------------------------------------------------------------------------
; The pop-up to shift an entire layer
; This is mostly for adjustment after a layer has been formatted. 
;------------------------------------------------------------------------------
pro shiftLayerControl, event 
    common curstate, state

    ; We make an temp image that is 9 times as larger, with the
    ; original image at the center. This way, it will be v. hard to actually 
    ; lose information everytime you move.
    correctTempImg = fltarr(state.data.imageSize[0]*3, $
                            state.data.imageSize[1]*3)

    ; We should always refer to the original file in case you screwed up the 
    ; last one, and screw everything over rip. 
    ; Words of advice: don't try screwing everything you see :p
    ; (I'm sorry I'm trying to make reading through and writing this crap 
    ; entertaining lol)
    header = headfits(state.data.curPath)
    newpath = fxpar(header, 'OLDPATH')
    if TYPENAME(newpath) eq 'STRING' then begin
        path = newpath
    endif else begin
        path = state.data.curPath
    endelse
    image = readfits(path)
    
    ; We put the original image in the center of the temp image. 
    for i = 0, state.data.imageSize[0] - 1 do begin
        for j = 0, state.data.imageSize[1] - 1 do begin
            correctTempImg[i+state.data.imageSize[0], $
                           j+state.data.imageSize[1]] = image[i, j]
            print, correctTempImg[i+state.data.imageSize[0], $
                           j+state.data.imageSize[1]]
        endfor
    endfor

    ; Setting up the widget for shifting layers. 
    ; It will create a popup. 
    *state.data.correctTempImg = correctTempImg

    state.widget.shiftLayerBase = widget_base(TITLE = 'Shift Layer', $
                                    /COLUMN, /TLB_SIZE_EVENTS)
                                    
    state.widget.shiftUp = widget_button(state.widget.shiftLayerBase, $
                            EVENT_PRO = 'shiftLayer', $
                            VALUE = 'Shift Right', $
                            UVALUE = 'shiftRight') 
    state.widget.shiftDown = widget_button(state.widget.shiftLayerBase, $
                            EVENT_PRO = 'shiftLayer', $
                            VALUE = 'Shift Left', $
                            UVALUE = 'shiftLeft')    
    state.widget.shiftLeft = widget_button(state.widget.shiftLayerBase, $
                            EVENT_PRO = 'shiftLayer', $
                            VALUE = 'Shift Up', $
                            UVALUE = 'shiftUp') 
    state.widget.shiftRight = widget_button(state.widget.shiftLayerBase, $
                            EVENT_PRO = 'shiftLayer', $
                            VALUE = 'Shift Down', $
                            UVALUE = 'shiftDown') 
    state.widget.shiftLayerDone = widget_button(state.widget.shiftLayerBase, $
                            EVENT_PRO = 'ShiftLayerFinish', $
                            VALUE = 'Done' )
    widget_control, state.widget.shiftLayerBase, /REALIZE
    XMANAGER, 'Shift Layer', state.widget.shiftLayerBase, $
              EVENT_HANDLER='resize', /NO_BLOCK
end

;------------------------------------------------------------------------------
; This routine is called from shiftlayer control, and it will shift the 
; entire layer in one direction
;------------------------------------------------------------------------------
pro shiftLayer, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    correctTempImg = *state.data.correctTempImg

    ; Literally move pixel by pixel and losing some data in the process lol
    case uvalue of 
       'shiftRight' : begin
            for i = state.data.imagesize[0]*3-1, 1, -1 do begin
                for j = 0, state.data.imagesize[1]*3 -1 do begin
                    correctTempImg[i, j] = correctTempImg[i-1, j]
                endfor
            endfor
        end
       'shiftLeft' : begin
            for i = 0, state.data.imagesize[0]*3 - 2 do begin
                for j = 0, state.data.imagesize[1]*3 - 1 do begin
                    correctTempImg[i, j] = correctTempImg[i+1, j]
                endfor
            endfor
        end
       'shiftDown' : begin
            for j = 0, state.data.imagesize[0]*3 -1 do begin
                for i = 0, state.data.imagesize[1]*3 - 2 do begin
                    correctTempImg[j, i] = correctTempImg[j, i+1]
                endfor
            endfor
        end
       'shiftUp' : begin
            for j = 0, state.data.imagesize[0]*3 -1 do begin
                for i = state.data.imagesize[1]*3-1, 1, -1 do begin
                    correctTempImg[j, i] = correctTempImg[j, i-1]
                endfor   
            endfor  
        end
    endcase

    image = fltarr(state.data.imageSize[0], state.data.imageSize[1])

    ; Extract the image from the larger temp image, taking what is at 
    ; the very center. 
    *state.data.correctTempImg = correctTempImg
    for i = 0, state.data.imageSize[0] - 1 do begin
        for j = 0, state.data.imageSize[1] - 1 do begin
            image[i, j] = correctTempImg[i+state.data.imageSize[0], $
                          j+state.data.imageSize[1]]
        endfor
    endfor
    *state.data.curimage = image   
    plotupdate

end

;------------------------------------------------------------------------------
; Saves the shifted layer in another directory incase we fk'ed up the shifting
; and no data will be lost. 
;------------------------------------------------------------------------------
pro shiftLayerFinish, event
    common curState, state

    ; Go up one directory from the one we were on, and go into 
    ; UFormattedMaps. 
    pathList = strsplit(state.data.curPath, '/', /EXTRACT)
    path = '/' + strjoin(pathList[0:N_ELEMENTS(pathList)-3], '/')
    directoryType = '/UFormattedMaps/'
 
    outputPath = path + directoryType + pathList[N_ELEMENTS(pathList)-1]
    
    print, outputPath
    writefits, outputPath, (*state.data.curimage) 
    (*state.data.files)[state.data.curPathIndex] = outputPath
        
    ; Adding references to previous paths and new paths
    finalHeader = headfits(outputPath)
    fxaddpar,finalHeader,'OLDPATH', $
    (*state.data.refFiles)[state.data.curPathIndex]
    modfits, outputPath, 0, finalHeader
        
    finalHeader = headfits((*state.data.refFiles)[state.data.curPathIndex])
    fxaddpar,finalHeader,'NEWPATH', outputPath
    modfits, (*state.data.refFiles)[state.data.curPathIndex], 0, finalHeader
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
; Finally, we save this new image at another directory. 
;------------------------------------------------------------------------------
pro smoothLayer, event
    common curState, state
    scale = state.data.scale

    if state.data.boolNewGeometry eq 0 then begin
        print, 'Please assign limbs'
    endif else begin   
        whereChange = 0
        ; Look at where it is in relation to center and look at limb coords.  
        for i = 0, state.data.imagesize[1]-1 do begin    
            limbArray = *state.data.limbArray
            ; Limb Y had two values since this is an ellipse
            limbX = where(limbArray[1, *] eq i*state.data.scale)

            maxX = max(limbArray[0, limbX])
            minX = min(limbArray[0, limbX])

            ; Look to see if there is sky at the right of the image
            if (state.data.imagesize[0]-1)*scale gt state.data.centerX and $
               (state.data.imagesize[0]-1)*scale gt maxX $
               then begin
                image = (*state.data.curImage)
                change = image[state.data.imagesize[0]-1, i]
            ; Look to see if there is sky at the left of the image
            endif else if (0)*scale lt state.data.centerx and $
                          (0)*scale lt minx then begin
                image = (*state.data.curImage)
                change = image[0, i]
            endif
            
            ; Subtract this sky value from the vertical line of pixels
            for j = 0, state.data.imagesize[0] - 1 do begin
                ;for k = 0, state.numFiles - 1 do begin
                ;endfor
                image[j, i] -= change
            endfor
        endfor
        
        upperLimbCoordX = []
        upperLimbCoordY = []

        lowerLimbCoordX = []
        lowerLimbCoordY = []

        for i = 0, 720 do begin
            x = (*state.data.limbArray)[0, i] / state.data.scale
            y = (*state.data.limbArray)[1, i] / state.data.scale   

            if x lt state.data.imageSize[0]-1 and $
               x ge state.data.imageSize[0] / 2 and $
               y ge 0 and y le state.data.imageSize[1] then begin 
                
                ; SoMeOnE please tell me a way to APPEND THESE DAMN ARRAYS
                tempCoordArr = [x]
                temparr = [upperLimbCoordX, tempCoordArr]
                upperLimbCoordX = temparr

                tempCoordArr = [y]
                temparr = [upperLimbCoordY, tempCoordArr]
                upperLimbCoordY = temparr
            endif

            ; Lower = to the left
            if y lt state.data.imageSize[1] and y ge 0 and $
               x lt state.data.imageSize[0] / 2 and x ge 0 then begin 
                
                tempCoordArr = [x]
                temparr = [lowerLimbCoordX, tempCoordArr]
                lowerLimbCoordX = temparr

                tempCoordArr = [y]
                temparr = [lowerLimbCoordY, tempCoordArr]
                lowerLimbCoordY = temparr
            endif
        endfor
        print, 'lowerLimbCoordX'
        print, lowerLimbCoordX
        print, 'upperLimbCoordX'
        print, upperLimbCoordX
        print, 'lowerLimbCoordY'
        print, lowerLimbCoordY
        print, 'upperLimbCoordY'
        print, upperLimbCoordY
        ; Insertion sort since the upperLimbcoordX is mostly reversed and it
        ; is the fastest out of the non-recursive sorts
        for a = 1, N_ELEMENTS(lowerLimbCoordY) - 1 do begin
            value = lowerLimbCoordY[a]
            j = a
            while j gt 0 and lowerLimbCoordY[j-1] gt lowerLimbCoordY[j] $
            do begin
                temp = lowerLimbCoordY[j - 1]              
                lowerLimbCoordY[j - 1] = lowerLimbCoordY[j]
                lowerLimbCoordY[j] = temp

                temp = lowerLimbCoordX[j - 1]              
                lowerLimbCoordX[j - 1] = lowerLimbCoordX[j]
                lowerLimbCoordX[j] = temp

                j -= 1
            endwhile
        endfor

        lowerX = fltarr(state.data.imageSize[1])
        upperX = fltarr(state.data.imageSize[1])

        average = fltarr(state.data.imageSize[1])
        print, 'lowerLimbCoordX'
        print, lowerLimbCoordX
        print, 'upperLimbCoordX'
        print, upperLimbCoordX
        print, 'lowerLimbCoordY'
        print, lowerLimbCoordY
        print, 'upperLimbCoordY'
        print, upperLimbCoordY
            limbArray = *state.data.LimbArray
            
        for i = 0, state.data.imageSize[1] - 1 do begin 
            
            upperY = nearest_element(i, upperLimbCoordY, upperYindex)
            lowerY = nearest_element(i, lowerLimbCoordY, lowerYindex)

            upperX[i] = fix(upperLimbCoordX[upperYindex])
            lowerX[i] = fix(lowerLimbCoordX[lowerYindex])
            
            for j = lowerX[i], upperX[i] do begin
                average[i] += image[j, i]
            endfor

        endfor
        twomeanAve = 0
        fourMeanAve = 0

        ; Weight the nearest two elements heavier than the four surrounding one
        ; It's accounting for the edge cases, where you want to take the four 
        ; to only the right or left. 
        for i = 1, state.data.imageSize[1] - 2 do begin
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
            ; Weighing things
            totalAve = (twoMeanAve + fourMeanAve) / 2
            ; We find the change needed, and divide by the pixels that is on
            ; the planet
            ; Finally, we apply the change to ONLY the pixels on the planet. 
            change = totalAve - average[i]
            change /= upperX[i] - lowerX[i]
            for j = lowerX[i], upperX[i] do begin
                image[j,i] += change
            endfor
            print, i
            print, lowerX[i], upperX[i]
        endfor

        ; Update the image.
        (*state.data.curImage) = image

        ; Creating the new path, depending on the images has been formatted 
        ; or not. 
	    pathList = strsplit(state.data.curPath, '/', /EXTRACT)
	    path = strjoin(pathList[0:N_ELEMENTS(pathList)-3], '/')
        if pathList[N_ELEMENTS(pathList)-2] eq 'maps' then begin
            directoryType = '/UMaps/'
        endif else begin
            directoryType = '/UFormattedMaps/'
        endelse
        
        outputPath = '/' + path + directoryType + $
                     pathList[N_ELEMENTS(pathList)-1]
        writefits, outputPath, image  
        (*state.data.files)[state.data.curPathIndex] = outputPath
       
        ; Adding references to previous paths and new paths
        finalHeader = headfits(outputPath)
        fxaddpar,finalHeader,'OLDPATH', $
        (*state.data.refFiles)[state.data.curPathIndex]
        modfits, outputPath, 0, finalHeader
        
        finalHeader = headfits((*state.data.refFiles)[state.data.curPathIndex])
        fxaddpar,finalHeader,'NEWPATH', outputPath
        modfits, (*state.data.refFiles)[state.data.curPathIndex], 0, $
        finalHeader
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
; This will attempt to corrcet an image if something like there is a tilt
; or if it is mis-aligned or something
; HAVE NOT BEEN TESTED YET!!!!! I don't think it works..
; SHIT THIS DOESN"T WORK FML
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
        for i = 0.0, 720.0 do begin
            x = fix((*state.data.limbArray)[0, i] / state.data.scale)
            y = fix((*state.data.limbArray)[1, i] / state.data.scale)
            if x lt state.data.imageSize[0]-1 and y ge 0 and x ge 0 and $
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

    ; This part looks for the lowest 5 non-zero value. 
    ; SHIT THIS DOESN"T WORK FML
    minim = [max(onLimbArrayContent), max(onLimbArrayContent), $
             max(onLimbArrayContent), max(onLimbArrayContent), $
             max(onLimbArrayContent)]
        for i = 0, onLimbSize[0] - 1 do begin
            if minim[0] gt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = minim[2] 
                minim[2] = minim[1] 
                minim[1] = minim[0] 
                minim[0] = onLimbArrayContent[i] 
            endif else if minim[1] gt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = minim[2] 
                minim[2] = minim[1] 
                minim[1] = onLimbArrayContent[i] 
            endif else if minim[2] gt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = minim[2]  
                minim[2] = onLimbArrayContent[i] 
            endif else if minim[3] gt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = minim[3] 
                minim[3] = onLimbArrayContent[i] 
            endif else if minim[4] gt onLimbArrayContent[i] and $
            onLimbArrayContent[i] gt 0 then begin

                minim[4] = onLimbArrayContent[i] 
            endif
        endfor
        lowLimbAverage = mean(minim)

        ; If the limb ratio is too great, it must be on the planet. 
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
; Based on which limbs are bad, for this specific image that we are on, 
; ahift the spectra. 
;
;------------------------------------------------------------------------------
pro shiftSpectra, badlimbs, x, y, tempimg, lowLimbAverage
    common curState, state
    ; Now tempimg is the by the factors of lowLimbAverage.
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
    
    factorArr = STRARR(state.data.imagesize[1])
    finishedX = []

    for i = 0, N_ELEMENTS(badlimbs) do begin
        if badlimbs[i] eq 1 and where(finishedX eq x[i]) eq -1 then begin
            for a = 0, state.data.imagesize[1] - 1 do begin
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
            ; We find the upper and lower Y that is on the limbs
            allY = where(y eq y[i])

            bottomY = min(y[allY])
            topY = max(y[allY])
            
            ; We need to find two possible limb, but we might only get one
            ; since the image may not be complete. 
            for a = 0, state.data.imagesize[1] - 1 do begin
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
            
            ; Checks for either limbs. 
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

        ; Reset to random characters. 
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

    ; Location of where the user want to move the pixels. 
    state.data.correctImgX = 0
    state.data.correctImgY = 0

    ; Making an image 9x the original, with the original at the center 
    ; and 0 value everywhere else. 
    correctTempImg = fltarr(state.data.imageSize[0]*3, $
                            state.data.imageSize[1]*3)
    image = *state.data.curImage
    for i = 0, state.data.imageSize[0] - 1 do begin
        for j = 0, state.data.imageSize[1] - 1 do begin
            correctTempImg[i+state.data.imageSize[0], $
                           j+state.data.imageSize[1]] = image[i, j]
        endfor
    endfor
    *state.data.correctTempImg = correctTempImg

    ; Sets up pop-up widget for moving. 
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

;------------------------------------------------------------------------------
; Sets up widget for changing orientation of line of pixels selected. 
;------------------------------------------------------------------------------
pro changeOrientation, event
    common curState, state

    ; Vertical -> horizontal and horizontal -> vertical. 
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

;------------------------------------------------------------------------------
; This routine decides if to chanfe the line of selected pixels
;------------------------------------------------------------------------------
pro changeSelectionLocation, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    
    ; Move the selected area respectively
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

    ; In case of edge cases, don't let it move more. 
    if state.data.imageX lt 0 then begin
        state.data.imageX = 0
    endif 
    if state.data.imageX gt (state.data.imagesize[0]-1)*state.data.scale $
    then begin
        state.data.imageX = (state.data.imagesize[0]-1)*state.data.scale
    endif
    if state.data.imageY lt 0 then begin
        state.data.imageY = 0
    endif 
    if state.data.imageY gt (state.data.imagesize[1]-1)*state.data.scale $
    then begin
        state.data.imageY = (state.data.imagesize[1]-1)*state.data.scale
    endif
    updateSelection

end

;------------------------------------------------------------------------------
; This routine will shift one line of pixel in a direction. 
;------------------------------------------------------------------------------
pro shiftPixels, event
    common curState, state
    widget_control, event.id, GET_UVALUE = uvalue
    correctTempImg = *state.data.correctTempImg
    case uvalue of 
       'shiftRight' : begin
            for i = state.data.imagesize[0]*3-1, 1, -1 do begin
                correctTempImg[i, state.data.imageY/state.data.scale + $
                state.data.imageSize[1]] $
                = correctTempImg[i-1, state.data.imageY/state.data.scale + $
                  state.data.imageSize[1]]
            endfor
        end
       'shiftLeft' : begin
            for i = 0, state.data.imagesize[0]*3 - 2 do begin
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
            endfor
        end
       'shiftUp' : begin
            for i = state.data.imagesize[1]*3-1, 1, -1 do begin
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

;------------------------------------------------------------------------------
; Saving the new images in a different directory to not screw ourself over lol
;------------------------------------------------------------------------------
pro correctImageDone, event
    common curState, state

    ; Parsing and going up one directory, then deciding to goto formatted
    ; or UMaps. 
    pathList = strsplit(state.data.curPath, '/', /EXTRACT)
    path = strjoin(pathList[0:N_ELEMENTS(pathList)-2], '/')
    if pathList[N_ELEMENTS(pathList)-2] eq 'maps' then begin   
        directoryType = '/UMaps/'
    endif else begin
        directoryType = '/UFormattedMaps/'
    endelse
 
    outputPath = '/' + path + directoryType + pathList[N_ELEMENTS(pathList)-1]
    writefits, outputPath, totalimage  
    (*state.data.files)[i] = outputPath
    
    ; Adding references to previous paths and new paths
    finalHeader = headfits(outputPath)
    fxaddpar,finalHeader,'OLDPATH', (*state.data.refFiles)[i]
    modfits, outputPath, 0, finalHeader
        
    finalHeader = headfits(*(state.data.refFiles)[i])
    fxaddpar,finalHeader,'NEWPATH', outputPath
    modfits, *(state.data.refFiles)[i], 0, finalHeader
end

;------------------------------------------------------------------------------
; Updates the rectrangle box of the selected pixels.  
;
;------------------------------------------------------------------------------
pro updateSelection
    common curState, state

    ; Erase previous limb fit
    wset, state.widget.plotwinID & tvscl, (*state.data.scaledImage), $ 
            xsize = state.data.imagesize[0], ysize = state.data.imagesize[1]

    if state.data.orientation eq 'horizontal' then begin
        x = [0, (state.data.imagesize[0])*state.data.scale, $
             (state.data.imagesize[0])*state.data.scale, 0, 0]
        y = [state.data.imageY, state.data.imageY, $
             state.data.imageY + state.data.scale, $
             state.data.imageY + state.data.scale, $
             state.data.imageY]
    endif else begin
        y = [0, 0, (state.data.imagesize[1])*state.data.scale, $
             (state.data.imagesize[1])*state.data.scale, 0]
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
    *state.data.geometryListDivider = geometryListDivider
    print, 'Found all index dividers in geometric reference list'
    print, 'Finding all map layer files'

    ; Looks to see if the image has been formated or not; if so, 
    ; look at the correct directory. 
    pathList = strsplit(imagepath, '/', /EXTRACT)
    newpath = fxpar(curHeader, 'FORMAT')
    newpath = 0    
    path = strjoin(pathList[0:N_ELEMENTS(pathList)-3], '/')
    if TYPENAME(newpath) eq 'STRING' then begin
        openr, 2, path + '/formattedMaps/info'
        readf, 2, info
        close, 2
        path += '/formattedMaps/map*'
        state.data.scale = 3
        state.data.boolNewGeometry = 1
        state.data.axisA = info[0]
        state.data.axisB = info[1]
        state.data.centerX = info[2]
        state.data.centerY = info[3]
        drawEllipse, silent = 0

    endif else begin
        path += '/maps/map*'
    endelse
    path = '/' + path
    files = file_search(path)
    state.data.numFiles = N_ELEMENTS(files) + 1

    ; This way to append the sum.fits file to the rest of the map layers 
    temparr = STRARR(1)
    temparr[0] = imagepath
    *state.data.refFiles = [files, temparr]
    (*state.data.files) = strarr(N_ELEMENTS((*state.data.refFiles)))
    state.data.curPathIndex = N_ELEMENTS(files)
    print, 'Loaded all layer location'

    ; Check if an updated path exists
    for i = 0, state.data.numFiles - 1 do begin
        str = (*state.data.refFiles)[i]
       
        header = headfits(str)
        ;newpath = fxpar(header, 'NEWPATH')

        ; If NEWPATH doesn't exist, it would have returned 0. 
        if TYPENAME(newpath) eq 'STRING' then begin
            (*state.data.files)[i] = newpath
        endif else begin
            (*state.data.files)[i] = (*state.data.refFiles)[i]
        endelse
    endfor
    print, 'Loading data complete'
    image = readfits ((*state.data.files)[state.data.numFiles-1])
    *state.data.curImage = image
    state.data.imagesize = SIZE(image, /DIMENSIONS)

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

    ; If we are already at the edge, we should not be able to go further out
    if state.data.x lt 0 then begin
        state.data.x = 0
    endif 
    if state.data.x gt (state.data.imagesize[0]-1)*state.data.scale then begin
        state.data.x = (state.data.imagesize[0]-1)*state.data.scale
    endif
    ;if state.data.y lt 0 then begin
    ;    state.data.y = 0
    ;endif 
    ;if state.data.y gt (state.data.imagesize[1]-1)*state.data.scale then begin
    ;    state.data.y = (state.data.imagesize[1]-1)*state.data.scale
    ;endif
    
    ; Set up array to plot the selected square
    x = [state.data.x, state.data.x+state.data.scale, $
         state.data.x+state.data.scale, state.data.x, state.data.x]
    y = [state.data.y, state.data.y, state.data.y-state.data.scale, $
         state.data.y-state.data.scale, state.data.y]

    ; Draw a ellipse if an ellipse has been fitted
    if state.data.boolNewGeometry eq 1 then begin
        drawEllipse, silent = 0
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
    updateInfoBar
end

;------------------------------------------------------------------------------
; Updates the displayed lat, lon, mu, mu0
;------------------------------------------------------------------------------
pro updateInfoBar

    common curState, state
    if state.data.curPathIndex eq state.data.numFiles - 1 then begin
        curUM = 'Micron meter: sum'
    endif else begin
        spectra = *state.data.spectra 
        curUM = 'Micron meter: ' + $
                strtrim(spectra[state.data.curPathIndex, 0, 0], 2)
    endelse
    
    ; Retrieve and displaying geometric info.  
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
    ;if realY lt 0 or realY ge state.data.imagesize[1] then begin
    ;    state.data.curLat = 'NaN'
    ;    state.data.curLon = 'NaN'
    ;    state.data.curMu = 'NaN'
    ;    state.data.curMu0 = 'NaN'
    ;endif else 
    if state.data.boolNewGeometry eq 0 then begin
        ; Assigning derefenced pointer to a variable for easier readability
        latArray = *state.data.latArray
        lonArray = *state.data.lonArray
        muArray  = *state.data.muArray
        mu0Array = *state.data.mu0Array

        ; Getting the index for each starting line of pixels. 
        index = geometryListDivider[realY]
        index += realX + 1
        state.data.curLat = latArray[index]
        state.data.curLon = lonArray[index]
        state.data.curMu = muArray[index]
        state.data.curMu0 = mu0Array[index]
        help, state.data.curLat
        if finite(state.data.curLat) eq 0 then begin
            state.data.curMu = 'NaN'
            state.data.curMu0 = 'NaN'
        endif
    endif else begin
        index = geometryListDivider[realX]
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
        theta = float(state.data.limbsTheta) * !CONST.PI / 180.0

        axisA = state.data.axisA
        axisB = state.data.axisB 

        ; Transform the center coordinates
        centX = state.data.centerX*(state.data.xscale/float(axisA)*cos(theta)+$
                state.data.yscale/axisB*sin(theta))*float(axisB)/axisA
        centY = state.data.centerY*(state.data.yscale / float(axisB)*cos(theta) + $
                state.data.xscale / axisA*sin(theta)) * float(axisA)/axisA

        ; we need the center information to be in the temporary header
        fxaddpar,newHeader,'CX', centX, 'center x of planettt'
        fxaddpar,newHeader,'CY', centY, 'center y of planettt'
        fxaddpar,newHeader,'CCW', -360.000, 'center y of planettt'
        newHeader = zmo_drm_lcm(newHeader)

        realX *= state.data.scale        
        realY *= state.data.scale
 
        ; So these numbers are based on the length and width, pixel-wise,
        ; of guide images....   
        ; Credit to Kenny Duran 2017(a summer intern from caltech) for the 
        ; transformation formula
    
        ; may20, 17 -- x = 55, y = 183
        transX = realX*(state.data.xscale/ float(axisA)*cos(theta) + $
                 state.data.yscale / axisB*sin(theta))* float(axisB)/axisA
        transY = realY*(state.data.yscale/ float(axisB)*cos(theta) + $
                 state.data.xscale / axisA*sin(theta))* float(axisA)/axisA

        ; Actually get the lat/long 
        status = zmo_pf_image_latlon(newHeader, transX, transY, lat, long, $
                                     hit, error, a, b, c)

        ; If errored out of if it did not hit the planet... NaN
        if status ne 0 or hit eq 0 then begin 
            state.data.curLat = 'NaN'
            state.data.curLon = 'NaN'
            state.data.curMu = 'NaN'
            state.data.curMu0 = 'NaN'
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
;------------------------------------------------------------------------------
pro fitLimb, event
    common curState, state

    ; Creating the widget pop-up for fitting the ellipse. 
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

    ; Set up initial parameters for the ellipse. 
    if state.data.boolNewGeometry eq 0 then begin
        state.data.centerX = state.data.imagesize[0]*state.data.scale/2
        state.data.centerY = state.data.imagesize[1]*state.data.scale/2
        state.data.axisA = state.data.imagesize[0]*state.data.scale/2 
        state.data.axisB = state.data.imagesize[1]*state.data.scale/2
    endif
    state.widget.fitLimbsStep = state.data.scale
    drawEllipse, silent = 0 
end

;------------------------------------------------------------------------------
;
;
;------------------------------------------------------------------------------
pro done, event
    common curState, state
    state.data.boolNewGeometry = 1
    getscale

    print, 'state.data.saveBool'
    print, state.data.saveBool

    print, 'state.data.saveCounter'
    print, state.data.saveCounter

    print, 'fix(state.data.numFiles/100) + 1'
    print, fix(state.data.numFiles/100) + 1
    ; Saving limb information...
    if state.data.saveBool eq 1 and $
       state.data.saveCounter lt fix(state.data.numFiles/100) + 1 then begin
        print, strtrim(state.data.saveCounter+1, 2) + ' layers fitted'
        ; [xxx, 0] -> layer
        ; [xxx, 1] -> center X
        (*state.data.saveData)[state.data.saveCounter, 0] = state.data.curPathIndex
        (*state.data.saveData)[state.data.saveCounter, 1] = state.data.centerX
        (*state.data.saveData)[state.data.saveCounter, 2] = state.data.centerY
        (*state.data.saveData)[state.data.saveCounter, 3] = state.data.axisA
        (*state.data.saveData)[state.data.saveCounter, 4] = state.data.axisB
        print, (*state.data.saveData)
        state.data.saveCounter += 1
    endif

    ; If all fitted limbdata array is populated... lol this isn't a bool lol. 
    if state.data.saveCounter eq fix(state.data.numFiles/100) + 1 then begin
        state.data.saveBool = 2
    endif

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
; This is called everytime the center is moved for the ellipse. 
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
print, 'state.data.centerX, state.data.centerY'
print, state.data.centerX, state.data.centerY
    drawEllipse, silent = 0
end

;------------------------------------------------------------------------------
; This is called every time the axis changes in value
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
print, 'state.data.axisA, state.data.axisB'
print, state.data.axisA, state.data.axisB
    state.data.boolChangeEllipse = 1
    drawEllipse, silent = 0
end

;------------------------------------------------------------------------------
; Changes the theta of the fitted ellipse. 
;------------------------------------------------------------------------------
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
    drawEllipse, silent = 0
end

;------------------------------------------------------------------------------
; Every time the image scale or the center changes, drawEllipse will be called
; It will also be called after fitting limbs is complete, so you can see the 
; limbs while selecting pixel for lat/lot. 
; If silent is set to 0 aka false, it will plot the ellipse. 
;------------------------------------------------------------------------------
pro drawEllipse, silent = silent
    common curState, state
    ; Erase previous limb fit
    wset, state.widget.plotwinID & tvscl, (*state.data.scaledImage), $ 
            xsize = state.data.imagesize[0], ysize = state.data.imagesize[1]
    ; Only calculate new limbArray if a change, such as axis change or
    ; center change is made
    if state.data.boolChangeEllipse eq 1 then begin
        limbArray = FLTARR(2, 721)    
        theta = state.data.limbsTheta * !CONST.PI/180
        for i = 0, 720.0 do begin
            radians = i/2 * !CONST.PI/ 180
            limbArray[0, i] = state.data.axisA * cos(radians)*cos(theta)-$
                              state.data.axisB * sin(radians)*sin(theta) $
                              + state.data.centerX
            limbArray[1, i] = state.data.axisA * cos(radians)*sin(theta)+$
                              state.data.axisB * sin(radians)*cos(theta) $
                              + state.data.centerY
        endfor
        *state.data.limbArray = limbArray
        state.data.boolChangeEllipse = 0
    endif else begin
        limbArray = *state.data.limbArray
    endelse
    
    ; If silent is 0 aka false, we should plot the ellipse. 
    if silent ne 1 then begin

        ; Create the center cross to denote center of planet
        verticalLineX = [state.data.centerX-12, state.data.centerX + 12]
        verticalLineY = [state.data.centerY, state.data.centerY]
        horizontalLineY = [state.data.centerY-12, state.data.centerY + 12]
        horizontalLineX = [state.data.centerX, state.data.centerX]

        ; Plots the ellipse and then the center cross
        plots,limbArray[0, *],limbArray[1, *], /device, color='0000FF'x
        plots, verticalLineX, verticalLineY, /device, color='0000FF'x
        plots, horizontalLineX, horizontalLineY, /device, color='0000FF'x
    endif

end

pro getscale
    common curState, state

    if state.data.boolGetScale ne 0 then begin
        goto, GOTTENSCALE
    endif
    state.data.boolGetScale = 1    
    limbArray = *state.data.limbArray
    ;print, 'limbsss'
    ;print, limbArray[0,*]
    ;print, 'break'
    ;print, limbArray[1,*]
    geometryListDivider = *state.data.geometryListDivider
    latArray = *state.data.latArray

    xlocIndex = where(limbArray[0, *] eq state.data.centerX) 
    ylocIndex = where(limbArray[0, *] eq state.data.centerY)
    theta = state.data.limbsTheta * !CONST.PI / 180

    yyloc = limbArray[1, xlocIndex]
    xxloc = limbArray[0, ylocIndex]
    xyloc = limbArray[1, ylocIndex]
    yxloc = limbArray[0, xlocIndex]

    
    xx = 99999
    yy = 99999
    if N_ELEMENTS(xxloc) ge 1 then begin
        if xxloc[0] le state.data.imagesize[0]*state.data.scale $
           and xxloc[0] gt 0 then begin
            xx = xxloc[0]
            yx = xyloc[0]
        endif else if N_ELEMENTS(xxloc) ge 2 then begin
                if xxloc[1] le state.data.imagesize[0]*state.data.scale  and $
                xxloc[1] gt 0 then begin
                    xx = xxloc[1]
                    yx = xyloc[1]
                endif
        endif
    endif
    print, 'xx, yx'
    print, xx, yx
    if xx ne 99999 then begin
        index = geometryListDivider[xx/state.data.scale]
        spectraNum = fix(latArray[index] - 10000)

        spectraPath = state.data.spectraPath + 'spectra*' + $
                          STRTRIM(spectraNum,2) +'*'
        spectraHeader = headfits(spectraPath)

        header = zmo_drm_ephemeris(spectraHeader,cancelevent, auto=auto, $
                                      specfun=specfun, ephem_header=ephem_header, $
                                      ephem_data=ephem_data)
        for i = 0, 1500 do begin
            newHeader = header
            axisA = state.data.axisA
            axisB = state.data.axisB 
            centX = (state.data.centerX*(i/ float(axisA)*cos(theta) + $
                    183.0 / axisB*sin(theta)) * float(axisB)/axisA)
            centY = state.data.centerY*(183.0 / float(axisB)*cos(theta) + $
                    i/ axisA*sin(theta)) * float(axisA)/axisA


            fxaddpar,newHeader,'CX', centX, 'center x of planettt'
            fxaddpar,newHeader,'CY', centY, 'center y of planettt'
            fxaddpar,newHeader,'CCW', -360.000, 'center y of planettt'
            newHeader = zmo_drm_lcm(newHeader)

            transX = (xx*(i/ float(axisA)*cos(theta) + $
                     183.0 / axisB*sin(theta)) * float(axisB)/axisA)
            transY = (yx*(183.0 / axisB*cos(theta) + $
                     i / axisA*sin(theta)) * axisA/axisA)
            status = zmo_pf_image_latlon(newHeader, transX, transY, lat, long, $
                                     hit, error, a, b, c)
            if status ne 0 or hit eq 0 then begin 
                state.data.xscale = i-1
                break
            endif
        endfor 
    endif


    if N_ELEMENTS(yyloc) gt 1 then begin
        yy = yyloc[0]
        xy = yxloc[0]
    endif

    if yy ne 99999 then begin
        index = geometryListDivider[xy/state.data.scale]
        spectraNum = fix(latArray[index] - 10000)

        spectraPath = state.data.spectraPath + 'spectra*' + $
                          STRTRIM(spectraNum,2) +'*'
        spectraHeader = headfits(spectraPath)

        header = zmo_drm_ephemeris(spectraHeader,cancelevent, auto=auto, $
                                      specfun=specfun, ephem_header=ephem_header, $
                                      ephem_data=ephem_data)
        for i = 0, 1500 do begin
            newHeader = header
            axisA = state.data.axisA
            axisB = state.data.axisB 
            centX = (state.data.centerX*(state.data.xscale/ float(axisA)*cos(theta) + $
                    i / axisB*sin(theta)) * axisB/axisA)
            centY = state.data.centerY*(i / float(axisB)*cos(theta) + $
                    state.data.xscale/ axisA*sin(theta)) * float(axisA)/axisA

            fxaddpar,newHeader,'CX', centX, 'center x of planettt'
            fxaddpar,newHeader,'CY', centY, 'center y of planettt'
            fxaddpar,newHeader,'CCW', -360.000, 'center y of planettt'
            newHeader = zmo_drm_lcm(newHeader)

            transX = (xy*(state.data.xscale/ float(axisA)*cos(theta) + $
                     i / axisB*sin(theta)) * axisB/axisA)
            transY = (float(yy)*(i / float(axisB)*cos(theta) + $
                     float(state.data.xscale) / axisA*sin(theta)) * float(axisA)/axisA)
            status = zmo_pf_image_latlon(newHeader, transX, transY, lat, long, $
                                     hit, error, a, b, c)
            if status ne 0 or hit eq 0 then begin 
                state.data.yscale = i-1
                break
            endif
        endfor 
    endif
print, 'x, y factors'
print, state.data.xscale, state.data.yscale
GOTTENSCALE: print, 'Exiting get scale routine...'
end


;------------------------------------------------------------------------------
; As the name implies (ಠ⌣ಠ) Gotta clean up after yourself
;------------------------------------------------------------------------------
pro viewLayer_cleanup, base
    common curState, state

    ; FREEING ALL THIS GOD DAMN POINTERS
    ptr_free, state.data.correctTempImg
    ptr_free, state.data.curHeader
    ptr_free, state.data.curImage
    ptr_free, state.data.files
    ptr_free, state.data.geometryListDivider
    ptr_free, state.data.latArray
    ptr_free, state.data.lonArray
    ptr_free, state.data.limbArray
    ptr_free, state.data.muArray
    ptr_free, state.data.mu0Array
    ptr_free, state.data.refFiles
    ptr_free, state.data.saveData
    ptr_free, state.data.scaledImage
    ptr_free, state.data.spectra
    state = 0B
end

;------------------------------------------------------------------------------
; Does nothing except resizing. Idk why we actually need this but I see it
; so I just added this lol. 
;------------------------------------------------------------------------------
pro resize, event
    common curState, state
end


