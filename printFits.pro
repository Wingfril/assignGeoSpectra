pro printfits

img = readfits('/home/mziyan/TestData/17May31.1/maps/map001.fits')

imgSize = size(img, /DIMENSIONS)
imgStr = STRARR(imgSize[0])
minim = max(img)

        ; Find the smallest number within the image that is not 0
        for i = 0, imgSize[0] - 1 do begin
            for j = 0, imgSize[1] - 1 do begin
                if img[i, j] lt minim and img[i, j] ne 0 then begin
                    minim = img[i, j]                
                endif
            endfor
        endfor

img /= minim

for i = 0, imgSize[0] - 1 do begin
    imgStr[i] = imgStr[i] + '||'
    for j = 0, imgSize[1] - 1 do begin
        imgStr[i] = imgStr[i] + STRING(img[i,j])
    endfor
        imgStr[i] = imgStr[i] + '||'
    print, i
endfor
openw, 1, '/home/mziyan/TestData/17May31.1/map001array' ,/APPEND
printf, 1, imgStr
close, 1

end
