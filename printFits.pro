pro printfits

img = readfits('/home/mziyan/TestData/17May20/maps/sum.fits')

imgSize = size(img, /DIMENSIONS)
imgStr = STRARR(imgSize[0])
for i = 0, imgSize[0] - 1 do begin
    imgStr[i] = imgStr[i] + '||'
    for j = 0, imgSize[1] - 1 do begin
        imgStr[i] = imgStr[i] + STRING(img[i,j])
    endfor
        imgStr[i] = imgStr[i] + '||'
    print, i
endfor
openw, 1, '/home/mziyan/TestData/17May20/finaldatasum' ,/APPEND
printf, 1, imgStr
close, 1

end
