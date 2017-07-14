pro findgoodwave

path = '/home/mziyan/TestData/17May20/cal/wavecal410-410.fits'
wavecal = mrdfits(path, 1)

siz = size(wavecal, /DIMENSIONS)
print, siz
for i = 0, siz[0] - 1 do begin
    for j = 0, siz[0] - 1 do begin
        if FINITE(wavecal[i, j], /Nan) then begin
        endif else begin
            print, 'i,j'
            print, i
            print, j
        endelse
    endfor
endfor
end
