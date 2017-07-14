pro matchwavelength

files = file_search('/home/mziyan/TestData/09aug23/proc/'+'spectra00*.fits')
f1 = readfits(files[0])
f5 = readfits(files[2])
f9 = readfits(files[4])
f13 = readfits(files[6])
f17 = readfits(files[8])
f21 = readfits(files[10])
f23 = readfits(files[11])


for i = 0, 564 do begin
        if f1[i, 0, 1] ne f5[i, 0, 1] then begin
            print, 'f1 and f5'
            print, i
            print, f1[i, 0, 1]
            print, f5[i, 0, 1]
        endif
        if f1[i, 0, 1] ne f9[i, 0, 1] then begin
            print, 'f1 and f9'
            print, i
            print, f1[i, 0, 1]
            print, f9[i, 0, 1]
        endif
        if f1[i, 0, 1] ne f13[i, 0, 1] then begin
            print, 'f1 and f13'
            print, i
            print, f1[i, 0, 1]
            print, f13[i, 0, 1]
        endif
        if f1[i, 0, 1] ne f17[i, 0, 1] then begin
            print, 'f1 and f17'
            print, i
            print, f1[i, 0, 1]
            print, f17[i, 0, 1]
        endif
        if f1[i, 0, 1] ne f21[i, 0, 1] then begin
            print, 'f1 and f21'
            print, i
            print, f1[i, 0, 1]
            print, f21[i, 0, 1]
        endif
        if f1[i, 0, 1] ne f23[i, 0, 1] then begin
            print, 'f1 and f23'
            print, i
            print, f1[i, 0, 1]
            print, f23[i, 0, 1]
        endif
endfor    
end
