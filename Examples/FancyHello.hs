-- Fancy version of the "Hello, world!" program
import Fudgets

main = fudlogue (shellF "Hello" helloF)

helloF = labelF (fancyTextD greeting)

fancyTextD text =
    stackD [bgD,
	    spacedD (hvMarginS (5+dist) 5) shadowTxtD,
	    spacedD (hvMarginS 5 (5+dist)) fgTxtD]
  where
    shadowTxtD = fgD shadow txtD
    fgTxtD = fgD color txtD
    txtD = spacedD centerS $ fontD font (g text)
    bgD = vboxD' 0 [fgD sky fillD,fgD ground fillD]
    fillD = g (filler False False 10)

-- Everything can be changed with command line switches:
color = argKey "color" "red"
shadow = argKey "shadow" "black"
sky = argKey "sky" "skyblue"
ground = argKey "ground" "blue"
greeting = argKey "greeting" "Hello, world!"
dist = diag (argReadKey "dist" 2)
font = argKey "font" "-*-helvetica-bold-r-*-*-24-*-*-*-*-*-iso8859-1"
