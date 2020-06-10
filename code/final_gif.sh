# Create a gif from all .png images
convert -resize 768x576 -delay 100 -loop 0 ../images/*.png ../images/all_plots.gif 
