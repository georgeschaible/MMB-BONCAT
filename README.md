# MMB-BONCAT
Repository for analysis of BONCAT fluorescent probing in multicellular magnetotactic bacteria.

![image](https://github.com/georgeschaible/MMB-BONCAT/assets/54877012/f97dfbd9-5fdb-4148-9673-8bef56f9e0cf)

The code in this repository is writen to compliment analysis of fluorescent images with EMAN2 (https://blake.bcm.edu/emanwiki/EMAN2). EMAN2 is a scientific image processing suite that can be used to process grayscale images and is ammendable to costum Python code. The software was used to select particles (e.g. individual MMB from confocal microscopy) and process them so find the center of each particle and threshold them. The Python code was used to calculate the pixel distance from the center to the edge of each particle. Next, R was used to calculate the ratio of each radius and relative fluorescence intensity (RFI) so a average of all particles could be plotted. 
