import ij.IJ;
import ij.gui.OvalRoi
IJ.run("Image Sequence...", "open=[/Users/tischer/Documents/sylwia/data] file=(MCF_10--W00013--P00001--Z(?<slice>\\d+)--T(?<timePoint>\\d+)--(?<channel>.+)\\.tif) sort")
IJ.getImage().setSlice(10)
IJ.getImage().setRoi( new OvalRoi(236.587,924.109,50,50) )

