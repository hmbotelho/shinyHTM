import ij.IJ;
import ij.gui.OvalRoi
IJ.run("Image Sequence...", "open=[/Users/tischer/Documents/sylwia/data] file=(MDA_10--W00085--P00001--Z(?<slice>\\d+)--T(?<timePoint>\\d+)--(?<channel>.+)\\.tif) sort")
IJ.getImage().setSlice(10)
IJ.getImage().setRoi( new OvalRoi(255.393,1021.129,50,50) )

