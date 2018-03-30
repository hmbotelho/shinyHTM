import ij.IJ;
import ij.ImagePlus
import ij.gui.OvalRoi
IJ.run("Image Sequence...", "open=[/Users/tischer/Documents/sylwia/data] file=(MCF_10--W00013--P00001--Z(?<slice>\\d+)--T(?<timePoint>\\d+)--(?<channel>.+)\\.tif) sort")
ImagePlus imp1 = IJ.getImage() 
IJ.open("/Users/tischer/Documents/sylwia/data/MCF_10--W00013--P00001--Z--T--foreground.tif")
ImagePlus imp2 = IJ.getImage() 
IJ.run(imp1, "Merge Channels...", "c1="+imp1.getTitle()+" c2="+imp2.getTitle()+" create ignore");

