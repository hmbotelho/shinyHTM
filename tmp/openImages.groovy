import ij.IJ;
import ij.ImagePlus;
import ij.gui.OvalRoi;
IJ.open("D:/Basel_demo/384/EXTDRG_valid2_plate1_batch1_20/W0349--CITED2--s20281/P004--CITED2--s20281/EXTDRG_valid2_plate1_batch1_20--CITED2--s20281--W0349--P004--T0000--C01.ome.tif");
ImagePlus imp1 = IJ.getImage(); 
IJ.open("D:/Basel_demo/384/EXTDRG_valid2_plate1_batch1_20/W0349--CITED2--s20281/P004--CITED2--s20281/EXTDRG_valid2_plate1_batch1_20--CITED2--s20281--W0349--P004--T0000--C00.ome.tif");
ImagePlus imp2 = IJ.getImage(); 
IJ.run(imp1, "Merge Channels...", "c1="+imp1.getTitle()+" c2="+imp2.getTitle()+" create");
IJ.wait( 100 );

