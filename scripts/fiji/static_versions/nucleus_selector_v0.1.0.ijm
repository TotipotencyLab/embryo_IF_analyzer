dna_mask_ch=2

// Making sure the current working environment is clean
run("Select None");
if (roiManager("count")>0) {
	roiManager("Deselect");
	roiManager("Delete");
}

// // # Nucleoli ------------------------------------------------------------
// run("Duplicate...", "duplicate channels="+dna_mask_ch);
// // run("Duplicate...", "duplicate");
// 
// tmpNucleoli=getTitle();
// print(tmpNucleoli);
// run("Invert", "stack");
// // Process > Filters > Gaussian Blur
// run("Gaussian Blur...", "sigma=5 stack");
// // Image > Adjust > Auto Threshold
// run("Auto Threshold", "method=Huang2 ignore_black ignore_white white stack");
// run("Close-", "stack");
// //run("Dilate", "stack");
// run("Analyze Particles...", "size=3-300 circularity=0.50-1.00 exclude add stack");
// 
// // run("Select All");
// run("Select None");

// # Nucleus --------------------------------------------------------------------
run("Duplicate...", "duplicate channels="+dna_mask_ch);

// Bluring the nucleus
//run("Gaussian Blur...", "sigma=3 stack");
//run("Gaussian Blur...", "sigma=5 stack");
//run("Gaussian Blur...", "sigma=7 stack");
run("Gaussian Blur...", "sigma=10 stack");

run("Auto Threshold", "method=Huang2 ignore_black ignore_white white stack use_stack_histogram");
//run("Close-", "stack");
			
// Process > Binary > Fill Holes
run("Fill Holes", "stack");
setOption("BlackBackground", true);
//run("Erode", "stack");
run("Analyze Particles...", "size=40-Infinity exclude include add stack");


