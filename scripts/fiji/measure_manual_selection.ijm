// version 0.1.0
// last update: 2024-02-28

outdir="/Users/chad/Lab/0_imaging/SR002_Nr5a2_F1/measurements/"
out_basename="SR006_"

// # Set_basename
cur_window = getTitle();
cur_name = getInfo("slice.label");
cur_name = split(cur_name, "\\/");
use_name = Array.filter(cur_name, "Position");
//Array.print(out_basename);
out_basename=out_basename + use_name[0];

// ## Image information --------------------------------------------------------
dna_mask_ch=2;
channel_to_measure=newArray(1,2,3);

close_outline_window = false;
close_outline_table_window = true;
reset_results_window = false;


// # ===========================================================================
// # Analysis Section ==========================================================

// # Set measurement
//if(run_fit_ellipse){
//	run("Set Measurements...", "area min area_fraction limit display redirect=None decimal=3");
//}else{
//	run("Set Measurements...", "area centroid perimeter fit shape feret's redirect=None decimal=2");
//}


// # Fetching some constant information
getPixelSize(unit, pixelWidth, pixelHeight);

clear_roi();
roiManager("Add");

save_outline_coord("manual_select_", outdir, out_basename, close_outline_window);
measure_roi(cur_window, channel_to_measure, outdir, out_basename, reset_results_window);

clear_roi();
Stack.setChannel(dna_mask_ch);

// # Local function ===============================================================

function clear_roi() {
	//run("Select None");
	if (roiManager("count")>0) {
		roiManager("Deselect");
		roiManager("Delete");
	}
}


function save_outline_coord(dm_name, outdir, outfile_basename, close_popup_window) {
	dm=dm_name;
	// # A placeholder for our outline table
	if(isOpen(dm)){
		cnt=Table.size(dm);
//		cnt=cnt-1;
		print(cnt);
		// use the same table
	}else{
		cnt=0;
		Table.create(dm);
	}
	
	
	// # This should be unnecessary 	
	//getPixelSize(unit, pixelWidth, pixelHeight);
	
	n = roiManager("count");
	if(n>0) {
		// # save all currently available ROI object
		roiManager("Save", outdir + outfile_basename + "_" + dm_name + "_ROIs.zip");
		
		// # Saving the outline into a text file
		for ( i=0; i<n; i++ ) { 
			roiManager("select", i); // select one ROI
			
			// # fetch information of the current ROI
			Stack.getPosition(channel, curr, frame);
			roi_name=Roi.getName;
			Roi.getCoordinates(xpoints, ypoints);
			
			// # add info to a table
			for (noc=0; noc<xpoints.length; noc++){
				Table.set("idx", cnt, cnt, dm);
				Table.set("name", cnt, outfile_basename, dm);
				Table.set("roi", cnt, roi_name, dm);
				Table.set("z", cnt, curr, dm);
				Table.set("x", cnt, xpoints[noc]*pixelWidth, dm);
				Table.set("y", cnt, ypoints[noc]*pixelWidth, dm);
				cnt++;
			}
			
		} // end of for loop
		
		// # Save to file
		Table.update(dm);
		//Table.save(outdir+outfile_basename+"_"+dm_name+".txt");	
		
		if(close_popup_window){
			cur_img=getTitle();
			//Table.reset(dm);
			selectWindow(dm);
			run("Close");
			selectWindow(cur_img);
		}
		
		
	}else{
		print("No ROI Found!");
	} // end of if n>0
}


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res) {
	selectWindow(window_name);
	//channel_to_measure=newArray(1,2,3);
	num_channel=channel_to_measure.length;
	
	n = roiManager("count");
	if(n>0) {
		// # i.e., there is at least one valid ROI
		
		for ( ch=0; ch<num_channel; ch++ ) {
			Stack.setChannel(ch+1)
			
			for ( i=0; i<n; i++ ) { 
				roiManager("select", i); // select one ROI
				roiManager("measure");
			}
		}
		
		// save measurement
		selectWindow("Results");
		saveAs("txt", outdir + outfile_basename + "_res.txt");
		
		if(reset_res) {
			Table.reset("Results");
			selectWindow("Results");
			run("Close");
			selectWindow(window_name);
		}
		
	}else{
		print("No ROI found!");
	}
	
}
