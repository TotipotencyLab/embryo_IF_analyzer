// last update: 2026-09-14
//
// CHANGELOG
//  - FIX: measure_roi() measured channels 1..N instead of the channels listed
//         in `channel_to_measure`. The array contents are now honoured.
//  - FIX: outline y-coordinates were scaled by pixelWidth instead of pixelHeight.
//  - FIX: the "Position" token lookup crashed when the slice label had no match.
//  - Set Measurements is now issued explicitly (it is a persistent Fiji user
//    preference, so output columns were machine-dependent).
//  - save_outline_coord() now takes an explicit `save_coord` flag. The Table.save
//    call was previously commented out, so despite its name the function only
//    ever wrote the ROI zip. Default is false, preserving the old behaviour.

outdir="/Users/chad/Lab/0_imaging/SR002_Nr5a2_F1/measurements/"
out_basename="SR006_"

// ## Image information --------------------------------------------------------
// Token used to locate the imaging position inside the slice label.
// Set to "" to always fall back to the image window title instead.
position_pattern = "Position";

dna_mask_ch=2;
channel_to_measure=newArray(1,2,3);

// Write the outline coordinate table to disk as well as the ROI zip.
save_outline_to_file = false;

// ## Measurements --------------------------------------------------------------
// Fiji's Set Measurements is a persistent USER PREFERENCE, not a per-macro
// setting, so relying on it makes the output columns machine-dependent and
// silently breaks the downstream R scripts. Setting it explicitly here makes
// the output schema deterministic. Reproduces the expected column set:
//   Label Area Mean StdDev Min Max X Y Circ. IntDen Median RawIntDen
//   Ch Slice AR Round Solidity
// NB: this overwrites the operator's Fiji preference, and it persists.
measurement_fields  = "area mean standard min centroid shape integrated median stack display";
measurement_decimal = 3;

close_outline_window = false;
close_outline_table_window = true;
reset_results_window = false;

// # Set_basename
// NB: must come after position_pattern is defined.
cur_window = getTitle();
out_basename = out_basename + resolve_image_id(position_pattern);


// # ===========================================================================
// # Analysis Section ==========================================================

// # Set measurement
// Force the measurement set so output columns do not depend on the operator's
// Fiji preferences. See `measurement_fields` above.
run("Set Measurements...", measurement_fields + " redirect=None decimal=" + measurement_decimal);


// # Fetching some constant information
getPixelSize(unit, pixelWidth, pixelHeight);

clear_roi();
roiManager("Add");

save_outline_coord("manual_select_", outdir, out_basename, close_outline_window, save_outline_to_file);
measure_roi(cur_window, channel_to_measure, outdir, out_basename, reset_results_window, 0, roiManager("count"));

clear_roi();
Stack.setChannel(dna_mask_ch);

// # Local function ===============================================================

// Naming ------------------------------------------------------------------------
// NB: duplicated verbatim from nucleus_selector.ijm -- the IJ1 macro language has
//     no import mechanism. This is the duplication the Groovy port removes.

function resolve_image_id(pattern){
	// Return an identifier for the current image, for use in output filenames.
	// Prefers a token matching `pattern` inside the slice label; falls back to
	// the image window title when the label carries no match (or is empty).
	label = getInfo("slice.label");
	if( (pattern != "") && (label != "") ){
		parts = split(label, "\\/");
		matched = Array.filter(parts, pattern);
		if(lengthOf(matched) > 0){
			// NB: assign before returning. The IJ1 macro interpreter cannot infer
			//     a string return type through a nested user-function call, so
			//     "return sanitize_name(...)" fails with "Numeric return value
			//     expected". Do not collapse these two lines.
			id = sanitize_name(matched[0]);
			return id;
		}
	}
	print("No '" + pattern + "' token in slice label -- falling back to image title.");
	id = sanitize_name(getTitle());
	return id;
}

function sanitize_name(s){
	// Drop a trailing image extension and replace characters that are unsafe
	// or awkward inside a filename.
	s = replace(s, "\\.(tif|tiff|lif|lifext|czi|nd2)$", "");
	s = replace(s, "[\\/\\\\:\\*\\?\"<>\\|]", "_");
	s = replace(s, "^\\s+", "");
	s = replace(s, "\\s+$", "");
	return s;
}


function clear_roi() {
	//run("Select None");
	if (roiManager("count")>0) {
		roiManager("Deselect");
		roiManager("Delete");
	}
}


function save_outline_coord(dm_name, outdir, outfile_basename, close_popup_window, save_coord) {
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
				Table.set("y", cnt, ypoints[noc]*pixelHeight, dm);
				cnt++;
			}
			
		} // end of for loop
		
		// # Save to file
		Table.update(dm);
		if(save_coord){
			Table.save(outdir+outfile_basename+"_"+dm_name+".txt");
		}
		
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


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res, min_idx, max_idx) {
	selectWindow(window_name);
	//channel_to_measure=newArray(1,2,3);
	num_channel=channel_to_measure.length;
	
	n = roiManager("count");
	if(max_idx > min_idx) {
		// # i.e., there is at least one valid ROI in the requested range
		
		for ( ch=0; ch<num_channel; ch++ ) {
			// NB: use the channel NUMBERS supplied by the caller (previously
			//     setChannel(ch+1), which ignored the requested channels).
			Stack.setChannel(channel_to_measure[ch]);
			
			for ( i=min_idx; i<max_idx; i++ ) { 
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
