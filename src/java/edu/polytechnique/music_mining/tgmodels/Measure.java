package edu.polytechnique.music_mining.tgmodels;

import java.util.List;

import org.herac.tuxguitar.song.models.TGBeat;
import org.herac.tuxguitar.song.models.TGMeasure;
import org.herac.tuxguitar.song.models.TGMeasureHeader;

public class Measure extends TGMeasure {

	public Measure(TGMeasureHeader header) {
		super(header);
	}
	
	@Override
	public String toString() {
		return "Measure [beats="
				+ getBeats() + "]";
	}

	
}
