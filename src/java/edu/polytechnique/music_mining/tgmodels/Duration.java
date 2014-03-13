package edu.polytechnique.music_mining.tgmodels;

import org.herac.tuxguitar.song.factory.TGFactory;
import org.herac.tuxguitar.song.models.TGDuration;

public class Duration extends TGDuration {

	public Duration(TGFactory factory) {
		super(factory);
	}

	@Override
	public String toString() {
		return "Duration [value=" + getValue() + "]";
	}

	
}
