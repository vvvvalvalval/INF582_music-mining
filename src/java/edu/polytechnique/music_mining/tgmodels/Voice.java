package edu.polytechnique.music_mining.tgmodels;

import org.herac.tuxguitar.song.factory.TGFactory;
import org.herac.tuxguitar.song.models.TGVoice;

public class Voice extends TGVoice {

	public Voice(TGFactory factory, int index) {
		super(factory, index);
	}

	@Override
	public String toString() {
		return "Voice [duration=" + getDuration() + ", notes="
				+ getNotes() + "]";
	}

	

}
