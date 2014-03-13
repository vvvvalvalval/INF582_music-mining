package edu.polytechnique.music_mining.tgmodels;

import java.util.Collections;
import java.util.List;

import org.herac.tuxguitar.song.factory.TGFactory;
import org.herac.tuxguitar.song.models.TGTrack;

public class Track extends TGTrack {

	public Track(TGFactory factory) {
		super(factory);
	}

	public List<TGTrack> measures(){
		return Util.listFromIterator(this.getMeasures());
	}
	
	@Override
	public String toString() {
		return "Track [number=" + getNumber() + ", measures="
				+ measures() + ", strings=" + getStrings()
				+ ", name=" + getName() + "]";
	}

	
}
