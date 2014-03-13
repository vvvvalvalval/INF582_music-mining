package edu.polytechnique.music_mining.tgmodels;

import java.util.ArrayList;
import java.util.List;

import org.herac.tuxguitar.song.factory.TGFactory;
import org.herac.tuxguitar.song.models.TGBeat;
import org.herac.tuxguitar.song.models.TGVoice;

public class Beat extends TGBeat {

	private List<TGVoice> voices(){
		List<TGVoice> res = new ArrayList<TGVoice>();
		for(int i = 0; i < this.countVoices(); i++){
			res.add(this.getVoice(i));
		}
		return res;
	}
	
	public Beat(TGFactory factory) {
		super(factory);
	}

	@Override
	public String toString() {
		return "Beat [voices=" + voices() + "]";
	}
	
	

}
